# investigate baseline AC destinations coverage for 20 minute neighbourhood intervention

#------------------------------------------------------------------------------#
# Process ----
# •	Find residential addresses (section 2)
#   o	Filter addresses to study region (Greater Melbourne GCCSA plus 10km buffer), 
#     and to residential meshblocks.
#   o	Add an ‘id’ field, for later linking to distances outputs.
#   o	Identify nearest network node to each residential address (‘residential nodes’).
#   o	Result saved as output/residential_addresses.sqlite.
# •	Make catchment for each AC (section 3.1 and functions/makeAcCatchments.R).  
#   o	Load  Activity Centre (AC) polygons and supermarkets
#   o	Catchment distance is 400m (small AC) or 800m (medium or large AC)  
#   o	Select anchor nodes, which are nearest nodes to supermarkets in the AC if 
#     any, or else AC centroid.
#   o	Select links within that euclidean catchment distance of the anchor nodes 
#     (only places that are within the Euclidean distance can be within the network distance).  
#   o	Densify those links with new nodes every 5m, creating a subnetwork.  
#   o	Calculate network distances (using the subnetwork) from anchor nodes to 
#     all other nodes, and select nodes reachable within the 400m or 800m buffer 
#     distance as applicable.  
#   o	Load residential addresses, find the nearest subnetwork node to each, and 
#     filter to the addresses where the nearest subnetwork node is a node 
#     reachable within the buffer distance.  Save as ‘ac catchment addresses’.
#   o	Draw a polygon around the AC catchment addresses: Voronoi polygons of the
#     addresses within the Euclidean buffer area, intersect with the underlying 
#     addresses, filter to those which are catchment addresses, and dissolve.  
#     Intersect with convex hull of the addresses to avoid extending too far into 
#     unpopulated areas. Save as ‘ac catchment polygon’.
#   o	Combine the outputs from all ACs into output/ac_catchment_addresses.rds 
#     (a dataframe of lists of the address id’s for each AC) and 
#     output/ac_catchment_polygons.sqlite (a dataframe of the polygons for the ACs).
# •	Find distances between addresses and destinations (section 4 and functions/addressDestinationDistances.R).
#   o	Load baseline destinations
#   o	For each destination type, find its nearest node (for polygons, these are 
#     the nearest nodes to pseudo entry points at 20m intervals along the boundary 
#     within 30m of a road, as well as nodes within the polygon itself)
#   o	Find the distance from each residential address to the nearest destination of each type:
#     	Find the nearest node to the destination (for polygons, being local parks 
#       and district sports, nearest nodes to pseudo entry points at 20m intervals 
#       along the boundary within 30m of a road, as well as nodes within the polygon itself)
#     	Measure distance from destinations to addresses (doing it this way because 
#       there are fewer destinations than addresses).
#     	Where there are more than 1000 addresses, break them into groups of 1000 
#       (because otherwise the destination matrix becomes impossibly large).  
#       Assign each group a ‘dest group name’
#     	For each destination group, get the distances to all the residential address 
#       nodes – hold the distance matrix in memory if up to 4 groups, or save to a 
#       temporary folder if more (because otherwise the memory can overflow with 
#       large numbers).  Use parallel processing if over 4 groups.
#     	Find the minimum distance to a destination feature for each address.  
#       If more than 1 group of 1000, then find the minimum for each group of 1000, 
#       and then find the minimum of minimums. 
#     	Save minimum distance for each destination to a temporary file, where it 
#       can then be joined to residential addresses.
#   o	Output saved as output/baseline_distances.csv
# •	Build table showing overall area coverage as percentage of residences with 
#   access to each destination type within specified distance for Greater Melbourne, 
#   all ACs, and large, medium and small ACs (section 5.1 and 
#   functions/calculateCoverage.R).  Output saved as output/20mn baseline area coverage.csv.
# •	Find percentage of residences in each AC with access to each destination 
#   type within specified walking distance (section 5.2 and functions/calculateAcCoverage.R).  
#   Output saved as output /20mn baseline AC coverage.csv.
# •	Build table showing summary of number and percentage of all, large, medium 
#   and small ACs with 80% of residences with access to each destination type 
#   within specified distance (section 5.3 and functions/calculateAcCoverageSummary.R).  
#   Output saved as output /20mn baseline AC coverage summary.csv.
# • Make boxplots investigating small ACs:
#   o	allocates a score, with 1 point for each of the 14 destination tests
#   o	makes boxplot for the score for each of large, medium, small (2-5k) and 
#     small (<2k), showing the score
#   o	divides small according to whether under or over 50% overlap with 
#     medium/large, and makes a second similar boxplot for them 
#   Outputs saved as images/baseline_score.png and images/overlap_score.png

#------------------------------------------------------------------------------#


# 1 Setup ----
#------------------------------------------------------------------------------#
## 1.1 Libraries ----
## ------------------------------------#
library(dplyr)
library(fs)
library(sf)
library(igraph)
library(lwgeom)  # used in densifySubNetwork
library(ggplot2)
library(stringr)
library(doSNOW)
library(parallel)
library(foreach)

options(scipen = 999)


## 1.2 Functions ----
## ------------------------------------#
dir_walk(path = "./functions/", source, recurse = T, type = "file")


## 1.3 Parameters ----
## ------------------------------------#
PROJECT.CRS <- 28355
BUFFDIST.SMALL <- 400  # distance to buffer small ACs
BUFFDIST.MED.LARGE <- 800  # distance to buffer medium and large ACs
DENSIFICATION.DIST <- 5  # distance to densify links for finding AC catchments


## 1.4 Data ----
## ------------------------------------#
#  activity centres
ACs <- read_zipped_GIS(zipfile = "../data/original/MICLUP-NACs.zip",
    file = "/MICLUP_COMMERCIAL_EXT_JUN2020.shp") %>%
  st_transform(PROJECT.CRS) %>%
  mutate(size = case_when(
    CENTRESIZE %in% c("Less than 2000", "2000 to 5000") ~ "small",
    CENTRESIZE == "5000 to 10000"                       ~ "medium",
    TRUE                                                ~ "large")
  ) %>%
  dplyr::select(-Shape_Leng, -Shape_Area)

# region buffer
region_buffer <- st_read("../data/processed/region_buffer.sqlite")

# load network, and filter to region buffer
links <- st_read("../data/processed/edgesMelbourne.gpkg") %>%
  st_filter(region_buffer, .predicate = st_intersects) %>%
  # tidy names to those expected by functions
  rename(from_id = from, to_id = to, id = edgeID, GEOMETRY = geom)

nodes <- st_read("../data/processed/nodesMelbourne.gpkg") %>%
  # tidy names to those expected by functions
  rename(id = nodeID, GEOMETRY = geom) %>%
  mutate(x = st_coordinates(GEOMETRY)[,1], y = st_coordinates(GEOMETRY)[,2])

links.walk <- links %>% filter(is_walk == TRUE)
nodes.walk <- nodes %>% filter(id %in% links.walk$from_id | id %in% links.walk$to_id)

links.cycle <- links %>% filter(is_cycle == TRUE)
nodes.cycle <- nodes %>% filter(id %in% links.cycle$from_id | id %in% links.cycle$to_id)

# keep just the largest connected networks ('network' is walking; 'network.cycle' is cycling)
network <- largestConnectedComponent(nodes.walk, links.walk)
network.nodes <- network[[1]]
network.links <- network[[2]]

network.cycle <- largestConnectedComponent(nodes.cycle, links.cycle)
network.nodes.cycle <- network.cycle[[1]]
network.links.cycle <- network.cycle[[2]]


# address and region data locations
address.location <- "../data/original/VIC_ADDRESS_DEFAULT_GEOCODE_psv.psv"
meshblock.location <- "../data/original/1270055001_mb_2016_vic_shape.zip"
region.location <- "../data/processed/region.sqlite"

# baseline points of interest location
POIs.location <- "../data/processed/Weighted POIs/poi.gpkg"
ANLS.pos.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"
ANLS.dest.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"

# residential addresses: set to F if using existing, or create in section 2
find.residential.addresses <- F
residential.address.location <- "./output/residential_addresses.sqlite"

# AC catchments: set to F if using existing, or create in section 3
make.AC.catchments <- F
ac.catchment.address.location <- "./output/ac_catchment_addresses.rds"
ac.catchment.polygon.location <- "./output/ac_catchment_polygons.sqlite"

# baseline address destination distances: set to F if using existing, or create in section 4
find.baseline.distances <- F
baseline.node.distance.location <- "./output/node_distances_baseline.csv"

# directory for outputs
if (!dir.exists("./output")) {
  dir.create("./output")
}


# 2 Residential addresses ----
# -----------------------------------------------------------------------------#
# find residential addresses
if (find.residential.addresses) {
  # addresses
  addresses<- read.table(address.location,
                         sep = "|", header = TRUE) %>%
    # crs is GDA 94 (see section 5.1 of 'G-NAF Product Description.pdf' within
    # 'nov18_gnaf_pipeseparatedvalue_20181119200719.zip')
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(4283)) %>%
    st_transform(PROJECT.CRS)
  
  # meshblocks
  meshblocks <- 
    read_zipped_GIS(zipfile = meshblock.location) %>%
    st_transform(PROJECT.CRS)
  
  # find addresses located in study area residential meshblocks
  residential.addresses <- addresses %>%
    # filter to region
    st_filter(region_buffer, .predicate = st_intersects) %>%
    # intersect with addresses and filter to residential
    st_intersection(meshblocks) %>%
    filter(MB_CAT16 == "Residential") %>%
    # add id
    mutate(id = row_number()) %>%
    dplyr::select(id) %>%
    # add nearest network node (note: this is walking network)
    mutate(address.n.node = network.nodes$id[st_nearest_feature(., network.nodes)])
  
  # add nearest cycling nodes
  residential.addresses <- residential.addresses %>%
    mutate(cycle.node = network.nodes.cycle$id[st_nearest_feature(., network.nodes.cycle)])
  
  # write output
  st_write(residential.addresses, residential.address.location, 
           delete_layer = TRUE)
  
}


# 3 AC catchments ----
# -----------------------------------------------------------------------------#
# create AC catchments
if (make.AC.catchments) {
  
  # load inputs
  supermarkets <- st_read(POIs.location) %>% filter(Attribute == "supermarket")
  residential.addresses <- st_read(residential.address.location)
  
  # omit 14 'undeveloped' ACs (can't select buffers for them)
  ACs.filtered <- ACs %>%
    filter(CENTRESIZE != "Undeveloped")
  
  # create temporary catchments folders to hold outputs from makeAcCatchments
  temp.address.location <- "./catchment addresses"
  temp.polygon.location <- "./catchment polygons"
  dir.create(temp.address.location)
  dir.create(temp.polygon.location)
  
  # run the function - saves files to the catchments folders
  makeAcCatchments(ACs.filtered,
                   supermarkets,
                   network.nodes,
                   network.links,
                   residential.addresses,
                   BUFFDIST.SMALL,
                   BUFFDIST.MED.LARGE,
                   DENSIFICATION.DIST, 
                   temp.address.location,
                   temp.polygon.location)
  
  # assemble outputs
  catchment.outputs <- assembleCatchmentOutputs(temp.address.location,
                                                temp.polygon.location)
  
  
  # save the outputs
  saveRDS(catchment.outputs[[1]], ac.catchment.address.location)
  st_write(catchment.outputs[[2]], ac.catchment.polygon.location, 
           delete_layer = TRUE)
  
  
  # remove the catchments folders
  unlink(temp.address.location, recursive = TRUE)
  unlink(temp.polygon.location, recursive = TRUE)

}



# 4 Distances between address nodes and existing destinations ----
# -----------------------------------------------------------------------------#
# find baseline address destination distances
if (find.baseline.distances) {
  
  # load residential addresses
  residential.addresses <- st_read(residential.address.location)
  
  # load baseline destinations - a list containing (1) a vector of 'destination 
  # types', and (2) a dataframe (sf object) for each destination type, based on 
  # input files 'POIs', 'ANLS.pos', 'ANLS.dest' etc
  baseline.destinations <- loadBaselineDestinations(POIs.location, 
                                                    ANLS.dest.location,
                                                    ANLS.pos.location,
                                                    region_buffer,
                                                    PROJECT.CRS)
  
  # find the distances
  baseline.node.distances <- 
    addressDestinationDistances(baseline.destinations,
                                residential.addresses,
                                network.nodes,
                                network.links, 
                                PROJECT.CRS,
                                multiple.destinations = list(c("restaurant_cafe", 4)),
                                mode = "walk")
  
  # save output
  write.csv(baseline.node.distances, baseline.node.distance.location, row.names = FALSE)
  
}


# 5 Assess baseline status ----
# -----------------------------------------------------------------------------#

## 5.1 Overall status ----
## ------------------------------------#

# calculate overall area coverage as percentage of residences with access to 
# each destination type within specified distance for Greater Melbourne, 
# all ACs, and large, medium and small ACs

# load inputs
residential.addresses <- st_read(residential.address.location)
baseline.distances <- residential.addresses  %>%
  st_drop_geometry() %>%
  # join distances
  left_join(read.csv(baseline.node.distance.location), 
            by = c("address.n.node" = "node_id")) %>%
  # remove any columns for second-most-distant, etc
  dplyr::select(-matches("[0-9]$"))
ac.catchment.addresses <- readRDS(ac.catchment.address.location)
region <- st_read(region.location)

# calculate coverage
baseline.coverage <- calculateCoverage(residential.addresses,
                                       baseline.distances, 
                                       ac.catchment.addresses, 
                                       ACs,
                                       region)

# write output
write.csv(baseline.coverage, "./output/20mn baseline area coverage.csv")


## 5.2 ACs ----
## ------------------------------------#

# make table showing % of residences in each AC within specified walking
# distance of destinations

# load inputs
residential.addresses <- st_read(residential.address.location)
baseline.distances <- residential.addresses  %>%
  st_drop_geometry() %>%
  # join distances
  left_join(read.csv(baseline.node.distance.location), 
            by = c("address.n.node" = "node_id")) %>%
  # remove any columns for second-most-distant, etc
  dplyr::select(-matches("[0-9]$"))
ac.catchment.addresses <- readRDS(ac.catchment.address.location)

# calculate coverage
baseline.AC.coverage <- calculateAcCoverage(baseline.distances,
                                              ac.catchments.addresses,
                                              ACs)

# write output
write.csv(baseline.AC.coverage, "./output/20mn baseline AC coverage.csv",
          row.names = FALSE)


## 5.3 AC summary ----
## ------------------------------------#

# summary table of ACs with 80% of residences with access to each destination
# type within specified walking distance

# load AC.pct.coverage from section 5.2
baseline.AC.coverage <- read.csv("./output/20mn baseline AC coverage.csv")

# calculate summary
baseline.AC.coverage.summary <- 
  calculateAcCoverageSummary(baseline.AC.coverage)

# write output
write.csv(baseline.AC.coverage.summary, 
          "./output/20mn baseline AC coverage summary.csv")


# 6 Small AC investigation ----
# -----------------------------------------------------------------------------#

## 6.1 Load required data ----
## -------------------------------------#
# baseline performance against destination tests, by AC
baseline.AC.coverage <- read.csv("./output/20mn baseline AC coverage.csv")

# AC catchments
ac.catchments <- readRDS(ac.catchment.address.location)


## 6.2 Allocate score ----
## -------------------------------------#
baseline.score <- baseline.AC.coverage %>%
  
  # add score, out of 14 (one for each test)
  mutate(score = as.integer(supermarket.800 >= 80) +
           as.integer(pharmacy.800 >= 80) +
           as.integer(pharmacy.800 >= 80) +
           as.integer(post.800 >= 80) +
           as.integer(gp.800 >= 80) +
           as.integer(mat.child.health.800 >= 80) +
           as.integer(dentist.800 >= 80) +
           as.integer(childcare.800 >= 80) +
           as.integer(kindergarten.800 >= 80) +
           as.integer(primary.800 >= 80) +
           as.integer(comm.library.800 >= 80) +
           as.integer(convenience.400 >= 80) +
           as.integer(rest.cafe.400 >= 80) +
           as.integer(park.400 >= 80) +
           as.integer(bus.400.tram.600.train.800 >= 80)
  ) %>%
  
  # divide small category into two
  left_join(ACs %>% dplyr::select(centre_no = CENTRE_NO, CENTRESIZE), by = "centre_no") %>%
  mutate(category = as.factor(case_when(
    size == "small" & CENTRESIZE == "2000 to 5000"   ~ "small 2-5k",
    size == "small" & CENTRESIZE == "Less than 2000" ~ "small <2k",
    TRUE ~ size
  ))) %>%
  # reorder the levels of the 'score' factor
  mutate(category = factor(category, 
                           levels = c("small <2k", "small 2-5k", "medium", "large")))

## 6.3 Boxplot for score ----
## -------------------------------------#
# calculate counts for each category
category.counts <- baseline.score %>%
  count(category)

# create the boxplot and add counts
score.plot <- ggplot(data = baseline.score, aes(x = category, y = score)) +
  # geom_jitter(position = position_jitter(width = 0.1), alpha = 0.2, colour = "blue") + 
  geom_boxplot() +
  labs(title = "Distribution of scores by Activity Centre size",
       x = "Activity Centre size (with number of centres)",
       y = "Score: no of destination targets met (max 14)") +
  scale_x_discrete(labels = paste0(category.counts$category, " (", category.counts$n, ")")) +
  theme_classic()

# save
ggsave("./images/baseline_score.png", score.plot, width = 15, height = 12, units = "cm")

# means
means <- baseline.score %>%
  group_by(category) %>%
  summarise(mean = mean(score))
means
# category    mean
# <fct>      <dbl>
# 1 small <2k   4.64
# 2 small 2-5k  6.35
# 3 medium      5.70
# 4 large       6.41


## 6.4 Allocate overlap ----
## -------------------------------------#
# join ac.catchments to details from baseline scores
catchments <- ac.catchments %>%
  rename(centre_no = CENTRE_NO) %>%
  left_join(baseline.score %>% 
              st_drop_geometry() %>%
              dplyr::select(centre_no, size, category, score), 
            by = "centre_no")

# address id's for medium and large ACs
medium.large <- c()
for (i in 1:nrow(catchments)) {
  if (catchments$size[i] %in% c("medium", "large")) {
    medium.large <- c(medium.large, unlist(catchments$address_ids[i]))
  }
}
medium.large <- medium.large %>% unique() %>% sort()

# overlap percentage for small, based on number of address id's that are also in medium.large
small.acs <- catchments %>%
  filter(size == "small")

for (i in 1:nrow(small.acs)) {
  ac.addresses <- unlist(small.acs$address_ids[i])
  overlap.addresses <- ac.addresses[(ac.addresses %in% medium.large)]
  small.acs$overlap.pct[i] <- length(overlap.addresses) / length(ac.addresses) * 100
}

# add overlap categories and groups
small.acs.with.groups <- small.acs %>%
  mutate(overlap.category = case_when(overlap.pct >= 50 ~ "large",
                                      TRUE    ~ "small"),
         group = paste0(category, ", ", overlap.category, " overlap"))


## 6.5 Boxplot for overlap score ----
## -------------------------------------#
# calculate counts for each category
group.counts <- small.acs.with.groups %>%
  count(group)

# create the boxplot and add counts
overlap.score.plot <- ggplot(data = small.acs.with.groups, aes(x = group, y = score)) +
  # geom_jitter(position = position_jitter(width = 0.1), alpha = 0.2, colour = "blue") + 
  geom_boxplot() +
  labs(title = "Distribution of scores by small Activity Centre overlap group",
       x = "Activity Centre overlap category (with number of centres)",
       y = "Score: no of destination targets met (max 14)",
       caption = "'Large overlap' means 50% or more of the dwellings in the small AC are also in a\n large or medium AC; small overlap means less than 50%.") +
  scale_x_discrete(labels = function(x) str_wrap(paste0(x, " (", group.counts$n, ")"), width = 15)) +
  # scale_x_discrete(labels = paste0(group.counts$group, " (", group.counts$n, ")")) +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0))  #  left alignment


# save
ggsave("./images/overlap_score.png", overlap.score.plot, width = 15, height = 12, units = "cm")


# means
means.overlap <- small.acs.with.groups %>%
  group_by(group) %>%
  summarise(mean = mean(score))
means.overlap
# group                      mean
# <chr>                     <dbl>
# 1 small <2k, large overlap   6.52
# 2 small <2k, small overlap   4.33
# 3 small 2-5k, large overlap  7.11
# 4 small 2-5k, small overlap  6.21