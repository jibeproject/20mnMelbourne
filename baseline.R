# investigate baseline NAC destinations coverage for 20 minute neighbourhood intervention

#------------------------------------------------------------------------------#
# Process ----
# •	Find residential addresses (section 2)
#   o	Filter addresses to study region (Greater Melbourne GCCSA plus 10km buffer), 
#     and to residential meshblocks.
#   o	Add an ‘id’ field, for later linking to distances outputs.
#   o	Identify nearest network node to each residential address (‘residential nodes’).
#   o	Result saved as output/residential_addresses.sqlite.
# •	Make catchment for each NAC (section 3.1 and functions/makeNacCatchments.R).  
#   o	Load Neighbourhood Activity Centre (NAC) polygons and supermarkets
#   o	Catchment distance is 400m (small NAC) or 800m (medium or large NAC)  
#   o	Select anchor nodes, which are nearest nodes to supermarkets in the NAC if 
#     any, or else NAC centroid.
#   o	Select links within that euclidean catchment distance of the anchor nodes 
#     (only places that are within the Euclidean distance can be within the network distance).  
#   o	Densify those links with new nodes every 5m, creating a subnetwork.  
#   o	Calculate network distances (using the subnetwork) from anchor nodes to 
#     all other nodes, and select nodes reachable within the 400m or 800m buffer 
#     distance as applicable.  
#   o	Load residential addresses, find the nearest subnetwork node to each, and 
#     filter to the addresses where the nearest subnetwork node is a node 
#     reachable within the buffer distance.  Save as ‘nac catchment addresses’.
#   o	Draw a polygon around the nac catchment addresses: Voronoi polygons of the
#     addresses within the Euclidean buffer area, intersect with the underlying 
#     addresses, filter to those which are catchment addresses, and dissolve.  
#     Intersect with convex hull of the addresses to avoid extending too far into 
#     unpopulated areas. Save as ‘nac catchment polygon’.
#   o	Combine the outputs from all NACs into output//nac_catchment_addresses.rds 
#     (a dataframe of lists of the address id’s for each NAC) and 
#     output/nac_catchment_polygons.sqlite (a dataframe of the polygons for the NACs).
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
#   all NACs, and large, medium and small NACs (section 5.1 and 
#   functions/calculateCoverage.R).  Output saved as output/20mn baseline area coverage.csv.
# •	Find percentage of residences in each NAC with access to each destination 
#   type within specified walking distance (section 5.2 and functions/calculateNacCoverage.R).  
#   Output saved as output /20mn baseline NAC coverage.csv.
# •	Build table showing summary of number and percentage of all, large, medium 
#   and small NACs with 80% of residences with access to each destination type 
#   within specified distance (section 5.3 and functions/calculateNacCoverageSummary.R).  
#   Output saved as output /20mn baseline NAC coverage summary.csv.

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
library(doSNOW)
library(parallel)
library(foreach)

library(ggplot2)  # testing only


## 1.2 Functions ----
## ------------------------------------#
dir_walk(path="./functions/",source, recurse=T, type = "file")


## 1.3 Parameters ----
## ------------------------------------#
PROJECT.CRS <- 28355
BUFFDIST.SMALL <- 400  # distance to buffer small NACs
BUFFDIST.MED.LARGE <- 800  # distance to buffer medium and large NACs
DENSIFICATION.DIST <- 5  # distance to densify links for finding NAC catchments


## 1.4 Data ----
## ------------------------------------#
# neighbourhood activity centres
NACs <- read_zipped_GIS(zipfile = "../data/original/MICLUP-NACs.zip",
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

# network
# # load 'sqlite' network, and filter to region buffer  
# links <- st_read("../data/processed/network.sqlite", layer = "links") %>%
#   st_filter(region_buffer, .predicate = st_intersects) %>%
#   # filter to walkable only
#   filter(is_walk == 1) %>%
#   # correct required fields to integer
#   mutate(from_id = as.integer(from_id),
#          to_id = as.integer(to_id),
#          id = as.integer(id))
# nodes <- st_read("../data/processed/network.sqlite", layer = "nodes") %>%
#   # only those used in links
#   filter(id %in% links$from_id | id %in% links$to_id) %>%
#   # correct required fields to integer
#   mutate(id = as.integer(id))


# load 'gpkg' network, and filter to region buffer
links <- st_read("../data/processed/edgesMelbourne.gpkg") %>%
  st_filter(region_buffer, .predicate = st_intersects) %>%
  # filter to walkable only
  filter(is_walk == TRUE) %>%
  # tidy names to those expected by functions
  rename(from_id = from, to_id = to, id = edgeID, GEOMETRY = geom)

nodes <- st_read("../data/processed/nodesMelbourne.gpkg") %>%
  # only those used in links
  filter(nodeID %in% links$from_id | nodeID %in% links$to_id) %>%
  # tidy names to those expected by functions
  rename(id = nodeID, GEOMETRY = geom) %>%
  mutate(x = st_coordinates(GEOMETRY)[,1], y = st_coordinates(GEOMETRY)[,2])

# keep just the largest connected network
network <- largestConnectedComponent(nodes, links)
network.nodes <- network[[1]]
network.links <- network[[2]]

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
temp_osm_2023.location <- "../data/processed/temp_osm_2023.sqlite"
community.centre.location <- "../data/processed/community_centre.sqlite"
community.health.location <- "../data/processed/community_health.sqlite"

# residential addresses: set to F if using existing, or create in section 2
find.residential.addresses <- F
residential.address.location <- "./output/residential_addresses.sqlite"

# NAC catchments: set to F if using existing, or create in section 3
make.NAC.catchments <- F
nac.catchment.address.location <- "./output/nac_catchment_addresses.rds"
nac.catchment.polygon.location <- "./output/nac_catchment_polygons.sqlite"

# baseline address destination distances: set to F if using existing, or create in section 4
find.baseline.distances <- F
baseline.distance.location <- "./output/baseline_distances.csv"
baseline.distsport.location <- "./output/baseline_distsport_display.sqlite"

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
  
  # # region boundary
  # region_buffer <- st_read(region_buffer.location)
  
  # find addresses located in study area residential meshblocks
  residential.addresses <- addresses %>%
    # filter to region
    st_filter(region_buffer, .predicate = st_intersects) %>%
    # intersect with addresses and filter to residential
    st_intersection(meshblocks) %>%
    filter(MB_CAT16 == "Residential") %>%
    # add id
    mutate(id = row_number()) %>%
    dplyr::select(id)
  
  # write output
  st_write(residential.addresses, residential.address.location, 
           delete_layer = TRUE)
  
}


# 3 NAC catchments ----
# -----------------------------------------------------------------------------#
# create NAC catchments
if (make.NAC.catchments) {
  
  # load inputs
  supermarkets <- st_read(POIs.location) %>% filter(Attribute == "supermarket")
  residential.addresses <- st_read(residential.address.location)
  
  # omit 14 'undeveloped' NACs (can't select buffers for them)
  NACs.filtered <- NACs %>%
    filter(CENTRESIZE != "Undeveloped")
  
  # CONSIDER FURTHER whether to omit any or all Fishermans Bend NACs
  # (1119, 1125, 1126, 1127, 1128, 1129, 1130, 1131 - all except 1119
  # have CENTRESIZE < 2000)
  
  # create temporary catchments folders to hold outputs from makeNacCatchments
  temp.address.location <- "./catchment addresses"
  temp.polygon.location <- "./catchment polygons"
  dir.create(temp.address.location)
  dir.create(temp.polygon.location)
  
  # run the function - saves files to the catchments folders
  makeNacCatchments(NACs.filtered,
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
  saveRDS(catchment.outputs[[1]], nac.catchment.address.location)
  st_write(catchment.outputs[[2]], nac.catchment.polygon.location, 
           delete_layer = TRUE)
  
  
  # remove the catchments folders
  unlink(temp.address.location, recursive = TRUE)
  unlink(temp.polygon.location, recursive = TRUE)

}



# 4 Distances between addresses and existing destinations ----
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
                                                    temp_osm_2023.location,
                                                    community.centre.location,
                                                    community.health.location,
                                                    region_buffer)
  
  # find the distances
  baseline.distances <- addressDestinationDistances(baseline.destinations,
                                                    residential.addresses,
                                                    network.nodes,
                                                    network.links, 
                                                    PROJECT.CRS)
  
  # save output
  write.csv(baseline.distances, baseline.distance.location, row.names = FALSE)
  
  # save district sport output for display (find the index of 'district_sport' 
  # in destination.types, then it's that index plus 1 in the baseline destinations list)
  st_write(baseline.destinations[[which(destination.types == "district_sport") + 1]],
           baseline.distsport.location, delete_layer = TRUE)
  
}


# 5 Assess baseline status ----
# -----------------------------------------------------------------------------#

## 5.1 Overall status ----
## ------------------------------------#

# calculate overall area coverage as percentage of residences with access to 
# each destination type within specified distance for Greater Melbourne, 
# all NACs, and large, medium and small NACs

# load inputs
residential.addresses <- st_read(residential.address.location)
baseline.distances <- read.csv(baseline.distance.location)
nac.catchment.addresses <- readRDS(nac.catchment.address.location)
region <- st_read(region.location)

# calculate coverage
baseline.coverage <- calculateCoverage(residential.addresses,
                                       baseline.distances, 
                                       nac.catchment.addresses, 
                                       NACs,
                                       region)

# write output
write.csv(baseline.coverage, "./output/20mn baseline area coverage.csv",
          row.names = FALSE)


## 5.2 NACs ----
## ------------------------------------#

# make table showing % of residences in each NAC within specified walking
# distance of destinations

# load inputs
baseline.distances <- read.csv(baseline.distance.location)
nac.catchment.addresses <- readRDS(nac.catchment.address.location)

# calculate coverage
baseline.NAC.coverage <- calculateNacCoverage(baseline.distances,
                                              nac.catchments.addresses,
                                              NACs)

# write output
write.csv(baseline.NAC.coverage, "./output/20mn baseline NAC coverage.csv",
          row.names = FALSE)


## 5.3 NAC summary ----
## ------------------------------------#

# summary table of NACs with 80% of residences with access to each destination
# type within specified walking distance

# load NAC.pct.coverage from section 5.2
baseline.NAC.coverage <- read.csv("./output/20mn baseline NAC coverage.csv")

# calculate summary
baseline.NAC.coverage.summary <- 
  calculateNacCoverageSummary(baseline.NAC.coverage)

# write output
write.csv(baseline.NAC.coverage.summary, 
          "./output/20mn baseline NAC coverage summary.csv",
          row.names = FALSE)

