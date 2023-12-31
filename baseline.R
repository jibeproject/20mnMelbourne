# add destinations to NACs for 20 minute neighbourhood intervention

#------------------------------------------------------------------------------#
# Process ----
# 1 

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

# library(tibble)  # for 'repair_names' in addressDestinationDestances - trial improvement

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
baseline.distport.location <- "./output/baseline_distsport_display.sqlite"

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
  # input files 'POIs', 'ANLS.pos' and 'ANLS.dest'
  POIs <- st_read(POIs.location)
  
  ANLS.pos <- st_read(ANLS.pos.location, layer = "public_open_space_osm_2018") %>%
    st_transform(PROJECT.CRS)
  
  ANLS.dest <- st_read(ANLS.dest.location, layer = "study_destinations") %>%
    st_transform(PROJECT.CRS)
  
  community.centre <- st_read(community.centre.location)
  
  community.health <- st_read(community.health.location)
  
  baseline.destinations <- loadBaselineDestinations(POIs, ANLS.pos, ANLS.dest,
                                                    temp_osm_2023.location,
                                                    community.centre,
                                                    community.health,
                                                    region_buffer)
  
  # find the distances
  baseline.distances <- addressDestinationDistances(baseline.destinations,
                                                    residential.addresses,
                                                    network.nodes,
                                                    network.links, 
                                                    PROJECT.CRS)
  
  # save output
  write.csv(baseline.distances, baseline.distance.location, row.names = FALSE)
  
  # save district sport output for display
  st_write(baseline.destinations[[17]], ## check that district sport is 17!
           baseline.distport.location, delete_layer = TRUE)
  
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

