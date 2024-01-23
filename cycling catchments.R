# file to make cycling distance catchments around ACs

# note - when run in Jan 2024, the network was wrongly marking some trunk
# roads as uncyclable; some outputs saved with 'MISSING_TRUNK' added

# draws heavily on section 3 of 'baseline'

# libraries, functions and parameters ----
# -------------------------------------#
library(dplyr)
library(fs)
library(sf)
library(igraph)
library(doSNOW)
library(parallel)
library(foreach)
library(ggplot2)  # testing only

dir_walk(path="./functions/",source, recurse=T, type = "file")

POIs.location <- "../data/processed/Weighted POIs/poi.gpkg"
residential.address.location <- "./output/residential_addresses.sqlite"

PROJECT.CRS <- 28355
BUFFDIST.SMALL <- 1200  # distance to buffer small NACs
BUFFDIST.MED.LARGE <- 1200  # distance to buffer medium and large NACs (currently same as small)
DENSIFICATION.DIST <- 5  # distance to densify links for finding NAC catchments

cycling.catchment.address.location <- "./output/cycling_catchment_addresses.rds"
cycling.catchment.polygon.location <- "./output/cycling_catchment_polygons.sqlite"


# load inputs ----
# -------------------------------------#
supermarkets <- st_read(POIs.location) %>% filter(Attribute == "supermarket")
residential.addresses <- st_read(residential.address.location)

ACs <- read_zipped_GIS(zipfile = "../data/original/MICLUP-NACs.zip",
                       file = "/MICLUP_COMMERCIAL_EXT_JUN2020.shp") %>%
  st_transform(PROJECT.CRS) %>%
  mutate(size = case_when(
    CENTRESIZE %in% c("Less than 2000", "2000 to 5000") ~ "small",
    CENTRESIZE == "5000 to 10000"                       ~ "medium",
    TRUE                                                ~ "large")
  ) %>%
  dplyr::select(-Shape_Leng, -Shape_Area)

region_buffer <- st_read("../data/processed/region_buffer.sqlite")

# load 'gpkg' network, and filter to region buffer
links <- st_read("../data/processed/edgesMelbourne.gpkg") %>%
  st_filter(region_buffer, .predicate = st_intersects) %>%
  # filter to cyclable only  # compare baseline - filtered to walking - NOTE AS AT 24-JAN-24 EXCLUDES SOME TRUNK ROADS
  filter(is_cycle == TRUE) %>%
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


# find cycling catchments ----
# -------------------------------------#
# omit 14 'undeveloped' NACs (can't select buffers for them)
ACs.filtered <- ACs %>%
  filter(CENTRESIZE != "Undeveloped")

# create temporary catchments folders to hold outputs from makeNacCatchments
temp.address.location <- "./catchment addresses"
temp.polygon.location <- "./catchment polygons"
dir.create(temp.address.location)
dir.create(temp.polygon.location)

# run the function - saves files to the catchments folders
makeNacCatchments(ACs.filtered,
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
saveRDS(catchment.outputs[[1]], cycling.catchment.address.location)
st_write(catchment.outputs[[2]], cycling.catchment.polygon.location, 
         delete_layer = TRUE)


# remove the catchments folders
unlink(temp.address.location, recursive = TRUE)
unlink(temp.polygon.location, recursive = TRUE)


# find the edges for upgrade ----
# -------------------------------------#
catchments <- st_read(cycling.catchment.polygon.location) 

unsafe.edges <- network.links %>%
  mutate(already_safe = ifelse(
    cycleway %in% c("bikepath", "separated_lane") | freespeed <= 40 / 3.6,
    1, 0
  )) %>%
  filter(already_safe == 0)

# catchment subgroup(s)
catchments.2000.plus <- catchments %>%
  left_join(ACs %>% st_drop_geometry(), by = c("centre_no" = "CENTRE_NO")) %>%
  filter(CENTRESIZE != "Less than 2000")

# select target roads
target.all <- unsafe.edges %>%
  st_filter(catchments, .predicate = st_intersects)
target.2000.plus <- unsafe.edges %>%
  st_filter(catchments.2000.plus, .predicate = st_intersects)

st_write(target.all, "./output/target_cycling_edges.sqlite", 
         layer = "catchments_all", delete_layer = TRUE)
st_write(target.2000.plus, "./output/target_cycling_edges.sqlite", 
         layer = "catchments_2000_plus", delete_layer = TRUE)
