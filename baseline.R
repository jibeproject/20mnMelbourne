# investigate baseline AC destinations coverage for 20 minute neighbourhood intervention

#------------------------------------------------------------------------------#
# Process ----
# •	Find residential addresses (section 2)
#   o	Filter GNAF addresses to study region (Greater Melbourne GCCSA plus 10km 
#     buffer) and join meshblocks.
#   o Exclude addresses in meshblocks with census count of <= 5 dwellings or
#     <= 10 persons.
#   o Add weights for each address: meshblock dwelling or person count divided by
#     number of address points in meshblock.
#   o Identify nearest node to each address on walking and cycling networks.
#   o	Add an ‘id’ field, for later linking to distances outputs.
#   o	Result saved as output/residential_addresses.sqlite.
#
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
#
# •	Make catchment for each AC using an alternative 'boundary' approach
#   where anchors are not used, and instead the catchment is all address points
#   within the required walking distance of the boundary of the AC (section 3.2 
#   and functions/makeAcCatchments.R).  
#
# •	Find distances between addresses and destinations (section 4 and 
#   functions/addressDestinationDistances.R).
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
#
# • Build output tables, saved as output/baseline assessment.xlsx:
#   o 'area coverage pop' (section 5.1 and functions/calculateCoverage.R): 
#     overall area coverage as percentage of population with access to each
#     destination type within specified distance for Greater Melbourne, all
#     all ACs, and large, medium and small ACs
#   o 'area coverage dwel' (section 5.1 and functions/calculateCoverage.R): 
#     same, but as percentage of dwellings with access rather than population
#
#   o 'AC coverage pop' (section 5.2 and functions/calculateAcCoverage.R): 
#     percentage of population in each AC with access to each destination 
#     type within specified walking distance
#   o 'AC coverage dwel' (section 5.2 and functions/calculateAcCoverage.R): 
#     same, but as percentage of dwellings with access rather than population
#   o 'AC coverage boundary' (section 5.4 and functions/calculateAcCoverage.R): 
#     same, but as percentage of population with access based on AC catchments
#     measured from AC boundaries rather than AC anchors
#
#   o 'AC coverage summ pop' (section 5.3 and functions/calculateAcCoverageSummary.R): 
#     summary of number and percentage of all, large, medium and small ACs with 
#     80% of population with access to each destination type within specified distance
#   o 'AC coverage summ dwel' (section 5.3 and functions/calculateAcCoverageSummary.R): 
#     same, but as number and percentage of dwellings rather than population
#   o 'AC coverage summ boundary' (section 5.5 and functions/calculateAcCoverageSummary.R): 
#     same, but as number and percentage of population with access based on AC catchments
#     measured from AC boundaries rather than AC anchors
#
#   o 'AC shortfall comp' (section 5.6): comparison table of the numbers
#     of ACs failing to meet the 80% target from each of the 'pop', 'dwel' and 
#     'boundary' summary tables
#   o 'AC shortfall comp grp' (section 5.7 and functions/calculateLgaGroupShortfalls):
#     breakdown of the percentages of ACs failing to meet the 80% target for each
#     of the 'pop' and 'boundary' methods, by inner/middle/outer LGA groups

# • Make boxplots investigating small ACs (for each of population and dwelling counts):
#   o	allocates a score, with 1 point for each of the 14 destination tests
#   o	makes boxplot for the score for each of large, medium, small (2-5k) and 
#     small (<2k), showing the score
#   o	divides small according to whether under or over 50% overlap with 
#     medium/large, and makes a second similar boxplot for them 
#   Outputs saved as images/baseline_score_pop.png, images/baseline_score_dwel.png,
#   images/overlap_score_pop.png and images/overlap_score_dwel.png

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
library(openxlsx)

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
links <- st_read("../data/processed/melbourneClipped_edges.sqlite") %>%
  st_filter(region_buffer, .predicate = st_intersects)

nodes <- st_read("../data/processed/melbourneClipped_nodes.sqlite")

links.walk <- links %>% filter(is_walk == TRUE)
nodes.walk <- nodes %>% filter(id %in% links.walk$from_id | id %in% links.walk$to_id)

links.cycle <- links %>% filter(is_cycle == TRUE)
nodes.cycle <- nodes %>% filter(id %in% links.cycle$from_id | id %in% links.cycle$to_id)

# keep just the largest connected networks
network.walk <- largestConnectedComponent(nodes.walk, links.walk)
network.nodes.walk <- network.walk[[1]]
network.links.walk <- network.walk[[2]]

network.cycle <- largestConnectedComponent(nodes.cycle, links.cycle)
network.nodes.cycle <- network.cycle[[1]]
network.links.cycle <- network.cycle[[2]]

# remove intermediate components (memory issues)
rm(links, nodes, links.walk, nodes.walk, links.cycle, nodes.cycle,
   network.walk, network.cycle)

# address, meshblock and region data locations
# note - using 2016 meshblock census counts; could be updated to a later
# census as part of a future general baseline data update 
address.location <- "../data/original/VIC_ADDRESS_DEFAULT_GEOCODE_psv.psv"
meshblock.location <- "../data/original/1270055001_mb_2016_vic_shape.zip"
meshblock.count.location <- "../data/original/2016 census mesh block counts.csv"
region.location <- "../data/processed/region.sqlite"

# baseline points of interest location
POIs.location <- "../data/processed/Destinations weights/Baseline/poi_weight.gpkg"
ANLS.pos.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"
ANLS.dest.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"

# LGA location
LGA.zipfile <- "../data/original/LGAs.zip"
LGA.subpath <- "/mga94_55/esrishape/whole_of_dataset/victoria/VMADMIN"

# residential addresses: set to F if using existing, or create in section 2
find.residential.addresses <- F
residential.address.location <- "./output/residential_addresses.sqlite"

# AC catchments: set to F if using existing, or create in section 3
make.AC.catchments <- F
ac.catchment.address.location <- "./output/ac_catchment_addresses.rds"
ac.catchment.polygon.location <- "./output/ac_catchment_polygons.sqlite"
make.AC.catchments.boundary <- T
ac.catchment.address.location.boundary <- "./output/ac_catchment_addresses_boundary.rds"
ac.catchment.polygon.location.boundary <- "./output/ac_catchment_polygons_boundary.sqlite"

# baseline address destination distances: set to F if using existing, or create in section 4
find.baseline.distances <- F
baseline.node.distance.location <- "./output/node_distances_baseline_walk.csv"

# baseline output summary
baseline.output.location <- "./output/baseline assessment.xlsx"

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
  
  # meshblocks with counts
  meshblocks <- 
    read_zipped_GIS(zipfile = meshblock.location) %>%
    st_transform(PROJECT.CRS) %>%
    left_join(read.csv(meshblock.count.location),
              by = c("MB_CODE16" = "MB_CODE_2016"))
  
  # find addresses in study area meshblocks meeting residential criteria,
  # with weights and nearest nodes
  residential.addresses <- addresses %>%
    # filter to region
    st_filter(region_buffer, .predicate = st_intersects) %>%
    # intersect with meshblocks
    st_intersection(meshblocks) %>%
    
    # exclude dwellings <= 5 or persons <= 10
    filter(Dwelling > 5 & Person > 10) %>%
    
    # add dwelling and person weights (meshblock count / number of address points in meshblock)
    group_by(MB_CODE16) %>%
    mutate(total_addresses = n()) %>%
    ungroup() %>%
    mutate(dwel_wt = Dwelling / total_addresses,
           pop_wt = Person / total_addresses) %>%
  
    # add nearest walking and cycling network nodes
    mutate(walk_node = network.nodes.walk$id[st_nearest_feature(., network.nodes.walk)],
           cycle_node = network.nodes.cycle$id[st_nearest_feature(., network.nodes.cycle)]) %>%
  
    # add id, and retain just id, weights and nodes
    mutate(id = row_number()) %>%
    dplyr::select(id, dwel_wt, pop_wt, walk_node, cycle_node)

  # write output
  st_write(residential.addresses, residential.address.location, 
           delete_layer = TRUE)
  
}


# 3 AC catchments ----
# -----------------------------------------------------------------------------#
## 3.1 Standard approach ----
## -----------------------------------------------------------------------------#
# catchments are residential addresses within required walking distance of AC
# 'anchors' - supermarket(s) if any, or else centroid

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
                   network.nodes.walk,
                   network.links.walk,
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


## 3.2 Alternative 'boundary' approach ----
## -----------------------------------------------------------------------------#
# catchments are residential addresses within required walking distance of the
# boundary of the AC

# create AC catchments
if (make.AC.catchments.boundary) {
  
  # load inputs
  residential.addresses <- st_read(residential.address.location)
  
  # omit 14 'undeveloped' ACs (can't select buffers for them)
  ACs.filtered <- ACs %>%
    filter(CENTRESIZE != "Undeveloped")
  
  # create temporary catchments folders to hold outputs from makeAcCatchments
  temp.address.location <- "./catchment addresses boundary"
  temp.polygon.location <- "./catchment polygons boundary"
  dir.create(temp.address.location)
  dir.create(temp.polygon.location)
  
  # run the function - saves files to the catchments folders
  makeAcCatchmentsBoundary(ACs.filtered,
                           network.nodes.walk,
                           network.links.walk,
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
  saveRDS(catchment.outputs[[1]], ac.catchment.address.location.boundary)
  st_write(catchment.outputs[[2]], ac.catchment.polygon.location.boundary, 
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
                                network.nodes.walk,
                                network.links.walk, 
                                PROJECT.CRS,
                                multiple.destinations = list(c("restaurant_cafe", 4)),
                                mode = "walk")
  
  # save output
  write.csv(baseline.node.distances, baseline.node.distance.location, row.names = FALSE)
  
}


# 5 Assess baseline status ----
# -----------------------------------------------------------------------------#
## 5.0 Set up output workbook (required for all parts of section 5) ----
## ------------------------------------#
# read in if it exists, or create if not
if (file.exists(baseline.output.location)) {
  wb <-loadWorkbook(baseline.output.location)
} else {
  wb <- createWorkbook()
}

## 5.1 Overall status ----
## ------------------------------------#

# calculate overall area coverage as percentage of population or dwellings with access to 
# each destination type within specified distance for Greater Melbourne, 
# all ACs, and large, medium and small ACs

# load inputs
residential.addresses <- st_read(residential.address.location)
baseline.distances <- residential.addresses  %>%
  st_drop_geometry() %>%
  # join distances
  left_join(read.csv(baseline.node.distance.location), 
            by = c("walk_node" = "node_id")) %>%
  # remove any columns for second-most-distant, etc
  dplyr::select(-matches("[0-9]$"))
ac.catchment.addresses <- readRDS(ac.catchment.address.location)
region <- st_read(region.location)

# calculate coverage
baseline.coverage.pop <- calculateCoverage(residential.addresses,
                                           baseline.distances, 
                                           ac.catchment.addresses, 
                                           ACs,
                                           region,
                                           mode = "people")

baseline.coverage.dwel <- calculateCoverage(residential.addresses,
                                            baseline.distances, 
                                            ac.catchment.addresses, 
                                            ACs,
                                            region,
                                            mode = "dwellings")


# write output
# add worksheets with required names if not already there
pop.name <- "area coverage pop"
dwel.name <- "area coverage dwel"
if (!pop.name %in% names(wb)) {
  addWorksheet(wb, sheetName = pop.name)
}
if (!dwel.name %in% names(wb)) {
  addWorksheet(wb, sheetName = dwel.name)
}

# write the results to the worksheets
writeData(wb, sheet = pop.name, baseline.coverage.pop)
writeData(wb, sheet = dwel.name, baseline.coverage.dwel)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, baseline.output.location, overwrite = TRUE)


## 5.2 ACs ----
## ------------------------------------#

# make table showing % of population or dwellings in each AC within specified walking
# distance of destinations

# load inputs
residential.addresses <- st_read(residential.address.location)
baseline.distances <- residential.addresses  %>%
  st_drop_geometry() %>%
  # join distances
  left_join(read.csv(baseline.node.distance.location), 
            by = c("walk_node" = "node_id")) %>%
  # remove any columns for second-most-distant, etc
  dplyr::select(-matches("[0-9]$"))
ac.catchment.addresses <- readRDS(ac.catchment.address.location)

# calculate coverage
baseline.AC.coverage.pop <- calculateAcCoverage(baseline.distances,
                                                ac.catchments.addresses,
                                                ACs,
                                                mode = "people")

baseline.AC.coverage.dwel <- calculateAcCoverage(baseline.distances,
                                                 ac.catchments.addresses,
                                                 ACs,
                                                 mode = "dwellings")

# write output
# add worksheets with required names if not already there
pop.name <- "AC coverage pop"
dwel.name <- "AC coverage dwel"
if (!pop.name %in% names(wb)) {
  addWorksheet(wb, sheetName = pop.name)
}
if (!dwel.name %in% names(wb)) {
  addWorksheet(wb, sheetName = dwel.name)
}

# write the results to the worksheets
writeData(wb, sheet = pop.name, baseline.AC.coverage.pop)
writeData(wb, sheet = dwel.name, baseline.AC.coverage.dwel)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, baseline.output.location, overwrite = TRUE)


## 5.3 AC summary ----
## ------------------------------------#

# summary table of ACs with 80% of population or dwellings with access to each destination
# type within specified walking distance

# load AC.pct.coverage from section 5.2
baseline.AC.coverage.pop <- read.xlsx(baseline.output.location, sheet = "AC coverage pop")
baseline.AC.coverage.dwel <- read.xlsx(baseline.output.location, sheet = "AC coverage dwel")

# calculate summary
baseline.AC.coverage.summary.pop <- calculateAcCoverageSummary(baseline.AC.coverage.pop)
baseline.AC.coverage.summary.dwel <- calculateAcCoverageSummary(baseline.AC.coverage.dwel)

# write output
# add worksheets with required names if not already there
pop.name <- "AC coverage summ pop"
dwel.name <- "AC coverage summ dwel"

if (!pop.name %in% names(wb)) {
  addWorksheet(wb, sheetName = pop.name)
}
if (!dwel.name %in% names(wb)) {
  addWorksheet(wb, sheetName = dwel.name)
}

# write the results to the worksheets
writeData(wb, sheet = pop.name, baseline.AC.coverage.summary.pop)
writeData(wb, sheet = dwel.name, baseline.AC.coverage.summary.dwel)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, baseline.output.location, overwrite = TRUE)


## 5.4 ACs  for 'boundary' approach ----
## ------------------------------------#
# # summary table of ACs with 80% of population or dwellings with access to each destination
# type within specified walking distance, for 'boundary' approach - based on 5.2

# load inputs
residential.addresses <- st_read(residential.address.location)
baseline.distances <- residential.addresses  %>%
  st_drop_geometry() %>%
  # join distances
  left_join(read.csv(baseline.node.distance.location), 
            by = c("walk_node" = "node_id")) %>%
  # remove any columns for second-most-distant, etc
  dplyr::select(-matches("[0-9]$"))
ac.catchment.addresses <- readRDS(ac.catchment.address.location.boundary)

# calculate coverage (note - mode is 'people' not 'dwellings')
baseline.AC.coverage.boundary <- calculateAcCoverage(baseline.distances,
                                                     ac.catchments.addresses,
                                                     ACs,
                                                     mode = "people")
# write output
# add worksheets with required names if not already there
boundary.name <- "AC coverage boundary"
if (!boundary.name %in% names(wb)) {
  addWorksheet(wb, sheetName = boundary.name)
}

# write the results to the worksheets
writeData(wb, sheet = boundary.name, baseline.AC.coverage.boundary)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, baseline.output.location, overwrite = TRUE)


## 5.5 AC summary for 'boundary' approach ----
## ------------------------------------#

# summary table of ACs with 80% of population with access to each destination
# type within specified walking distance

# load AC.pct.coverage from section 5.4
baseline.AC.coverage.boundary <- read.xlsx(baseline.output.location, sheet = "AC coverage boundary")

# calculate summary
baseline.AC.coverage.summary.boundary <- calculateAcCoverageSummary(baseline.AC.coverage.boundary)

# write output
# add worksheet with required name if not already there
boundary.name <- "AC coverage summ boundary"

if (!boundary.name %in% names(wb)) {
  addWorksheet(wb, sheetName = boundary.name)
}

# write the results to the worksheets
writeData(wb, sheet = boundary.name, baseline.AC.coverage.summary.boundary)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, baseline.output.location, overwrite = TRUE)


## 5.6 Comparison of pop, dwel and boundary ----
## ------------------------------------#
# comparison of number of ACs requiring new destinations for each approach
# read in the pop, dwel and boundary summaries
summary.pop <- read.xlsx(baseline.output.location, sheet = "AC coverage summ pop")
summary.dwel <- read.xlsx(baseline.output.location, sheet = "AC coverage summ dwel")
summary.boundary <- read.xlsx(baseline.output.location, sheet = "AC coverage summ boundary")

# total number of ACs (for last row of comparison table)
no.of.ACs <-data.frame(dest.dist = "Number of ACs",
               pop = summary.pop[summary.pop$dest.dist == "Number of ACs", "all.no"],
               dwel = summary.dwel[summary.dwel$dest.dist == "Number of ACs", "all.no"],
               boundary = summary.boundary[summary.boundary$dest.dist == "Number of ACs", "all.no"])

# comparison table
shortfall.comparison <- summary.pop %>%
  dplyr::select(dest.dist, pop = all.shortfall) %>%
  left_join(summary.dwel %>%
              dplyr::select(dest.dist, dwel = all.shortfall),
            by = "dest.dist") %>%
  left_join(summary.boundary %>%
              dplyr::select(dest.dist, boundary = all.shortfall),
            by = "dest.dist") %>%
  # remove and replace last 'Number of ACs' row
  filter(dest.dist != "Number of ACs") %>%
  bind_rows(., no.of.ACs)

# write output
# add worksheet with required name if not already there
comparison.name <- "AC shortfall comp"

if (!comparison.name %in% names(wb)) {
  addWorksheet(wb, sheetName = comparison.name)
}

# write the results to the worksheets
writeData(wb, sheet = comparison.name, shortfall.comparison)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, baseline.output.location, overwrite = TRUE)


## 5.7 LGA group analysis of pop and boundary   ----
## ------------------------------------#
# breakdown of numbers of ACs not meeting target for pop and boundary approaches
# by inner/middle/outer categorisation

# load and classify LGAs (inner, middle, outer)
LGAs <- read_zipped_GIS(zipfile = LGA.zipfile, subpath = LGA.subpath)
LGAs.classified <- classifyLGAs(LGAs)

# classify ACs as inner/middle/outer based on centroid
ACs.classified <- ACs %>%
  dplyr::select(CENTRE_NO) %>%
  st_centroid() %>%
  st_join(LGAs.classified %>% dplyr::select(NAME, group),
          join = st_intersects) %>%
  st_drop_geometry()

# load coverage results and join classification
baseline.AC.coverage.pop <- 
  read.xlsx(baseline.output.location, sheet = "AC coverage pop") %>%
  left_join(ACs.classified, by = c("centre_no" = "CENTRE_NO"))
baseline.AC.coverage.boundary <- 
  read.xlsx(baseline.output.location, sheet = "AC coverage boundary") %>%
  left_join(ACs.classified, by = c("centre_no" = "CENTRE_NO"))

# calculate the shortfalls by group
pop.shortfall <- calculateLgaGroupShortfalls(baseline.AC.coverage.pop)
boundary.shortfall <- calculateLgaGroupShortfalls(baseline.AC.coverage.boundary)

# make LGA group shortfall comparison table (note 'pop' becomes 'anc[hor]' and
# 'boundary' becomes 'b[oun]dry')
group.shortfall.comparison <- pop.shortfall %>%
  filter(dest.dist != "Number of ACs") %>%
  dplyr::select(dest.dist, inner.anc = inner.pct, 
                middle.anc = middle.pct, outer.anc = outer.pct) %>%
  left_join(boundary.shortfall %>% 
              filter(dest.dist != "Number of ACs") %>%
              dplyr::select(dest.dist, inner.bdry = inner.pct, 
                            middle.bdry = middle.pct, outer.bdry = outer.pct),
            by = "dest.dist") %>%
  # differences between the shortfall percentages
  mutate(inner.diff = inner.bdry - inner.anc,
         middle.diff = middle.bdry - middle.anc,
         outer.diff = outer.bdry - outer.anc) %>%
  # rearrange in required order
  dplyr::select(dest.dist,
                inner.anc, inner.bdry, inner.diff,
                middle.anc, middle.bdry, middle.diff,
                outer.anc, outer.bdry, outer.diff)


# write output
# add worksheet with required name if not already there
group.comparison.name <- "AC shortfall comp grp"

if (!group.comparison.name %in% names(wb)) {
  addWorksheet(wb, sheetName = group.comparison.name)
}

# write the results to the worksheets
writeData(wb, sheet = group.comparison.name, group.shortfall.comparison)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, baseline.output.location, overwrite = TRUE)


# 6 Small AC investigation ----
# -----------------------------------------------------------------------------#

## 6.1 Load required data ----
## -------------------------------------#
# baseline performance against destination tests, by AC
baseline.AC.coverage.pop <- read.xlsx(baseline.output.location, sheet = "AC coverage pop")
baseline.AC.coverage.dwel <- read.xlsx(baseline.output.location, sheet = "AC coverage dwel")

# AC catchments
ac.catchments <- readRDS(ac.catchment.address.location)


## 6.2 Allocate score ----
## -------------------------------------#
baseline.score <- function(baseline.AC.coverage) {
  output <- baseline.AC.coverage %>%
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
} 
  
baseline.score.pop <- baseline.score(baseline.AC.coverage.pop)
baseline.score.dwel <- baseline.score(baseline.AC.coverage.dwel)

## 6.3 Boxplot for score ----
## -------------------------------------#
scorePlot <- function(baseline.score) {
  # calculate counts for each category
  category.counts <- baseline.score %>%
    count(category)
  
  # create the boxplot and add counts
  score.plot <- ggplot(data = baseline.score, aes(x = category, y = score)) +
    # geom_jitter(position = position_jitter(width = 0.1), alpha = 0.2, colour = "blue") + 
    geom_boxplot() +
    labs( #title = "Distribution of scores by Activity Centre size",
         x = "Activity Centre population size (with number of centres)",
         y = "Score: no of destination targets met (max 14)") +
    scale_x_discrete(labels = paste0(category.counts$category, " (", category.counts$n, ")")) +
    theme_classic()
  
  return(score.plot)
}

score.plot.pop <- scorePlot(baseline.score.pop)
score.plot.dwel <- scorePlot(baseline.score.dwel)

# save
ggsave("./images/baseline_score_pop.png", score.plot.pop, width = 15, height = 12, units = "cm", dpi = 1000)
ggsave("./images/baseline_score_dwel.png", score.plot.dwel, width = 15, height = 12, units = "cm")

# means
means.pop <- baseline.score.pop %>%
  group_by(category) %>%
  summarise(mean = mean(score))
means.pop 
# category    mean
# <fct>      <dbl>
# 1 small <2k   4.59
# 2 small 2-5k  6.27
# 3 medium      5.60
# 4 large       6.26

means.dwel <- baseline.score.dwel %>%
  group_by(category) %>%
  summarise(mean = mean(score))
means.dwel
# category    mean
# <fct>      <dbl>
# 1 small <2k   4.61
# 2 small 2-5k  6.30
# 3 medium      5.65
# 4 large       6.34


## 6.4 Allocate overlap ----
## -------------------------------------#
allocateOverlap <- function(ac.catchments, baseline.score) {
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
  
  
  return(small.acs.with.groups)
}

small.acs.with.groups.pop <- allocateOverlap(ac.catchments, baseline.score.pop)
small.acs.with.groups.dwel <- allocateOverlap(ac.catchments, baseline.score.dwel)

## 6.5 Boxplot for overlap score ----
## -------------------------------------#
overlapPlot <- function(small.acs.with.groups) {
  # calculate counts for each category
  group.counts <- small.acs.with.groups %>%
    count(group) %>%
    # order so as to match the order in which the plot will be sorted
    mutate(order = case_when(
      group == "small <2k, large overlap" ~ 1,
      group == "small <2k, small overlap" ~ 2,
      group == "small 2-5k, large overlap" ~ 3,
      group == "small 2-5k, small overlap" ~ 4
    )) %>%
    arrange(order)
  
  # create the boxplot and add counts
  overlap.score.plot <- ggplot(data = small.acs.with.groups, aes(x = group, y = score)) +
    # geom_jitter(position = position_jitter(width = 0.1), alpha = 0.2, colour = "blue") + 
    geom_boxplot() +
    labs(#title = "Distribution of scores by small Activity Centre overlap group",
         x = "Activity Centre overlap category (with number of centres)",
         y = "Score: no of destination targets met (max 14)",
         caption = "'Large overlap' means 50% or more of the dwellings in the small AC are also in a\n large or medium AC; small overlap means less than 50%.") +
    scale_x_discrete(labels = function(x) str_wrap(paste0(x, " (", group.counts$n, ")"), width = 15)) +
    # scale_x_discrete(labels = paste0(group.counts$group, " (", group.counts$n, ")")) +
    theme_classic() +
    theme(plot.caption = element_text(hjust = 0))  #  left alignment
  
  return(overlap.score.plot)
}

overlap.plot.pop <- overlapPlot(small.acs.with.groups.pop)
overlap.plot.dwel <- overlapPlot(small.acs.with.groups.dwel)

# save
ggsave("./images/overlap_score_pop.png", overlap.plot.pop, width = 15, height = 12, units = "cm")
ggsave("./images/overlap_score_dwel.png", overlap.plot.dwel, width = 15, height = 12, units = "cm")


# means
means.overlap.pop <- small.acs.with.groups.pop %>%
  group_by(group) %>%
  summarise(mean = mean(score))
means.overlap.pop  
# group                      mean
# <chr>                     <dbl>
# 1 small 2-5k, large overlap  6.81
# 2 small 2-5k, small overlap  6.18
# 3 small <2k, large overlap   6.47
# 4 small <2k, small overlap   4.30

means.overlap.dwel <- small.acs.with.groups.dwel %>%
  group_by(group) %>%
  summarise(mean = mean(score))
means.overlap.dwel  
# group                      mean
# <chr>                     <dbl>
# 1 small 2-5k, large overlap  6.86
# 2 small 2-5k, small overlap  6.2 
# 3 small <2k, large overlap   6.5 
# 4 small <2k, small overlap   4.33


# 7 Distance from AC investigation ----
# -----------------------------------------------------------------------------#
# Find proportion of Melbourne population within 800m walking distance / 2km
# cycling distance from AC

## 7.1 AC anchors ----
## -------------------------------------#
# code from 'makeAcCatchments.R'

# ACs and supermarkets
ACs.developed = ACs %>% filter(CENTRESIZE != "Undeveloped")
supermarkets = st_read(POIs.location) %>% filter(Attribute == "supermarket")

# buffer ACs by 30m, to catch supermarket locations placed in adjacent roads
ACs.buffered <- ACs.developed %>%
  st_buffer(30)

supermarket.anchors <- st_intersection(ACs.buffered, supermarkets) %>%
  dplyr::select(CENTRE_NO, size)

centroid.anchors <- ACs %>%
  # ACs that don't have supermarkets
  filter(!CENTRE_NO %in% supermarket.anchors$CENTRE_NO) %>%
  # centroid
  st_centroid() %>%
  dplyr::select(CENTRE_NO, size)

anchors <- bind_rows(supermarket.anchors,
                     centroid.anchors) %>%
  # add nearest nodes
  mutate(walk_node = network.nodes.walk$id[st_nearest_feature(., network.nodes.walk)],
         cycle_node = network.nodes.cycle$id[st_nearest_feature(., network.nodes.cycle)])


## 7.2 Distances from ACs to walking and cycling nodes ----
## -------------------------------------#
# code adapted from elements of 'addressDestinationDistances.R'

# residential addresses
residential.addresses <- st_read(residential.address.location)

# walking and cycling graphs
g.walk <- graph_from_data_frame(network.links.walk %>%
                                  st_drop_geometry() %>%
                                  mutate(weight = length) %>%
                                  dplyr::select(from_id, to_id, id, weight), 
                                directed = F)

g.cycle <- graph_from_data_frame(network.links.cycle %>%
                                   st_drop_geometry() %>%
                                   mutate(weight = length) %>%
                                   dplyr::select(from_id, to_id, id, weight), 
                                 directed = F)

# unique walking and cycling nodes
residential.walk.nodes <- unique(residential.addresses$walk_node)
residential.cycle.nodes <- unique(residential.addresses $cycle_node)

# distances from anchors to residential nodes
walk.distances <- distances(g.walk,
                            as.character(anchors$walk_node),
                            as.character(residential.walk.nodes))

cycle.distances <- distances(g.cycle,
                            as.character(anchors$cycle_node),
                            as.character(residential.cycle.nodes))

# minimum distances for each residential node (columns are resid nodes, so 2)
min.dist.walk <- apply(walk.distances, 2, min, na.rm = TRUE) %>%  
  as.data.frame() %>%
  cbind(id = as.numeric(row.names(.))) %>%
  rename(walk_dist = ".")  

min.dist.cycle <- apply(cycle.distances, 2, min, na.rm = TRUE) %>%  
  as.data.frame() %>%
  cbind(id = as.numeric(row.names(.))) %>%
  rename(cycle_dist = ".")  


## 7.3 Percentage of residences within walking/cycling distances ----
## -------------------------------------#
# join residential addresses to the walking/cycling distances
addresses.with.dist <- residential.addresses %>%
  st_drop_geometry() %>%
  left_join(min.dist.walk, by = c("walk_node" = "id")) %>%
  left_join(min.dist.cycle, by = c("cycle_node" = "id"))

chk <- addresses.with.dist %>%
  filter(walk_dist != cycle_dist)

# percentages within 800m walking / 2km cycling distance
people <- sum(addresses.with.dist$pop_wt)
people.within.walk.dist <- sum(addresses.with.dist %>%
                                 filter(walk_dist <= 800) %>%
                                 .$pop_wt)
people.within.cycle.dist <- sum(addresses.with.dist %>%
                                 filter(cycle_dist <= 2000) %>%
                                 .$pop_wt)

walk.pct <- people.within.walk.dist / people * 100
cycle.pct <- people.within.cycle.dist / people * 100

walk.pct  # 55.49034
cycle.pct  # 89.32641
> 

