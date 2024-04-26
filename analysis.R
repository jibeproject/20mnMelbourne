# analyse accessibility and underutilised capacity consequences of intervention (destination component)

#------------------------------------------------------------------------------#

# Outline - 
# Section 1 - set up and load data required for sections 2 and 3
# Section 2 - accessibility analysis
# Section 3 - underutilisation analysis

#------------------------------------------------------------------------------#

# 1 Setup ----
#------------------------------------------------------------------------------#
## 1.1 Libraries ----
## ------------------------------------#
library(tidyverse)
library(fs)
library(sf)
library(igraph)
library(doSNOW)
library(parallel)
library(foreach)
library(openxlsx)


## 1.2 Functions ----
## ------------------------------------#
dir_walk(path = "./functions/", source, recurse = T, type = "file")


## 1.3 Parameters ----
## ------------------------------------#
PROJECT.CRS <- 28355

# node distances for accessibility analysis (section 2.1): set to F if using existing, 
# or create in section 2.1
find.accessibility.node.distances <- F

# people within destination catchements for underutilisation analysis (section 3.1):
# set to F if using existing, or create in section 3.1
find.people.served <- F


## 1.4 Data ----
## ------------------------------------#
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

# residential addresses (created in section 2 of baseline.R)
residential.addresses <- st_read("./output/residential_addresses.sqlite")

# baseline locations
POIs.location <- "../data/processed/Destinations weights/Baseline/poi_weight.gpkg"
ANLS.pos.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"
ANLS.dest.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"
# baseline destinations - first layer is the types, subsequent layers one for each type
baseline.destinations <- loadBaselineDestinations(POIs.location, 
                                                  ANLS.dest.location,
                                                  ANLS.pos.location,
                                                  region_buffer,
                                                  PROJECT.CRS)

# intervention locations - first layer is the types, subsequent layers one for each type
intervention.destination.location <- "./output/intervention locations.sqlite"
intervention.destinations <- list()
intervention.destinations[[1]] <- st_layers(intervention.destination.location)$name
for (i in 1:length(intervention.destinations[[1]])) {
  layername = intervention.destinations[[1]][i]
  intervention.destinations[[i+1]] <- 
    st_read("./output/intervention locations.sqlite", layer = layername)
}

# local government areas
LGAs <- read_zipped_GIS(zipfile = "../data/original/LGAs.zip",
                         subpath = "/mga94_55/esrishape/whole_of_dataset/victoria/VMADMIN")

# SA2s
SA2s <- read_zipped_GIS(zipfile = "../data/original/1270055001_sa2_2016_aust_shape.zip") %>%
  st_transform(PROJECT.CRS)

# AC catchments
ac.catchment.address.location <- "./output/ac_catchment_addresses.rds"
ac.catchment.polygon.location <- "./output/ac_catchment_polygons.sqlite"

# people within catchments of destinations
people.served.location <- "./output/people served.sqlite"

# output table locations
accessibility.tables.location <- "./output/accessibility tables.xlsx"
underutilisation.tables.location <- "./output/underutilisation tables.xlsx"


# 2 Accessibility analysis ----
# -----------------------------------------------------------------------------#

## 2.0 Set up output workbook (required for sections 2.3 to 2.5) ----
## ------------------------------------#
# read in if it exists, or create if not
if (file.exists(accessibility.tables.location)) {
  wb <-loadWorkbook(accessibility.tables.location)
} else {
  wb <- createWorkbook()
}

## 2.1 Destination address node distances ----
## ------------------------------------#

if (find.accessibility.node.distances) {
  
  ### 2.1.1 Multiple destinations ----
  ### -----------------------------------#
  # destinations for which multiple 'nearest' are to be found, in the form of a list,
  # where each element of the list is a vector with 2 elements: destination type, and
  # number of them to be found
  multiple.destinations <- list(c("restaurant_cafe", 4))
  # multiple.destinations <- NA
  
  ### 2.1.2 Baseline ----
  ### ------------------------------------#
  # 'walk' is the same as in baseline.R (but verify that 'multiple.destinations'
  # is the same as is required here)
  
  baseline.node.distances.walk <- read.csv("./output/node_distances_baseline_walk.csv")
  
  # # alternatively, if the baseline needs to be recreated
  # baseline.node.distances.walk <-
  #   addressDestinationDistances(baseline.destinations,
  #                               residential.addresses,
  #                               network.nodes.walk,
  #                               network.links.walk,
  #                               PROJECT.CRS,
  #                               multiple.destinations,
  #                               mode = "walk")
  

  # cycle
  baseline.node.distances.cycle <-
    addressDestinationDistances(baseline.destinations,
                                residential.addresses,
                                network.nodes.cycle,
                                network.links.cycle,
                                PROJECT.CRS,
                                multiple.destinations,
                                mode = "cycle")
  
  # save output
  write.csv(baseline.node.distances.cycle, "./output/node_distances_baseline_cycle.csv", row.names = FALSE)
  
  
  ### 2.1.3 Intervention ----
  ### ------------------------------------#
  # compile combined baseline and intervention destinations
  all.destinations <- list()
  
  all.destinations[[1]] <- baseline.destinations[[1]]
  
  for (i in 1:length(baseline.destinations[[1]])) {
    # destination type
    dest.type <- baseline.destinations[[1]][i]
    
    # baseline locations for that destination type
    output <- as.data.frame(baseline.destinations[[i+1]]) %>%
      st_sf() %>%
      st_set_geometry("geom")
    
    # intervention locations for that destination type, if any
    if (dest.type %in% intervention.destinations[[1]]) {
      int.dest.idx <- which(intervention.destinations[[1]] == dest.type)
      output.intervention <- as.data.frame(intervention.destinations[[int.dest.idx + 1]]) %>%
        st_sf() %>%
        st_set_geometry("geom")
      output <- bind_rows(output, output.intervention)
    }
    
    # add to the list
    all.destinations[[i+1]] <- output
    
  }
  
  # optionally, for memory (if needed again, recreate in section 1.4) - 
  rm(baseline.destinations, intervention.destinations)
  
  # walk
  intervention.node.distances.walk <- 
    addressDestinationDistances(all.destinations,
                                residential.addresses,
                                network.nodes.walk,
                                network.links.walk, 
                                PROJECT.CRS,
                                multiple.destinations,
                                mode = "walk")
  
  # save output
  write.csv(intervention.node.distances.walk, "./output/node_distances_intervention_walk.csv", row.names = FALSE)
  
  # cycle
  intervention.node.distances.cycle <- 
    addressDestinationDistances(all.destinations,
                                residential.addresses,
                                network.nodes.cycle,
                                network.links.cycle, 
                                PROJECT.CRS,
                                multiple.destinations,
                                mode = "cycle")
  
  # save output
  write.csv(intervention.node.distances.cycle, "./output/node_distances_intervention_cycle.csv", row.names = FALSE)

}

## 2.2 Read in node distances and calculate scores ----
## ------------------------------------#
# walk
baseline.walk <- read.csv("./output/node_distances_baseline_walk.csv")  # see notes in 2.1 on this file
intervention.walk <- read.csv("./output/node_distances_intervention_walk.csv")

baseline.walk.scores <- calculateAccessibilityScores(baseline.walk, mode = "walk")
intervention.walk.scores <- calculateAccessibilityScores(intervention.walk, mode = "walk")

# write output (can be used for display)
write.csv(baseline.walk.scores %>%
            left_join(intervention.walk.scores,
                      by = "node_id",
                      suffix = c("_base", "_int")),
          "./output/dwel accessibility scores walk.csv", row.names = FALSE)

#cycle
baseline.cycle <- read.csv("./output/node_distances_baseline_cycle.csv")
intervention.cycle <- read.csv("./output/node_distances_intervention_cycle.csv")

baseline.cycle.scores <- calculateAccessibilityScores(baseline.cycle, mode = "cycle")
intervention.cycle.scores <- calculateAccessibilityScores(intervention.cycle, mode = "cycle")

# write output (can be used for display)
write.csv(baseline.cycle.scores %>%
            left_join(intervention.cycle.scores,
                      by = "node_id",
                      suffix = c("_base", "_int")),
          "./output/dwel accessibility scores cycle.csv", row.names = FALSE)


## 2.3 Aggregate scores for LGAs ----
## ------------------------------------#

# note that only 'people' mode is used, not 'dwellings'

addresses.with.LGA <- residential.addresses %>%
  st_join(., classifyLGAs(LGAs) %>% dplyr::select(NAME, group), .predicate = st_intersects) %>%
  mutate(LGA = case_when(NAME == "MERRI-BEK" ~ "Merri-bek",
                         TRUE ~ str_to_title(NAME))) %>%
  st_drop_geometry()

# walk
address.scores.walk <- addresses.with.LGA %>%
  left_join(read.csv("./output/dwel accessibility scores walk.csv"),
            by = c("walk_node" = "node_id"))

LGA.scores.walk <- calculateLgaAccessibilityScores(address.scores.walk,
                                                   mode = "people")

# write output
# add worksheet with required name if not already there
LGA.scores.walk.name <- "LGA accessibility scores walk"

if (!LGA.scores.walk.name %in% names(wb)) {
  addWorksheet(wb, sheetName = LGA.scores.walk.name)
}

# write the results to the worksheets
writeData(wb, sheet = LGA.scores.walk.name, LGA.scores.walk)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, accessibility.tables.location, overwrite = TRUE)


# cycle
address.scores.cycle <- addresses.with.LGA %>%
  left_join(read.csv("./output/dwel accessibility scores cycle.csv"),
            by = c("cycle_node" = "node_id"))

LGA.scores.cycle <- calculateLgaAccessibilityScores(address.scores.cycle,
                                                    mode = "people")

# write output
# add worksheet with required name if not already there
LGA.scores.cycle.name <- "LGA accessibility scores cycle"

if (!LGA.scores.cycle.name %in% names(wb)) {
  addWorksheet(wb, sheetName = LGA.scores.cycle.name)
}

# write the results to the worksheets
writeData(wb, sheet = LGA.scores.cycle.name, LGA.scores.cycle)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, accessibility.tables.location, overwrite = TRUE)


## 2.4 Combined walk/cycle table for main single hard score approach ----
## ------------------------------------#

# load single hard walk and cycle scores
walk.scores <- read.xlsx(accessibility.tables.location,
                         sheet = "LGA accessibility scores walk") %>%
  dplyr::select(group, LGA, 
                score_single_hard_base, score_single_hard_int,
                score_single_hard_diff, score_single_hard_rank)
cycle.scores <- read.xlsx(accessibility.tables.location,
                         sheet = "LGA accessibility scores cycle") %>%
  dplyr::select(group, LGA, 
                score_single_hard_base, score_single_hard_int,
                score_single_hard_diff, score_single_hard_rank)

score.summary <- walk.scores %>%
  left_join(cycle.scores, by = c("group", "LGA"),
            suffix = c("_base", "_int")) %>%
  arrange(group, score_single_hard_rank_base)

# write output
# add worksheet with required name if not already there
LGA.scores.summary.name <- "LGA accessibility scores summ"

if (!LGA.scores.summary.name %in% names(wb)) {
  addWorksheet(wb, sheetName = LGA.scores.summary.name)
}

# write the results to the worksheets
writeData(wb, sheet = LGA.scores.summary.name, score.summary)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, accessibility.tables.location, overwrite = TRUE)


## 2.5 Aggregate scores for SA2s ----
## ------------------------------------#

# requires baseline and intervention scores created in section 2.2
# note that only 'people' mode is used, not 'dwellings'

addresses.with.SA2 <- residential.addresses %>%
  st_join(., SA2s %>% dplyr::select(SA2_MAIN16), .predicate = st_intersects) %>%
  st_drop_geometry()

# walk
address.scores.walk <- addresses.with.SA2 %>%
  left_join(read.csv("./output/dwel accessibility scores walk.csv"),
            by = c("walk_node" = "node_id"))

SA2.scores.walk <- calculateSA2AccessibilityScores(address.scores.walk,
                                                   mode = "people")

# write output
# add worksheet with required name if not already there
SA2.scores.walk.name <- "SA2 accessibility scores walk"

if (!SA2.scores.walk.name %in% names(wb)) {
  addWorksheet(wb, sheetName = SA2.scores.walk.name)
}

# write the results to the worksheets
writeData(wb, sheet = SA2.scores.walk.name, SA2.scores.walk)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, accessibility.tables.location, overwrite = TRUE)


# cycle
address.scores.cycle <- addresses.with.SA2 %>%
  left_join(read.csv("./output/dwel accessibility scores cycle.csv"),
            by = c("cycle_node" = "node_id"))

SA2.scores.cycle <- calculateSA2AccessibilityScores(address.scores.cycle,
                                                    mode = "people")

# write output
# add worksheet with required name if not already there
SA2.scores.cycle.name <- "SA2 accessibility scores cycle"

if (!SA2.scores.cycle.name %in% names(wb)) {
  addWorksheet(wb, sheetName = SA2.scores.cycle.name)
}

# write the results to the worksheets
writeData(wb, sheet = SA2.scores.cycle.name, SA2.scores.cycle)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, accessibility.tables.location, overwrite = TRUE)


#  3 Underutilisation analysis ----
# -----------------------------------------------------------------------------#

## 3.0 Set up output workbook (required for sections 3.4 and 3.5) ----
## ------------------------------------#
# read in if it exists, or create if not
if (file.exists(underutilisation.tables.location)) {
  wb <-loadWorkbook(underutilisation.tables.location)
} else {
  wb <- createWorkbook()
}


## 3.1 People served by destinations ----
## ------------------------------------#
# note - requires intervention destinations and baseline destination; if 
# these have been removed in section 2.1.3, re-create them using section 1.4

# find the number of people in the walking/cycling catchments of each of the
# new/existing destination locations

if (find.people.served) {
  people.served.new.walk <- peopleServed(intervention.destinations,
                                         residential.addresses,
                                         network.nodes.walk,
                                         network.links.walk, 
                                         PROJECT.CRS,
                                         transport = "walk",
                                         mode = "people")
  st_write(people.served.new.walk, people.served.location, 
           layer = "new_walk", delete_layer = TRUE)
  
  people.served.new.cycle <- peopleServed(intervention.destinations,
                                          residential.addresses,
                                          network.nodes.cycle,
                                          network.links.cycle, 
                                          PROJECT.CRS,
                                          transport = "cycle",
                                          mode = "people")
  st_write(people.served.new.cycle, people.served.location, 
           layer = "new_cycle", delete_layer = TRUE)
  
  people.served.existing.walk <- peopleServed(baseline.destinations,
                                              residential.addresses,
                                              network.nodes.walk,
                                              network.links.walk, 
                                              PROJECT.CRS,
                                              transport = "walk",
                                              mode = "people")
  st_write(people.served.existing.walk, people.served.location, 
           layer = "existing_walk", delete_layer = TRUE)
  
  people.served.existing.cycle <- peopleServed(baseline.destinations,
                                               residential.addresses,
                                               network.nodes.cycle,
                                               network.links.cycle, 
                                               PROJECT.CRS,
                                               transport = "cycle",
                                               mode = "people")
  st_write(people.served.existing.cycle, people.served.location, 
           layer = "existing_cycle", delete_layer = TRUE)
  
} else {
  people.served.new.walk <- st_read(people.served.location, layer = "new_walk")
  people.served.new.cycle <- st_read(people.served.location, layer = "new_cycle")
  people.served.existing.walk <- st_read(people.served.location, layer = "existing_walk")
  people.served.existing.cycle <- st_read(people.served.location, layer = "existing_cycle")
}


## 3.2 Population requirements ----
## ------------------------------------#
# table of population requirements
pop.reqts <- tribble(
  ~dest_type             , ~pop_reqt,
  "supermarket"          , 10000    ,
  "convenience_store"    ,  1000    ,
  "cafe"                 ,  1000    ,
  "pharmacy"             , 10000    ,
  "post"                 , 10000    ,
  "gp"                   ,  1000    ,
  "maternal_child_health", 16000    ,
  "dentist"              ,  1000    ,
  "childcare"            ,  9000    ,
  "kindergarten"         , 10000    ,
  "primary"              ,  9000    ,
  "community_centre"     ,  8000    ,
  "park"                 ,  1000    ,
  "bus"                  ,  1000
)


## 3.3 Utilisation for destinations ----
## ------------------------------------#
# function to add dest_type to existing destinations, and exclude those
# that aren't in baseline
addDestType <- function(people.served) {
  output <- people.served %>%
    # dest_type column, based on existing destination attributes
    mutate(dest_type = case_when(
      attribute == "supermarket"            ~ "supermarket",
      str_detect(dest_class, "convenience") ~ "convenience_store",
      attribute == "restaurant"             ~ "restaurant", # OMIT
      attribute == "cafe"                   ~ "cafe", 
      attribute == "pharmacy"               ~ "pharmacy",
      attribute == "post_office"            ~ "post",
      str_detect(dest_class, "gp")          ~ "gp",
      str_detect(dest_class, "mc_family")   ~ "maternal_child_health",
      str_detect(dest_class, "dentist")     ~ "dentist",
      attribute == "child care"             ~ "childcare",
      str_detect(dest_class, "preschool")   ~ "kindergarten",
      str_detect(dest_class, "P_12") | str_detect(dest_class, "primary") ~ "primary",
      attribute == "community centre"       ~ "community_centre",
      attribute == "library"                ~ "library",  # OMIT
      !is.na(aos_id)                        ~ "park",
      attribute == "bus"                    ~ "bus",
      attribute == "tram"                   ~ "tram",  # OMIT
      str_detect(dest_class, "train")       ~ "train",  # OMIT
     )) %>%
    filter(!dest_type %in% c("restaurant", "library", "tram", "train"))
}

# for each new destination, dwellings served / dwelling requirement
utilisation.new.walk <- people.served.new.walk %>%
  left_join(pop.reqts, by = "dest_type") %>%
  mutate(utilisation = served / pop_reqt)

utilisation.new.cycle <- people.served.new.cycle %>%
  left_join(pop.reqts, by = "dest_type") %>%
  mutate(utilisation = served / pop_reqt)

utilisation.existing.walk <- addDestType(people.served.existing.walk) %>%
  left_join(pop.reqts, by = "dest_type") %>%
  mutate(utilisation = served / pop_reqt)

utilisation.existing.cycle <- addDestType(people.served.existing.cycle) %>%
  left_join(pop.reqts, by = "dest_type") %>%
  mutate(utilisation = served / pop_reqt)


## 3.4 Calculate average for each destination type and LGA ----
## ------------------------------------#
util.LGA.new.walk <- 
  calculateLgaUtilisationScores(utilisation.new.walk, LGAs)
util.LGA.new.cycle <- 
  calculateLgaUtilisationScores(utilisation.new.cycle, LGAs)
util.LGA.existing.walk <- 
  calculateLgaUtilisationScores(utilisation.existing.walk, LGAs)
util.LGA.existing.cycle <- 
  calculateLgaUtilisationScores(utilisation.existing.cycle, LGAs)

# write output
# add worksheets with required names if not already there
util.LGA.new.walk.name <- "LGA new walk"
util.LGA.new.cycle.name <- "LGA new cycle"
util.LGA.existing.walk.name <- "LGA existing walk"
util.LGA.existing.cycle.name <- "LGA existing cycle"

if (!util.LGA.new.walk.name %in% names(wb)) {
  addWorksheet(wb, sheetName = util.LGA.new.walk.name)
}
if (!util.LGA.new.cycle.name %in% names(wb)) {
  addWorksheet(wb, sheetName = util.LGA.new.cycle.name)
}
if (!util.LGA.existing.walk.name %in% names(wb)) {
  addWorksheet(wb, sheetName = util.LGA.existing.walk.name)
}
if (!util.LGA.existing.cycle.name %in% names(wb)) {
  addWorksheet(wb, sheetName = util.LGA.existing.cycle.name)
}

# write the results to the worksheets
writeData(wb, sheet = util.LGA.new.walk.name, util.LGA.new.walk)
writeData(wb, sheet = util.LGA.new.cycle.name, util.LGA.new.cycle)
writeData(wb, sheet = util.LGA.existing.walk.name, util.LGA.existing.walk)
writeData(wb, sheet = util.LGA.existing.cycle.name, util.LGA.existing.cycle)


# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, underutilisation.tables.location, overwrite = TRUE)


## 3.5 Calculate average for each destination type and SA2 ----
## ------------------------------------#
util.SA2.new.walk <- 
  calculateSA2UtilisationScores(utilisation.new.walk, SA2s)
util.SA2.new.cycle <- 
  calculateSA2UtilisationScores(utilisation.new.cycle, SA2s)
util.SA2.existing.walk <- 
  calculateSA2UtilisationScores(utilisation.existing.walk, SA2s)
util.SA2.existing.cycle <- 
  calculateSA2UtilisationScores(utilisation.existing.cycle, SA2s)

# write output
# add worksheets with required names if not already there
util.SA2.new.walk.name <- "SA2 new walk"
util.SA2.new.cycle.name <- "SA2 new cycle"
util.SA2.existing.walk.name <- "SA2 existing walk"
util.SA2.existing.cycle.name <- "SA2 existing cycle"

if (!util.SA2.new.walk.name %in% names(wb)) {
  addWorksheet(wb, sheetName = util.SA2.new.walk.name)
}
if (!util.SA2.new.cycle.name %in% names(wb)) {
  addWorksheet(wb, sheetName = util.SA2.new.cycle.name)
}
if (!util.SA2.existing.walk.name %in% names(wb)) {
  addWorksheet(wb, sheetName = util.SA2.existing.walk.name)
}
if (!util.SA2.existing.cycle.name %in% names(wb)) {
  addWorksheet(wb, sheetName = util.SA2.existing.cycle.name)
}

# write the results to the worksheets
writeData(wb, sheet = util.SA2.new.walk.name, util.SA2.new.walk)
writeData(wb, sheet = util.SA2.new.cycle.name, util.SA2.new.cycle)
writeData(wb, sheet = util.SA2.existing.walk.name, util.SA2.existing.walk)
writeData(wb, sheet = util.SA2.existing.cycle.name, util.SA2.existing.cycle)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, underutilisation.tables.location, overwrite = TRUE)


## 3.6 Calculate LGA density and compare utilisation ----
## ------------------------------------#
# read in ACs
ac.catchment.addresses <- readRDS(ac.catchment.address.location)
ac.catchment.polygons <- st_read(ac.catchment.polygon.location)

# dwellings in ACs in each LGA
LGA.ac.dwel <- residential.addresses %>%
  # filter to all address ids comprising the ac catchments
  filter(id %in% (ac.catchment.addresses$address_ids %>% 
                    unlist() %>% 
                    unique())) %>%
  # join LGAs
  st_join(classifyLGAs(LGAs) %>% dplyr::select(NAME, group),
          join = st_intersects) %>%
  # sum pop_weights for LGA to get LGA population
  st_drop_geometry() %>%
  group_by(NAME, group) %>%
  summarise(dwel = sum(dwel_wt)) %>%
  ungroup()
  
# area of ACs in LGAs
LGA.ac.area <- ac.catchment.polygons %>%
  summarise() %>%
  # intersect with LGAs
  st_intersection(., LGAs %>% dplyr::select(NAME)) %>%
  # add area in hectares
  mutate(area_ha = as.numeric(st_area(.)) / 10000) %>%
  st_drop_geometry()

# calculate density, in dwellings/ha, of AC polygons within each LGA
LGA.density <- LGA.ac.dwel %>%
  left_join(LGA.ac.area, by = "NAME") %>%
  mutate(dwel_ha = dwel / area_ha)

# function to read in underutilisation tables and calculate a single util score
meanUtilScore <- function(underutilisation.tables.location, sheet.name, LGA.density) {
  
  output <- read.xlsx(underutilisation.tables.location, sheet = sheet.name) %>%
           # calculate mean of the individual destination values
           mutate(mean_util = rowMeans(select(., -c(NAME, LGA, group)), na.rm = TRUE)) %>%
           # remove the individual destination utilisations, and join density
           dplyr::select(NAME, LGA, group, mean_util) %>%
           left_join(LGA.density %>% dplyr::select(NAME, dwel_ha), by = "NAME")
  
  return(output)
}

# read in underutilisation tables and calculate a single util score
LGA.util.new.walk <- meanUtilScore(underutilisation.tables.location,
                                   "LGA new walk", LGA.density)
LGA.util.new.cycle <- meanUtilScore(underutilisation.tables.location,
                                   "LGA new cycle", LGA.density)
LGA.util.existing.walk <- meanUtilScore(underutilisation.tables.location,
                                   "LGA existing walk", LGA.density)
LGA.util.existing.cycle <- meanUtilScore(underutilisation.tables.location,
                                   "LGA existing cycle", LGA.density)

# plot density against utilisation
utilPlot <- function(walk.data, cycle.data) {
  
  # calculate regression for the models, and find the r-squared
  lm_model_walk <- lm(mean_util ~ dwel_ha, data = walk.data)
  lm_model_cycle <- lm(mean_util ~ dwel_ha, data = cycle.data)
  r2_walk <- summary(lm_model_walk)$adj.r.squared
  r2_cycle <- summary(lm_model_cycle)$adj.r.squared
  
  # get the x- and y-coordinates for the R-squared text
  x_text_walk <- max(walk.data$dwel_ha) * 0.9  # a percentage of the maximum dwel_ha value
  y_text_walk <- predict(lm_model_walk, 
                         newdata = data.frame(dwel_ha = x_text_walk)) + 1.5
  x_text_cycle <- max(cycle.data$dwel_ha) * 0.9  # a percentage of the maximum dwel_ha value
  y_text_cycle <- predict(lm_model_cycle, 
                          newdata = data.frame(dwel_ha = x_text_cycle)) + 1.2
  
  # add columns combining group and mode
  walk.data <- walk.data %>%
    mutate(groupmode = paste("Walking -", group))
  cycle.data <- cycle.data %>%
    mutate(groupmode = paste("Cycling -", group))
  
  util.plot <- ggplot() +
    # walk data
    geom_smooth(data = walk.data,
                aes(x = dwel_ha, y = mean_util), method = lm, se = FALSE,
                linetype = "dashed", color = "black") +  
    geom_point(data = walk.data,
               aes(x = dwel_ha, y = mean_util, shape = groupmode, fill = groupmode), 
               colour = "black", size = 3) +  # set outline color to black
    # cycle data
    geom_smooth(data = cycle.data,
                aes(x = dwel_ha, y = mean_util), method = lm, se = FALSE,
                linetype = "dashed", color = "black") +  
    geom_point(data = cycle.data,
               aes(x = dwel_ha, y = mean_util, shape = groupmode, fill = groupmode), 
               colour = "black", size = 3) +  # set outline color to black
    # other plot elements
    labs(x = "Density (dwellings per hectare)",
         y = "Mean utilisation score") +
    scale_y_continuous(breaks = seq(1, ceiling(max(walk.data$dwel_ha)), by = 1)) +
    scale_shape_manual(values = c(22, 21, 24, 22, 21, 23)) + # different shapes for outer cycling/walking, because they are close on the plot
    scale_fill_manual(values = c("#1b9e77", "#7570b3", "#d95f02", "#1b9e77", "#7570b3", "#d95f02")) +
    guides(fill = guide_legend(title = "Local\nGovernment\nAreas"),
           shape = guide_legend(title = "Local\nGovernment\nAreas")) +
    theme_classic() +
    # labels for mode and r-squared
    geom_label(aes(x = x_text_walk, y = y_text_walk,
                   label = "Walking utilisation scores"),
               hjust = 1, vjust = 0, size = 5, colour = "black",
               fill = "white", alpha = 0.7) +
    # fill = "white", alpha = 0.7, label.size = NA, label.padding = unit(0, "lines")) +  # alt with no border
    geom_label(aes(x = x_text_cycle, y = y_text_cycle,
                   label = "Cycling utilisation scores"),
               hjust = 1, vjust = 0, size = 5, colour = "black", 
               fill = "white", alpha = 0.7) +
    # fill = "white", alpha = 0.7, label.size = NA, label.padding = unit(0, "lines")) +  # alt with no border
    geom_label(aes(x = x_text_walk, y = y_text_walk, 
                   label = paste("R-squared:", round(r2_walk, 2))),
               hjust = 1, vjust = 1.5, size = 3, color = "black",
               fill = "white", alpha = 0.7, label.size = NA,
               label.padding = unit(0, "lines")) +
    geom_label(aes(x = x_text_cycle, y = y_text_cycle, 
                   label = paste("R-squared:", round(r2_cycle, 2))),
               hjust = 1, vjust = 1.5, size = 3, color = "black",
               fill = "white", alpha = 0.7, label.size = NA,
               label.padding = unit(0, "lines"))
  return(util.plot)
}

util.plot.new <- utilPlot(LGA.util.new.walk, LGA.util.new.cycle)
util.plot.existing <- utilPlot(LGA.util.existing.walk, LGA.util.existing.cycle)

ggsave("./images/util_plot_new.png", util.plot.new, width = 24, 
       height = 16, units = "cm", dpi = 1000)
ggsave("./images/util_plot_existing.png", util.plot.existing, width = 24, 
       height = 16, units = "cm", dpi = 1000)

# details of range of new walking and cycling scores
min(LGA.util.new.walk$mean_util)  # 0.2651838
max(LGA.util.new.walk$mean_util)  # 1.510051
min(LGA.util.new.cycle$mean_util)  # 1.197553
max(LGA.util.new.cycle$mean_util)  # 9.173139
