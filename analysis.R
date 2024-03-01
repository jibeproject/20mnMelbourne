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


## 1.2 Functions ----
## ------------------------------------#
dir_walk(path = "./functions/", source, recurse = T, type = "file")


## 1.3 Parameters ----
## ------------------------------------#
PROJECT.CRS <- 28355


## 1.4 Data ----
## ------------------------------------#
# region buffer
region_buffer <- st_read("../data/processed/region_buffer.sqlite")

# network, and filter to region buffer
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

# keep just the largest connected networks
network.walk <- largestConnectedComponent(nodes.walk, links.walk)
network.nodes.walk <- network.walk[[1]]
network.links.walk <- network.walk[[2]]

network.cycle <- largestConnectedComponent(nodes.cycle, links.cycle)
network.nodes.cycle <- network.cycle[[1]]
network.links.cycle <- network.cycle[[2]]

# residential addresses (created in section 2 of baseline.R)
residential.addresses <- st_read("./output/residential_addresses.sqlite")

# baseline locations
POIs.location <- "../data/processed/Weighted POIs/poi.gpkg"
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


# node distances for accessibility analysis (section 2.1): set to F if using existing, 
# or create in section 2.1
find.accessibility.node.distances <- F

# distances from destinations to dwellings for underutilisation analysis (section 3.1):
# set to F if using existing, or create in section 3.1
find.destination.dwelling.distances <- F


# 2 Accessibility analysis ----
# -----------------------------------------------------------------------------#

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
  
  baseline.node.distances.walk <- read.csv("./output/node_distances_baseline.csv")
  
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
baseline.walk <- read.csv("./output/node_distances_baseline.csv")  # see notes in 2.1 on this file
intervention.walk <- read.csv("./output/node_distances_intervention_walk.csv")

baseline.walk.scores <- calculateAccessibilityScores(baseline.walk, mode = "walk")
intervention.walk.scores <- calculateAccessibilityScores(intervention.walk, mode = "walk")

write.csv(baseline.walk.scores %>%
            left_join(intervention.walk.scores, 
                      by = "node_id",
                      suffix = c("_base", "int")),
          "./output/dwel accessibility scores walk.csv", row.names = FALSE)

#cycle
baseline.cycle <- read.csv("./output/node_distances_baseline_cycle.csv")
intervention.cycle <- read.csv("./output/node_distances_intervention_cycle.csv")

baseline.cycle.scores <- calculateAccessibilityScores(baseline.cycle, mode = "cycle")
intervention.cycle.scores <- calculateAccessibilityScores(intervention.cycle, mode = "cycle")

write.csv(baseline.cycle.scores %>%
            left_join(intervention.cycle.scores, 
                      by = "node_id",
                      suffix = c("_base", "int")),
          "./output/dwel accessibility scores cycle.csv", row.names = FALSE)


## 2.3 Aggregate scores for LGAs ----
## ------------------------------------#
addresses.with.LGA <- residential.addresses %>%
  st_join(., LGAs %>% dplyr::select(NAME), .predicate = st_intersects) %>%
  mutate(LGA = case_when(NAME == "MERRI-BEK" ~ "Merri-bek",
                         TRUE ~ str_to_title(NAME))) %>%
  st_drop_geometry()

# walk
address.scores.walk <- addresses.with.LGA %>%
  # join the scores
  left_join(baseline.walk.scores, 
            by = c("address.n.node" = "node_id")) %>%
  left_join(intervention.walk.scores, 
            by = c("address.n.node" = "node_id"),
            suffix = c("_base", "_int"))

LGA.scores.walk <- calculateLgaAccessibilityScores(address.scores.walk)

# save output
write.csv(LGA.scores.walk, "./output/LGA accessibility scores walk.csv", row.names = F)

# cycle
address.scores.cycle <- addresses.with.LGA %>%
  # join the scores
  left_join(baseline.cycle.scores, 
            by = c("cycle.node" = "node_id")) %>%
  left_join(intervention.cycle.scores, 
            by = c("cycle.node" = "node_id"),
            suffix = c("_base", "_int"))

LGA.scores.cycle <- calculateLgaAccessibilityScores(address.scores.cycle)

# save output
write.csv(LGA.scores.cycle, "./output/LGA accessibility scores cycle.csv", row.names = F)


#  3 Underutilisation analysis ----
# -----------------------------------------------------------------------------#
## 3.1 Dwellings served by new destinations ----
## ------------------------------------#
# find the number of dwellings in the 800m/400m catchment of each of the
# new destination locations

if (find.destination.dwelling.distances) {
  intervention.destinations.with.dwellings <-    
    destinationDwellings(intervention.destinations,
                         residential.addresses,
                         network.nodes.walk,
                         network.links.walk, 
                         PROJECT.CRS)
  
  st_write(intervention.destinations.with.dwellings,
           "./output/intervention_destinations_with_dwellings.sqlite")
} else {
  intervention.destinations.with.dwellings <- 
    st_read("./output/intervention_destinations_with_dwellings.sqlite")
}


## 3.2 Population and dwelling requiements ----
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

# convert to dwelling requirements
# average number of people per household for Greater Melbourne GCCSA in 2021 census:
# https://www.abs.gov.au/census/find-census-data/quickstats/2021/2GMEL
dwel.reqts <- pop.reqts %>%
  mutate(dwel_reqt = round(pop_reqt / 2.6))


## 3.3 Utilisation for new destinations ----
## ------------------------------------#
# for each new destination, dwellings served / dwelling requirement
dwel.utilisation <- intervention.destinations.with.dwellings %>%
  left_join(dwel.reqts, by = "dest_type") %>%
  mutate(utilisation = dwel_served / dwel_reqt)



## 3.4 Calculate average for each destination type and LGA ----
## ------------------------------------#
dwel.LGA <- dwel.utilisation %>%
  # convert polygons to centroids
  st_centroid() %>%
  # intersect with LGAs %>%
  st_join(., LGAs %>% dplyr::select(NAME), .predicate = st_intersects) %>%
  mutate(LGA = case_when(NAME == "MERRI-BEK" ~ "Merri-bek",
                          TRUE ~ str_to_title(NAME))) %>%
  st_drop_geometry() %>%
  # find LGA average utilisation for each destination type
  group_by(LGA, dest_type) %>%
  summarise(mean_util = mean(utilisation)) %>%
  ungroup() %>%
  # arrange for display
  pivot_wider(names_from = dest_type,
              values_from = mean_util) %>%
  # filter to the main 31 Greater Melbourne LGAs
  filter(LGA %in% c("Banyule", "Bayside", "Boroondara", "Brimbank", "Cardinia",
                    "Casey", "Darebin", "Frankston", "Glen Eira", "Greater Dandenong",
                    "Hobsons Bay", "Hume", "Kingston", "Knox", "Manningham",
                    "Maribyrnong", "Maroondah", "Melbourne", "Melton", "Merri-bek",
                    "Monash", "Moonee Valley", "Mornington Peninsula", "Nillumbik", "Port Phillip",
                    "Stonnington", "Whitehorse", "Whittlesea", "Wyndham", "Yarra",
                    "Yarra Ranges")) %>%
  # arrange output columns in desired order
  dplyr::select(LGA, supermarket, convenience_store, cafe, pharmacy, post,
                gp, maternal_child_health, dentist, childcare, kindergarten,
                primary, community_centre, park, bus)

 
# write output
write.csv(dwel.LGA, "./output/underutilisation LGA.csv", row.names = FALSE)
