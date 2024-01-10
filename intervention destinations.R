# add destinations to NACs for 20 minute neighbourhood intervention

#------------------------------------------------------------------------------#
# Process ----
# •	

#------------------------------------------------------------------------------#
# 1 Setup ----
#------------------------------------------------------------------------------#
## 1.1 Libraries ----
## ------------------------------------#
library(dplyr)
library(fs)
library(sf)
library(igraph)

library(ggplot2)  # testing only


## 1.2 Functions ----
## ------------------------------------#
dir_walk(path="./functions/",source, recurse=T, type = "file")


## 1.3 Parameters ----
## ------------------------------------#
PROJECT.CRS <- 28355
BUFFDIST.SMALL <- 400  # distance to buffer small NACs
BUFFDIST.MED.LARGE <- 800  # distance to buffer medium and large NACs


## 1.4 Data ----
## ------------------------------------#

# Note that much of the data loading is (and needs to be) the same as in 
# baseline.R

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


# NAC catchment addresses
nac.catchment.address.location <- "./output/nac_catchment_addresses.rds"
nac.catchment.addresses <- readRDS(nac.catchment.address.location)


# baseline NAC coverage
baseline.NAC.coverage <- read.csv("./output/20mn baseline NAC coverage.csv")


# destinations
POIs.location <- "../data/processed/Weighted POIs/poi.gpkg"
ANLS.pos.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"
ANLS.dest.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"
temp_osm_2023.location <- "../data/processed/temp_osm_2023.sqlite"
community.centre.location <- "../data/processed/community_centre.sqlite"
community.health.location <- "../data/processed/community_health.sqlite"

region_buffer <- st_read("../data/processed/region_buffer.sqlite")

# load baseline destinations - a list containing (1) a vector of 'destination 
# types', and (2) a dataframe (sf object) for each destination type, based on 
# input files 'POIs', 'ANLS.pos', 'ANLS.dest' etc
baseline.destinations <- loadBaselineDestinations(POIs.location, 
                                                  ANLS.pos.location,
                                                  temp_osm_2023.location,
                                                  community.centre.location,
                                                  community.health.location,
                                                  region_buffer)

destination.types <- baseline.destinations[[1]]


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


# graph for finding distances (undirected as used for walking)
g.links <- network.links %>%
  st_drop_geometry() %>%
  mutate(weight = length) %>%
  dplyr::select(from_id, to_id, id, weight)

g <- graph_from_data_frame(g.links, directed = F)

# residential addresses
residential.address.location <- "./output/residential_addresses.sqlite"
residential.addresses <- st_read(residential.address.location) %>%
  mutate(address.n.node = network.nodes$id[st_nearest_feature(., network.nodes)])


# intervention location
intervention.location <- "./output/intervention locations.sqlite"


# 2 Add new destination locations ----
# -----------------------------------------------------------------------------#

for (i in 1:length(destination.types)) {
# for (i in 2:4) { ## bus, tram, train
# for (i in 13:15) {  ## community_centre, childcare, kindergarten
  
  # set up destination type and failed NACs
  # -----------------------------------#
  # destination type
  destination.type <- destination.types[i]

  # skip tram and train (as they are covered in bus)
  if (destination.type %in% c("tram", "train")) {
    next
  }

  # baseline locations for that type (using the index number of the type from 
  # 'destination.types', plus 1)
  baseline.locations <- 
    baseline.destinations[[which(destination.types == destination.type) + 1]] %>%
    # rename geometry column, to ensure consistency with new locations
    st_set_geometry("GEOMETRY")
  
  # add extra baseline locations where they count towards satisfying the test
  # note that the identifying mode field is 'dest_type', which matches the 
  # identifying field that will be added to 'new.locations'
  if (destination.type == "bus") {  
    baseline.locations <- 
      bind_rows(baseline.locations %>%
                  mutate(dest_type = "bus"),
                # plus baseline train and tram
                baseline.destinations[[which(destination.types == "tram") + 1]] %>%
                  st_set_geometry("GEOMETRY") %>%
                  mutate(dest_type = "tram"),
                baseline.destinations[[which(destination.types == "train") + 1]] %>%
                  st_set_geometry("GEOMETRY") %>%
                  mutate(dest_type = "train")) %>%
      dplyr::select(dest_type)
  } else if (destination.type == "convenience_store") {
    baseline.locations <- 
      bind_rows(baseline.locations, 
                # plus baseline supermarkets
                baseline.destinations[[which(destination.types == "supermarket") + 1]] %>%
                  st_set_geometry("GEOMETRY"),
                # plus intervention supermarkets
                st_read(intervention.location, layer = "supermarket"))
  } else if (destination.type == "park") {  # FOR NOW - BUT MAYBE park BEFORE district_sport, AND THEN JUST FIND park LOCATIONS FOR dist_sport, NOT NEW
    baseline.locations <-
      baseline.locations <- 
      bind_rows(baseline.locations, 
                # plus intervention district sport (not baseline district sport, because they are already 'parks')
                st_read(intervention.location, layer = "district_sport"))
  } 
  
  # field for the relevant destination
  destination.field <- case_when(
    destination.type == "restaurant_cafe" ~ "rest.cafe.400",
    destination.type == "bus" ~ "bus.400.tram.600.train.800",  ## CONSIDER PT FURTHER
    destination.type == "supermarket"  ~ "supermarket.800",
    destination.type == "convenience_store" ~ "convenience.400",
    destination.type == "butcher" ~ "butcher.800",
    destination.type == "bakery" ~ "bakery.800",
    destination.type == "pharmacy" ~ "pharmacy.800",
    destination.type == "post" ~ "post.800",
    destination.type == "district_sport" ~ "distsport.800",
    destination.type == "park" ~ "park.400",
    destination.type == "community_centre" ~ "comm.ctr.800",
    destination.type == "childcare" ~ "childcare.800",
    destination.type == "kindergarten" ~ "kindergarten.800",
    destination.type == "primary" ~ "primary.800",
    destination.type == "community_health" ~ "comm.health.800",
    destination.type == "maternal_child_health" ~ "mat.child.health.800",
    destination.type == "gp" ~ "gp.400",
    destination.type == "dentist" ~ "dentist.800"
  )
  
  # required distance for 80% of addresses
  required.dist <- case_when(
    destination.type %in% c("restaurant_cafe", "bus", "convenience_store",
                            "park", "gp")  ~ 400,
    destination.type == "bus" ~ c(400, 600, 800), 
    TRUE     ~ 800,
  )
  
  # find NACs that failed the 80% test in baseline
  failed.NACs <- baseline.NAC.coverage %>%
    dplyr::select(centre_no, size, !!destination.field) %>%
    filter(get(destination.field) < 80)

  # order failed NACs  - small to large for 400m walk dist, or large to small
  # for 800; then order by  neediest (lowest percentage; if equality then highest 
  # number of addresses)
  failed.NACs.with.details <- failed.NACs %>%
    # add number of addresses
    left_join(nac.catchment.addresses, by = c("centre_no" = "CENTRE_NO")) %>%
    rowwise() %>%
    mutate(no.addresses = length(unlist(address_ids))) %>%
    ungroup() %>%
    # remove the address_ids field (used to calculate the number of addresses)
    dplyr::select(-address_ids) %>%
    mutate(size = factor(size, levels = c("small", "medium", "large")))
  
  if (destination.type %in% c("restaurant_cafe", "bus", "convenience_store",
                              "park", "gp")) {
    failed.NACs.ordered <- failed.NACs.with.details %>%
      # small to large, then neediest
      arrange(size, get(destination.field), desc(no.addresses))
  }  else {
    failed.NACs.ordered <- failed.NACs.with.details %>%
      # large to small, then neediest
      arrange(desc(size), get(destination.field), desc(no.addresses))
  }
  
  # for district sport and park (polygons), find entry nodes for baseline
  # locations (see findEntryNodes.R for details)
  if (destination.type %in% c("district_sport", "park")) {
    entry.nodes <- findEntryNodes(destination.type,
                                  baseline.locations,
                                  network.nodes,
                                  network.links)
  } else {
    entry.nodes <- c()
  }


  # loop through failed NACs and add locations as required
  # ---------------------------------#
  
  # report progress
  print(paste(Sys.time(), "| Finding new", destination.type, "locations for",
              nrow(failed.NACs), "neighbourhood activity centres"))
  
  # empty vector to hold new destinations that are added in the loop
  new.locations <- c()
  
  # loop to test and add locations
  for (j in 1:nrow(failed.NACs.ordered)) {
    
    # set up destinations and NAC 
    # ---------------------------------#
    
    # destinations, including any new
    destination.locations <- bind_rows(baseline.locations,
                                       new.locations)
    
    # failed nac (centre no and size, not the geometry)
    failed.NAC <- failed.NACs.ordered[j, ]
    
    # NAC (with geometry)
    NAC <- NACs %>%
      filter(CENTRE_NO == failed.NAC$centre_no)
    
    # residential addresses for the NAC
    NAC.address.ids <- nac.catchment.addresses %>%
      filter(CENTRE_NO == failed.NAC$centre_no) %>%
      .$address_ids %>%
      unlist()
    
    NAC.addresses <- residential.addresses %>%
      filter(id %in% NAC.address.ids)
    
    
    # initial test 
    # -------------------------------#
    
    # test that it's still failed (could be fixed by previous new locations)
    test.outputs <- testFailedNac(NAC.addresses,
                                  destination.type,
                                  destination.locations,
                                  network.nodes,
                                  network.links,
                                  g,
                                  required.dist,
                                  entry.nodes)
    
    test.result <- test.outputs[[1]]
    failed.addresses <- test.outputs[[2]]
    
    # report progress
    if (test.result) {
      print(paste0(Sys.time(), " | Test passed for centre no ", failed.NAC$centre_no, 
                  " (", j, " of ", nrow(failed.NACs.ordered), ")"))
    } else {
      print(paste0(Sys.time(), " | Test failed for centre no ", failed.NAC$centre_no, 
                   " (", j, " of ", nrow(failed.NACs.ordered),
                   "): needs new ", destination.type, "(s)"))
    }
    
    
    # loop to add and re-test 
    # -------------------------------#
    
    # NOTE - supermarkets don't really test the loop, because at 800m it always
    # satisfies the test first time
    
    # if failed, then keep adding locations until the test is passed
    while (!test.result) {
      
      # add a new location
      new.location.outputs <- addLocation(failed.addresses,
                                          NAC,
                                          destination.type,
                                          network.nodes,
                                          network.links,
                                          g,
                                          required.dist)
      new.location <- new.location.outputs[[1]]
      new.entry.nodes <- new.location.outputs[[2]]
      
      # add the new location both to both new.locations (the overall
      # list of new locations for the destination type) and
      # destination.locations (used for re-testing for the specific NAC)
      new.locations <- bind_rows(new.locations, new.location)
      destination.locations <- bind_rows(destination.locations, new.location)
      
      # add the new entry nodes to the entry nodes (only relevant for 
      # district sport and park)
      entry.nodes <- unique(c(entry.nodes, new.entry.nodes))
      
      # and test again
      test.outputs <- testFailedNac(NAC.addresses,
                                    destination.type,
                                    destination.locations,
                                    network.nodes,
                                    network.links,
                                    g,
                                    required.dist,
                                    entry.nodes)
      
      test.result <- test.outputs[[1]]
      failed.addresses <- test.outputs[[2]]
      
    }
  }
  
  # write output
  st_write(new.locations, 
           intervention.location, layer = destination.type,
           delete_layer = TRUE)

}

