# add destinations to ACs for 20 minute neighbourhood intervention

#------------------------------------------------------------------------------#
# Process ----
# •	For each destination type, identify the ACs that failed the test.
#   o	‘Fail the test’ means that 80% of people in an AC are not within the 
#     required walking distance (400m, 600m or 800m as applicable) of the 
#     relevant destination type.
#
# •	Order the failed ACs.  
#   o	For destinations where 400m walking distance is required (restaurant_cafe, 
#     bus, convenience_store, park, gp), order small to large: a large AC 
#     should be made up of small ACs, each of which contains these, so fill out 
#     the small ACs first.  For the 800m destinations, order large to small: a 
#     large AC should contain these, so they are accessible to small ACs within it.  
#   o	Within each size category, order by neediest first, with the aim of 
#     maximising the possibility of a new destination also helping other ACs 
#     pass the test. ‘Neediest’ is lowest percentage of addresses within the 
#     required distance and, in case of equality, highest number of failed addresses
#   o	In order to assess sensitivity to order, can run as 'small first' and 
#     'large first' for all destination types, and then assemble the desired mix
#     from their outputs (instead of running again as 'mixed').
#
# •	For each ‘failed AC’:
#   o	Test whether the AC now passes the test, taking account of any new 
#     destinations that may already have been added (functions/testFailedAc.R). 
#   o	If it fails the test, identify the ‘failed addresses’ that are not within 
#     the required distance.  Add a new location at the node which maximises the 
#     number of people that are now reachable within the required distance 
#     and, if several nodes reach the same maximum number, select the one which 
#     minimises the sum of the distances (weighted by the number of people for
#     each address) for all failed addresses (functions/addLocation.R).  
#     Re-test, and continue adding locations until the test is met.
#   o	When searching for the best location node, candidate nodes are:
#     	for supermarket, butcher, bakery, pharmacy or post for all ACs; or 
#       for restaurant_cafe or convenience_store for small ACs only – nodes 
#       on links that are within 30m of the AC (ie the core); or
#     	otherwise – nodes within 400m (restaurant_cafe, bus, convenience_store, 
#       park) or 800m (otherwise) of the failed addresses.
#   o	Include any added locations in the dataframe of new destinations, which 
#     are used when re-testing the current AC and testing further ACs. 
#
# • Save output locations: 'output/intervention locations.sqlite': along with
#   tables for each of the alternative orders 'output/intervention locations small first.sqlite'
#   and 'output/intervention locations large first.sqlite'
#
# • Build output tables, saved as 'output/intervention tables.xlsx':
#   o	'added destinations' (section 3.1): number of added destinations of each 
#     type, joined to baseline results for numbers of ACs meeting, or not meeting,
#     the 80% target for each destination type
#   o	'order comparison' (section 3.2): numbers of added destinations under 
#     each of the 'small first' and 'large first' approaches
#
#------------------------------------------------------------------------------#


# 1 Setup ----
#------------------------------------------------------------------------------#
## 1.1 Libraries ----
## ------------------------------------#
library(dplyr)
library(fs)
library(sf)
library(igraph)
library(openxlsx)

# library(ggplot2)  # testing only


## 1.2 Functions ----
## ------------------------------------#
dir_walk(path = "./functions/", source, recurse = T, type = "file")


## 1.3 Parameters ----
## ------------------------------------#
PROJECT.CRS <- 28355
BUFFDIST.SMALL <- 400  # distance to buffer small ACs
BUFFDIST.MED.LARGE <- 800  # distance to buffer medium and large ACs

# ordering parameter - select one
# processing.order <- "neediest-first"
# processing.order <- "least-needy-first"
processing.order <- "small-first"
# processing.order <- "large-first"


## 1.4 Data ----
## ------------------------------------#

# Note that much of the data loading is (and needs to be) the same as in 
# baseline.R

# activity centres
ACs <- read_zipped_GIS(zipfile = "../data/original/MICLUP-NACs.zip",
                        file = "/MICLUP_COMMERCIAL_EXT_JUN2020.shp") %>%
  st_transform(PROJECT.CRS) %>%
  mutate(size = case_when(
    CENTRESIZE %in% c("Less than 2000", "2000 to 5000") ~ "small",
    CENTRESIZE == "5000 to 10000"                       ~ "medium",
    TRUE                                                ~ "large")
  ) %>%
  dplyr::select(-Shape_Leng, -Shape_Area)


# AC catchment addresses
ac.catchment.address.location <- "./output/ac_catchment_addresses.rds"
ac.catchment.addresses <- readRDS(ac.catchment.address.location)


# baseline AC coverage (note that this is % of people (not dwellings) within 
# the required walking distance of each dwelling type)
baseline.output.location <- "./output/baseline assessment.xlsx"
baseline.AC.coverage <- read.xlsx(baseline.output.location, 
                                  sheet = "AC coverage pop")

# destinations
POIs.location <- "../data/processed/Destinations weights/Baseline/poi_weight.gpkg"
ANLS.pos.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"
ANLS.dest.location <- 
  "../data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg"

region_buffer <- st_read("../data/processed/region_buffer.sqlite")

# load baseline destinations - a list containing (1) a vector of 'destination 
# types', and (2) a dataframe (sf object) for each destination type, based on 
# input files 'POIs', 'ANLS.pos', 'ANLS.dest' etc
baseline.destinations <- loadBaselineDestinations(POIs.location, 
                                                  ANLS.dest.location,
                                                  ANLS.pos.location,
                                                  region_buffer,
                                                  PROJECT.CRS)

destination.types <- baseline.destinations[[1]]


# load  network, and filter to region buffer
links <- st_read("../data/processed/melbourneClipped_edges.sqlite") %>%
  st_filter(region_buffer, .predicate = st_intersects) %>%
  # filter to walkable only
  filter(is_walk == TRUE)

nodes <- st_read("../data/processed/melbourneClipped_nodes.sqlite") %>%
  # only those used in links
  filter(id %in% links$from_id | id %in% links$to_id)

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
residential.addresses <- st_read(residential.address.location)

# intervention locations (sqlites are for location of new destinations; xlsx
# is for output tables)
intervention.location.neediest.first <- "./output/intervention locations neediest first.sqlite"
intervention.location.least.needy.first <- "./output/intervention locations least needy first.sqlite"
intervention.location.small.first <- "./output/intervention locations small first.sqlite"
intervention.location.large.first <- "./output/intervention locations large first.sqlite"
intervention.location.final <- "./output/intervention locations.sqlite"
intervention.tables.location <- "./output/intervention tables.xlsx"

# 2 Add new destination locations ----
# -----------------------------------------------------------------------------#
## 2.1 Add locations ----
## -----------------------------------------------------------------------------#
# Order of processing depends on the 'processing.order' parameter.  See section
# 2.2 for options to assemble a mixed output.

for (i in 1:length(destination.types)) {
# for (i in 2:4) { 
# for (i in c(1, 14, 13)) {
  
  # set up intervention location (to write results)
  # -----------------------------------#
  if (processing.order == "neediest-first") {
    intervention.location <- intervention.location.neediest.first
  } else if (processing.order == "least-needy-first") {
    intervention.location <- intervention.location.least.needy.first
  } else if (processing.order == "small-first") {
    intervention.location <- intervention.location.small.first
  } else if (processing.order == "large-first") {
    intervention.location <- intervention.location.large.first
  } else {
    print("The 'processing.order' parameter must be set as 'neediest-first', 'least-needy-first', 'small-first' or 'large-first'; terminating")
    return()
  }
  
  # set up destination type and failed ACs
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
  }
  
  # field for the relevant destination
  destination.field <- case_when(
    destination.type == "supermarket"  ~ "supermarket.800",
    destination.type == "convenience_store" ~ "convenience.400",
    destination.type == "restaurant_cafe" ~ "rest.cafe.400",
    destination.type == "pharmacy" ~ "pharmacy.800",
    destination.type == "post" ~ "post.800",
    destination.type == "gp" ~ "gp.800",
    destination.type == "maternal_child_health" ~ "mat.child.health.800",
    destination.type == "dentist" ~ "dentist.800",
    destination.type == "childcare" ~ "childcare.800",
    destination.type == "kindergarten" ~ "kindergarten.800",
    destination.type == "primary" ~ "primary.800",
    destination.type == "community_centre_library" ~ "comm.library.800",
    destination.type == "park" ~ "park.400",
    destination.type == "bus" ~ "bus.400.tram.600.train.800"
  )
  
  # required distance for 80% of people
  if (destination.type %in% c("convenience_store", "restaurant_cafe", "park")) {
    required.dist <- 400
  } else if (destination.type == "bus") {
    required.dist <- c(400, 600, 800)
  } else {
    required.dist <- 800
  }

  # find ACs that failed the 80% test in baseline
  failed.ACs <- baseline.AC.coverage %>%
    dplyr::select(centre_no, size, !!destination.field) %>%
    filter(get(destination.field) < 80)

  # order failed ACs  - small to large for 400m walk dist, or large to small
  # for 800; then order by  neediest (lowest percentage; if equality then highest 
  # number of addresses)
  failed.ACs.with.details <- failed.ACs %>%
    # add number of addresses
    left_join(ac.catchment.addresses, by = c("centre_no" = "CENTRE_NO")) %>%
    rowwise() %>%
    mutate(no.addresses = length(unlist(address_ids))) %>%
    ungroup() %>%
    # remove the address_ids field (used to calculate the number of addresses)
    dplyr::select(-address_ids) %>%
    mutate(size = factor(size, levels = c("small", "medium", "large")))
  
  if (processing.order == "neediest-first") {
    failed.ACs.ordered <- failed.ACs.with.details %>%
      arrange(get(destination.field), desc(no.addresses))
  } else if (processing.order == "least-needy-first") {
    failed.ACs.ordered <- failed.ACs.with.details %>%
      arrange(desc(get(destination.field)), no.addresses)
  } else if (processing.order == "small-first") {
    failed.ACs.ordered <- failed.ACs.with.details %>%
      # combine ‘large’ and ‘medium’ as large, and convert to factors
      mutate(size_group = ifelse(size == "small", "small", "large"))%>%
      mutate(size_group = factor(size_group, levels = c("small", "large"))) %>%
      # small to large, then neediest
      arrange(size_group, get(destination.field), desc(no.addresses))
  } else if (processing.order == "large-first") {
    failed.ACs.ordered <- failed.ACs.with.details %>%
      # combine ‘large’ and ‘medium’ as large, and convert to factors
      mutate(size_group = ifelse(size == "small", "small", "large"))%>%
      mutate(size_group = factor(size_group, levels = c("small", "large"))) %>%
      # small to large, then neediest
      arrange(dessc(size_group), get(destination.field), desc(no.addresses))
  }  
  
  # for park (polygons), find entry nodes for baseline locations (see findEntryNodes.R for details)
  if (destination.type == "park") {
    buffered.links <- st_buffer(network.links, 30)
    
    entry.nodes <- findEntryNodes(destination.type,
                                  baseline.locations,
                                  network.nodes,
                                  buffered.links)
  } else {
    entry.nodes <- c()
  }


  # loop through failed ACs and add locations as required
  # ---------------------------------#
  
  # report progress
  print(paste(Sys.time(), "| Finding new", destination.type, "locations for",
              nrow(failed.ACs), "activity centres"))
  
  # empty vector to hold new destinations that are added in the loop
  new.locations <- c()
  
  # loop to test and add locations
  for (j in 1:nrow(failed.ACs.ordered)) {
  # for (j in 1:10) {
    
    # set up destinations and AC 
    # ---------------------------------#
    
    # destinations, including any new
    destination.locations <- bind_rows(baseline.locations,
                                       new.locations)
    
    # failed ac (centre no and size, not the geometry)
    failed.AC <- failed.ACs.ordered[j, ]
    
    # AC (with geometry)
    AC <- ACs %>%
      filter(CENTRE_NO == failed.AC$centre_no)
    
    # residential addresses for the AC
    AC.address.ids <- ac.catchment.addresses %>%
      filter(CENTRE_NO == failed.AC$centre_no) %>%
      .$address_ids %>%
      unlist()
    
    AC.addresses <- residential.addresses %>%
      filter(id %in% AC.address.ids)
    
    
    # initial test 
    # -------------------------------#
    
    # test that it's still failed (could be fixed by previous new locations)
    test.outputs <- testFailedAc(AC.addresses,
                                 destination.type,
                                 destination.locations,
                                 network.nodes,
                                 network.links,
                                 g,
                                 required.dist,
                                 entry.nodes,
                                 mode = "people")
    
    test.result <- test.outputs[[1]]
    failed.addresses <- test.outputs[[2]]
    
    # report progress
    if (test.result) {
      print(paste0(Sys.time(), " | Test passed for centre no ", failed.AC$centre_no, 
                  " (", j, " of ", nrow(failed.ACs.ordered), ")"))
    } else {
      print(paste0(Sys.time(), " | Test failed for centre no ", failed.AC$centre_no, 
                   " (", j, " of ", nrow(failed.ACs.ordered),
                   "): needs new ", destination.type, "(s)"))
    }
    
    
    # loop to add and re-test 
    # -------------------------------#
    
    # if failed, then keep adding locations until the test is passed
    while (!test.result) {
      
      # add a new location
      new.location.outputs <- addLocation(failed.addresses,
                                          AC,
                                          destination.type,
                                          network.nodes,
                                          network.links,
                                          buffered.links,
                                          g,
                                          required.dist,
                                          mode = "people")
      new.location <- new.location.outputs[[1]]
      new.entry.nodes <- new.location.outputs[[2]]
      
      # add the new location both to both new.locations (the overall
      # list of new locations for the destination type) and
      # destination.locations (used for re-testing for the specific AC)
      new.locations <- bind_rows(new.locations, new.location)
      destination.locations <- bind_rows(destination.locations, new.location)
      
      # add the new entry nodes to the entry nodes (only relevant for 
      # district sport and park)
      entry.nodes <- unique(c(entry.nodes, new.entry.nodes))
      
      # and test again
      test.outputs <- testFailedAc(AC.addresses,
                                   destination.type,
                                   destination.locations,
                                   network.nodes,
                                   network.links,
                                   g,
                                   required.dist,
                                   entry.nodes,
                                   mode = "people")
      
      test.result <- test.outputs[[1]]
      failed.addresses <- test.outputs[[2]]
      
    }
  }
  
  # write output
  st_write(new.locations, 
           intervention.location, layer = destination.type,
           delete_layer = TRUE)

}


## 2.2 Assemble final locations ----
## -----------------------------------------------------------------------------#
# Option if final locations are being assembled from 'small-first' and 'large-first'
# (adapt if assembled in some other way)

for (i in 1:length(destination.types)) {
  destination.type <- destination.types[i]
  
  # read in the relevant layer
  if (destination.type %in% c("convenience_store", "restaurant_cafe", "park", "bus")) {
    if (destination.type %in% st_layers(intervention.location.small.first)$name) {
      dest.layer <- st_read(intervention.location.small.first, layer = destination.type)
      
      # write to final location
      st_write(dest.layer,
               intervention.location.final, layer = destination.type,
               delete_layer = TRUE)
    }
  }  else {
    if (destination.type %in% st_layers(intervention.location.large.first)$name) {
      dest.layer <- st_read(intervention.location.large.first, layer = destination.type)
      
      # write to final location
      st_write(dest.layer,
               intervention.location.final, layer = destination.type,
               delete_layer = TRUE)
    }
  }
}

# Using neediest-first as final
for (i in 1:length(destination.types)) {
  destination.type <- destination.types[i]
  
  if (destination.type %in% st_layers(intervention.location.neediest.first)$name) {
    # read in the relevant layer
    dest.layer <- st_read(intervention.location.neediest.first, layer = destination.type)
    
    # write to final location
    st_write(dest.layer,
             intervention.location.final, layer = destination.type,
             delete_layer = TRUE)
  }
}

# BUT ALSO CONSIDER doing the comparison table in 3.2, then assembling the final from the lowest?


# 3 Output tables ----
# -----------------------------------------------------------------------------#

## 3.0 Set up output workbook (required for all parts of section 3) ----
## ------------------------------------#
# read in if it exists, or create if not
if (file.exists(intervention.tables.location)) {
  wb <-loadWorkbook(intervention.tables.location)
} else {
  wb <- createWorkbook()
}


## 3.1 Summary table of number of new destination locations ----
## -----------------------------------------------------------------------------#
# read in baseline results (no and % meeting target, and shortfall)
baseline.table <- 
  read.xlsx(baseline.output.location, sheet = "AC coverage summ pop") %>%
  dplyr::select(dest.dist, all.no, all.pct, all.shortfall) %>%
  # add shortfall % column
  mutate(shortfall.pct = 100 - all.pct)

# summarise number of each new destination type
new.location.summary <- locationSummary(intervention.location.final)

# prepare new locations for final table
new.location.summary.tidied <- new.location.summary %>%
  # names corresponding to baseline names
  mutate(dest.dist = case_when(
    dest_type == "supermarket"  ~ "Supermarket",
    dest_type == "pharmacy"     ~ "Pharmacy",
    dest_type == "post"         ~ "Post office",
    dest_type == "gp"           ~ "GP",
    dest_type == "maternal_child_health" ~ "Maternal & child health centre",
    dest_type == "dentist"      ~ "Dentist",
    dest_type == "childcare"    ~ "Childcare centre",
    dest_type == "kindergarten" ~ "Kindergarten",
    dest_type == "primary"      ~ "Primary school",
    dest_type == "community_centre"   ~ "Community centre or library",
    dest_type == "convenience_store"  ~ "Convenience store or supermarket",
    dest_type == "cafe"         ~ "Restaurant or cafe",
    dest_type == "park"         ~ "Local park",
    dest_type == "bus"          ~ "Bus stop, tram stop or train station"
  )) %>%
  dplyr::select(dest.dist, added.dest)

# combine baseline and new location tables
added.destinations <- baseline.table %>%
  left_join(new.location.summary.tidied, by = "dest.dist")

# write output
# add worksheet with required name if not already there
added.dest.name <- "added destinations"

if (!added.dest.name %in% names(wb)) {
  addWorksheet(wb, sheetName = added.dest.name)
}

# write the results to the worksheets
writeData(wb, sheet = added.dest.name, added.destinations)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, intervention.tables.location, overwrite = TRUE)


## 3.2 Comparison table of different approaches ----
## -----------------------------------------------------------------------------#
# summarise numebrs of new destinations for each of small and large first
neediest.first.table <- locationSummary(intervention.location.neediest.first) %>%
  rename(neediest.first = added.dest)
least.needy.first.table <- locationSummary(intervention.location.least.needy.first) %>%
  rename(least.needy.first = added.dest)
small.first.table <- locationSummary(intervention.location.small.first) %>%
  rename(small.first = added.dest)
large.first.table <- locationSummary(intervention.location.large.first) %>%
  rename(large.first = added.dest)

# combine
order.comparison.table <- neediest.first.table %>%
  left_join(least.needy.first.table, by = "dest_type") %>%
  left_join(small.first.table, by = "dest_type") %>%
  left_join(large.first.table, by = "dest_type") %>%
  # order dest_type column for display
  mutate(dest_type = factor(dest_type, 
                            levels = c("supermarket", "pharmacy", "post", "gp",
                                       "maternal_child_health", "dentist", "childcare",
                                       "kindergarten", "primary", "community_centre",
                                       "convenience_store", "cafe", "park", "bus",
                                       "total"))) %>%
  arrange(dest_type)

##  would it be good to add a final column saying which is the lowest?  testing
for (i in 1:nrow(order.comparison.table)) {
  row <- order.comparison.table[i, ] %>% dplyr::select(-dest_type)
  lowest.col <- colnames(row)[which.min(row[1, ])]  # not quite sure about this - do I need the 1?
  order.comparison.table[i, "lowest"] <- lowest.col
}

# write output
# add worksheet with required name if not already there
order.comp.name <- "order comparison"

if (!order.comp.name %in% names(wb)) {
  addWorksheet(wb, sheetName = order.comp.name)
}

# write the results to the worksheets
writeData(wb, sheet = order.comp.name, order.comparison.table)

# write the workbook to  file (will create if new, or else overwrite)
saveWorkbook(wb, intervention.tables.location, overwrite = TRUE)


