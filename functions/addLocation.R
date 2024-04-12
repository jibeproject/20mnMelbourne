# function to add a new destination location to an AC to help it meet test of 
# 80% of residences within required distance

# new destination is added at the location in the AC (core or periphery,
# depending on the destination type) which maximises the number of failed 
# addresses that are now reachable within the required distance (and, if several 
# nodes reach the same maximum number, select the one which minimises the sum of 
# the distances for all failed addresses)

addLocation <- function(failed.addresses,
                        AC,
                        destination.type,
                        network.nodes,
                        network.links,
                        buffered.links,
                        g,
                        required.dist,
                        mode) {
  
  
  # exit if mode not correctly set
  if (!mode %in% c("people", "dwellings")) {
    print(paste0("Not configured for mode ", mode, "; terminating"))
    return()
  }
  
  # set up addresses and candidate nodes
  # -----------------------------------#
  
  # unique AC address network nodes 
  address.nodes <- unique(failed.addresses$walk_node)
  
  # network nodes that are potential destination locations
  # vector to hold outputs
  candidate.nodes <- c()
  
  # allocate candidate nodes according to whether core or periphery
  # core - core 800m destinations; and core 400m destinations in small ACs 
  
  # if core - nodes of links within 30m of AC
  if (destination.type %in% c("supermarket", "pharmacy", "post") |
      (destination.type %in% c("convenience_store", "restaurant_cafe") & 
       AC$size == "small")) { 
    
    candidate.links <- network.links %>%
      st_filter(st_buffer(AC, 30), .predicate = st_intersects)
    
    candidate.nodes <- c(candidate.links$from_id, candidate.links$to_id) %>%
      unique()
  } 
  
  # if not core, or if core but no links within 30m - any nodes within 
  # required distance of failed addresses [note that may include the core, and 
  # so a 'periphery' destination could be in the core if that is the best way
  # to satisfy the maximum number of failed addresses]
  if (length(candidate.nodes) == 0) {
    
    search.area <- st_buffer(failed.addresses, required.dist[1]) %>% 
      summarise()
    
    candidate.nodes <- network.nodes %>%
      st_intersection(search.area) %>%
      .$id
  }
  
  # report progress
  print(paste(Sys.time(), "| > Looking for", destination.type, "location for",
              nrow(failed.addresses), "addresses for centre no", AC$CENTRE_NO))

# sample plot - adjust XX wherever appears
# outputXX <- ggplot() +
#   geom_sf(data = AC.addresses, colour = "blue") +
#   geom_sf(data = failed.addresses, colour = "red") +
#   geom_sf(data = destination.locations %>%
#             st_filter(AC.addresses %>% st_buffer(., 400), .predicate = st_intersects),
#           colour = "black", shape = 15, size = 4) +
#   geom_sf(data = new.locations, colour = "black", shape = 1, size = 10, stroke = 2, fill = NA) +
#   theme_minimal() +  # Use a minimal theme
#   theme(panel.grid = element_blank(),     # Remove gridlines
#         axis.title = element_blank(),    # Remove axis titles
#         axis.text = element_blank(),     # Remove axis text
#         panel.border = element_blank(),  # Remove panel border
#         plot.margin = margin(0, 0, 0, 0)) # Remove plot margin

# ggsave("./images/outputsketchXX.png", plot = outputXX, width = 20, height = 20, units = "cm")
# 


  # find new location
  # -----------------------------------#
  
  # distances between failed address nodes and candidate nodes
  address.node.dist <- distances(g,
                         as.character(address.nodes),
                         as.character(candidate.nodes))
  
  # expand table to cover all addresses
  address.dist <- address.node.dist %>%
    as.data.frame() %>%
    # address node column
    mutate(walk_node = as.numeric(row.names(.))) %>%
    relocate(walk_node) %>%
    # join addresses
    full_join(failed.addresses %>% st_drop_geometry(), 
              by = "walk_node") %>%
    relocate(id)
  
  # add unit column depending on mode
  if (mode == "people") {
    address.dist <- address.dist %>%
      mutate(unit = pop_wt) %>%
      relocate(unit)
  } else if (mode == "dwellings") {
    address.dist <- address.dist %>%
      mutate(unit = dwel_wt) %>%
      relocate(unit)
  }

  # number of units (people) within required distance for each candidate node
  candidate.unit.no <- data.frame(candidate.node = as.numeric(),
                                  no.of.units = as.numeric())
  for (i in 1:length(candidate.nodes)) {
    candidate.node <- as.character(candidate.nodes[i])
    # find the addresses within the required distance
    addresses.within.distance <- address.dist %>%
      # select the unit column, and the column with the distances for the candidate node
      dplyr::select(id, unit, all_of(candidate.node)) %>%
      # rename the candidate node column as 'distance'
      setNames(c("id", "unit", "distance")) %>%
      # filter to distance within required distance
      filter(distance <= required.dist[1], na.rm = TRUE)
    
    # add the number of units for the candidate node to the table
    candidate.unit.no <- rbind(
      candidate.unit.no,
      cbind(candidate.node = as.numeric(candidate.node),
            no.of.units = sum(addresses.within.distance$unit))
    )
  }

  # nodes with the maximum number of units
  max.units <- max(candidate.unit.no$no.of.units)
  

  # loop to apply when core nodes used and none can find more addresses
  # [in principle, this could also arise for periphery nodes where all
  # are more than required distance from failed addresses, for example
  # along a single long highway - so far not encountered]
  # -----------------------------------#
  if (max.units == 0) {
    # use periphery nodes, and re-find max.addresses
    search.area <- st_buffer(failed.addresses, required.dist[1]) %>% 
      summarise()
    
    candidate.nodes <- network.nodes %>%
      st_intersection(search.area) %>%
      .$id
    
    # distances between failed address nodes and candidate nodes
    address.node.dist <- distances(g,
                                   as.character(address.nodes),
                                   as.character(candidate.nodes))
    
    # expand table to cover all addresses
    address.dist <- address.node.dist %>%
      as.data.frame() %>%
      # address node column
      mutate(walk_node = as.numeric(row.names(.))) %>%
      relocate(walk_node) %>%
      # join addresses
      full_join(failed.addresses %>% st_drop_geometry(), 
                by = "walk_node") %>%
      relocate(id)
    
    # add unit column depending on mode
    if (mode == "people") {
      address.dist <- address.dist %>%
        mutate(unit = pop_wt) %>%
        relocate(unit)
    } else if (mode == "dwellings") {
      address.dist <- address.dist %>%
        mutate(unit = dwel_wt) %>%
        relocate(unit)
    }
    
    # number of units (people) within required distance for each candidate node
    candidate.unit.no <- data.frame(candidate.node = as.numeric(),
                                    no.of.units = as.numeric())
    for (i in 1:length(candidate.nodes)) {
      candidate.node <- as.character(candidate.nodes[i])
      # find the addresses within the required distance
      addresses.within.distance <- address.dist %>%
        # select the unit column, and the column with the distances for the candidate node
        dplyr::select(id, unit, all_of(candidate.node)) %>%
        # rename the candidate node column as 'distance'
        setNames(c("id", "unit", "distance")) %>%
        # filter to distance within required distance
        filter(distance <= required.dist[1], na.rm = TRUE)
      
      # add the number of units for the candidate node to the table
      candidate.unit.no <- rbind(
        candidate.unit.no,
        cbind(candidate.node = as.numeric(candidate.node),
              no.of.units = sum(addresses.within.distance$unit))
      )
    }
    
    # nodes with the maximum number of units
    max.units <- max(candidate.unit.no$no.of.units)
    
  }
  
  # best nodes are those which find the max number of addresses
  best.candidate.nodes <- candidate.unit.no %>%
    filter(no.of.units == max.units) %>%
    .$candidate.node
  
  # find the winning node, which will be used for the new location
  if (length(best.candidate.nodes) == 1) {
    
    # if only one best candidate node, that is the winner
    winning.node <- best.candidate.nodes
  
  } else {
    
    # otherwise, find the one that minimises the distances for all failed units (people)
    distance.sums <- apply(address.dist %>%
                             # unit and best candidate node columns
                             dplyr::select(unit, as.character(best.candidate.nodes)) %>%
                             # weight best candidate node columns by unit, and remove unit column
                             mutate(across(as.character(best.candidate.nodes), ~ .x * unit)) %>%
                             dplyr::select(-unit),
                           2,
                           sum, na.rm = TRUE) %>%
      as.data.frame() %>%
      cbind(candidate.node = as.numeric(row.names(.))) %>%
      rename("distance.sum" = ".")
    
    # winner is the one with the lowest distance sum
    winning.node <- distance.sums %>%
      filter(distance.sum == min(distance.sum)) %>%
      .$candidate.node
    
    # if there is still more than one (equal distance), pick the first
    if (length(winning.node) > 1) {
      winning.node <- winning.node[1]
    }
    
  }
  
  # report progress
  print(paste(Sys.time(), "| > Found", destination.type, "location for",
              round(max.units), mode, "for centre no", AC$CENTRE_NO))
  
  # construct new location
  # -----------------------------------#
  
  new.location <- network.nodes %>%
    filter(id == winning.node) %>%
    # ensure geometry column name is consistent
    st_set_geometry("GEOMETRY") %>%
    # add centre no and destination type columns (and drop the rest)
    mutate(centre_no = AC$CENTRE_NO,
           dest_type = case_when(
             destination.type == "restaurant_cafe" ~ "cafe",
             destination.type == "community_centre_library" ~ "community_centre",
             TRUE ~ destination.type
           )) %>%
    dplyr::select(centre_no, dest_type)
  
  # if park, buffer to create polygon
  if (destination.type == "park") {
    
    # buffer to 0.4ha - lower bound of 'local park' in 
    # see https://auo.org.au/portal/metadata/access-to-areas-of-public-open-space/,
    # and see K. Villanueva, H. Badland, P. Hooper, M. J. Koohsari, S. Mavoa, M. Davern, et al.
    # Developing indicators of public open space to promote health and wellbeing in communities,
    # Applied Geography 2015 Vol. 57 Pages 112-119, Table 1 esp comments at item 8
    
    new.location <- st_buffer(new.location,  sqrt(4000 / pi))
  
  }
  
  # if park, find entry nodes for new location (otherwise,
  # entry nodes are not required so are set to empty vector)
  if (destination.type == "park") {
    new.entry.nodes <- findEntryNodes(destination.type,
                                      new.location,
                                      network.nodes,
                                      buffered.links)
    
  } else {
    new.entry.nodes <- c()
  }

    
  # return outputs - new location and new entry nodes
  # -----------------------------------#
  
  return(list(new.location, new.entry.nodes))

}


