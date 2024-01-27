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
                        g,
                        required.dist) {
  
  
  # set up addresses and candidate nodes
  # -----------------------------------#
  
  # unique AC address network nodes 
  address.nodes <- unique(failed.addresses$address.n.node)
  
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
              nrow(failed.addresses), "dwellings for centre no", AC$CENTRE_NO))

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
    mutate(address.n.node = as.numeric(row.names(.))) %>%
    relocate(address.n.node) %>%
    # join addresses
    full_join(failed.addresses %>% st_drop_geometry(), 
              by = "address.n.node") %>%
    relocate(id)
    
  # number of addresses within required distance for each candidate node
  # (columns are candidate nodes, so 2)
  candidate.address.no <- apply(address.dist %>%
                                dplyr::select(-id, -address.n.node), 
                              2, 
                              function(x) sum(x <= required.dist[1], na.rm = TRUE)) %>%
    as.data.frame() %>%
    cbind(candidate.node = as.numeric(row.names(.))) %>%
    rename("no.of.addresses" = ".")
  
  # nodes with the maximum number of addresses
  max.addresses <- max(candidate.address.no$no.of.addresses)
  
  
  # loop to apply when core nodes used and none can find more addresses
  # [in principle, this could also arise for periphery nodes where all
  # are more than required distance from failed addresses, for example
  # along a single long highway - so far not encountered]
  # -----------------------------------#
  if (max.addresses == 0) {
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
      mutate(address.n.node = as.numeric(row.names(.))) %>%
      relocate(address.n.node) %>%
      # join addresses
      full_join(failed.addresses %>% st_drop_geometry(), 
                by = "address.n.node") %>%
      relocate(id)
    
    # number of addresses within required distance for each candidate node
    # (columns are candidate nodes, so 2)
    candidate.address.no <- apply(address.dist %>%
                                    dplyr::select(-id, -address.n.node), 
                                  2, 
                                  function(x) sum(x <= required.dist[1], na.rm = TRUE)) %>%
      as.data.frame() %>%
      cbind(candidate.node = as.numeric(row.names(.))) %>%
      rename("no.of.addresses" = ".")
    
    # nodes with the maximum number of addresses
    max.addresses <- max(candidate.address.no$no.of.addresses)
    
  }
  
  # best nodes are those which find the max number of addresses
  best.candidate.nodes <- candidate.address.no %>%
    filter(no.of.addresses == max.addresses) %>%
    .$candidate.node
  
  # find the winning node, which will be used for the new location
  if (length(best.candidate.nodes) == 1) {
    
    # if only one best candidate node, that is the winner
    winning.node <- best.candidate.nodes
  
  } else {
    
    # otherwise, find the one that minimises the distances for all failed addresses
    distance.sums <- apply(address.dist %>%
                             dplyr::select(as.character(best.candidate.nodes)),
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
              max.addresses, "dwellings for centre no", AC$CENTRE_NO))
  
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
                                      network.links)
    
  } else {
    new.entry.nodes <- c()
  }

    
  # return outputs - new location and new entry nodes
  # -----------------------------------#
  
  return(list(new.location, new.entry.nodes))

}


