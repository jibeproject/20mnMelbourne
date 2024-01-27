# function to test whether AC meets test of 80% of residences within required distance

# function to find distances between destinations and addresses

testFailedAc <- function(AC.addresses,
                         destination.type,
                         destination.locations,
                         network.nodes,
                         network.links,
                         g,
                         required.dist,
                         entry.nodes) {  
  
  # set up addresses and destinations
  # -----------------------------------#
  
  # unique AC address network nodes
  address.nodes <- unique(AC.addresses$address.n.node)   
  
  # nodes representing destinations: separate bus, tram and train for bus;
  # pre-calculated entry nodes for polygons; otherwise nearest nodes to features
  
  if (destination.type == "bus") {
    
    # buffer to required.dist for train (only destinations within that distance 
    # can be reachable from the addresses)
    AC.buffer <- AC.addresses %>%
      st_buffer(max(required.dist)) %>%
      summarise()

    # destinations within the AC buffer
    AC.destinations <- destination.locations %>%
      st_filter(AC.buffer, .predicate = st_intersects)
    
    # nearest node to feature types
    AC.destination.nodes.bus <- 
      network.nodes$id[st_nearest_feature(AC.destinations %>%
                                            filter(dest_type == "bus"),
                                          network.nodes)] %>%
      unique()
    
    AC.destination.nodes.tram <- 
      network.nodes$id[st_nearest_feature(AC.destinations %>%
                                            filter(dest_type == "tram"),
                                          network.nodes)] %>%
      unique()
    
    AC.destination.nodes.train <- 
      network.nodes$id[st_nearest_feature(AC.destinations %>%
                                            filter(dest_type == "train"),
                                          network.nodes)] %>%
      unique()
    
    AC.destination.nodes <- c(AC.destination.nodes.bus,
                              AC.destination.nodes.tram,
                              AC.destination.nodes.train)
    
    
    
  } else if (destination.type == "park") {
    
    # buffer to required.dist
    AC.buffer <- AC.addresses %>%
      st_buffer(required.dist) %>%
      summarise()
    
    # entry nodes within the AC buffer
    AC.destination.nodes <- network.nodes %>%
      filter(id %in% entry.nodes) %>%
      st_intersection(AC.buffer) %>%
      .$id
    
  } else {
    
    # buffer to required.dist
    AC.buffer <- AC.addresses %>%
      st_buffer(required.dist) %>%
      summarise()
    
    # destinations within the AC buffer
    AC.destinations <- destination.locations %>%
      st_filter(AC.buffer, .predicate = st_intersects)
    
    # nearest nodes to features
    AC.destination.nodes <- 
      network.nodes$id[st_nearest_feature(AC.destinations, network.nodes)] %>%
      unique()
    
  }
  
  # ggplot() +
  #   geom_sf(data = AC.buffer) +
  #   geom_sf(data = AC.addresses, colour = "blue") +
  #   geom_sf(data = AC.destinations, colour = "red", size = 2)
  
  
  
  # run the test
  # -----------------------------------#
  
  if (length(AC.destination.nodes) > 0) {
    
    # where some destination locations are present - do they cover 80%?
    # ---------------------------------#
    
    if (destination.type == "bus") {
      
      # for bus - separate distance calculations for each mode
      
      # bus distances, and minimum for each address node
      if (length(AC.destination.nodes.bus) > 0) {
        dest.dist.bus <- distances(g,
                                   as.character(AC.destination.nodes.bus),
                                   as.character(address.nodes))
        
        min.dist.bus <- apply(dest.dist.bus, 2, min, na.rm = TRUE) %>%  
          as.data.frame() %>%
          cbind(id = as.numeric(row.names(.))) %>%
          rename("bus" = ".") 
        
      } else {
        # if no bus nodes, set all distances to 1000 (will fail 800m test)
        min.dist.bus <- data.frame(id = address.nodes,
                                   bus = 1000)
      }
      
      # tram distances, and minimum for each address node
      if (length(AC.destination.nodes.tram) > 0) {
        dest.dist.tram <- distances(g,
                                    as.character(AC.destination.nodes.tram),
                                    as.character(address.nodes))
        
        min.dist.tram <- apply(dest.dist.tram, 2, min, na.rm = TRUE) %>%  
          as.data.frame() %>%
          cbind(id = as.numeric(row.names(.))) %>%
          rename("tram" = ".") 
        
      } else {
        # if no tram nodes, set all distances to 1000 (will fail 800m test)
        min.dist.tram <- data.frame(id = address.nodes,
                                    tram = 1000)
      }
      
      # train distances, and minimum for each address node
      if (length(AC.destination.nodes.train) > 0) {
        dest.dist.train <- distances(g,
                                     as.character(AC.destination.nodes.train),
                                     as.character(address.nodes))
        
        min.dist.train <- apply(dest.dist.train, 2, min, na.rm = TRUE) %>%  
          as.data.frame() %>%
          cbind(id = as.numeric(row.names(.))) %>%
          rename("train" = ".") 
        
      } else {
        # if no train nodes, set all distances to 1000 (will fail 800m test)
        min.dist.train <- data.frame(id = address.nodes,
                                     train = 1000)
      }
      
      # join the 3 modes
      min.dist <- min.dist.bus %>%
        left_join(min.dist.tram, by = "id") %>%
        left_join(min.dist.train, by = "id")
      
    } else {
      
      # otherwise (ie not bus) - single set of distance calculations
      
      # distances from destinations to addresses
      dest.dist <- distances(g,
                             as.character(AC.destination.nodes),
                             as.character(address.nodes))
      
      # minimum distance between destinations and addresses
      # lowest distance for each address node (columns are address nodes, so 2)
      min.dist <- apply(dest.dist, 2, min, na.rm = TRUE) %>%  
        as.data.frame() %>%
        cbind(id = as.numeric(row.names(.))) %>%
        rename("distance" = ".")  
      
    }
    
    # join to AC addresses
    AC.addresses.with.dist <- AC.addresses %>%
      left_join(min.dist, by = c("address.n.node" = "id"))
    
    # test whether at least 80% of distances are within required.dist
    
    if (destination.type == "bus") {
      
      # if bus - are 80% within required distance for any mode ('required.dist'
      # is a vector with separate distances for each mode)
      
      test.result <- nrow(AC.addresses.with.dist %>%
                            filter(bus <= required.dist[1] |
                                     tram <= required.dist[2] |
                                     train <= required.dist[3])) / 
        nrow(AC.addresses.with.dist) >= 0.8
      
    } else {
      
      # otherwise (ie not bus) - are 80% within required distance
      
      test.result <- nrow(AC.addresses.with.dist %>%
                            filter(distance <= required.dist[1])) /
        nrow(AC.addresses.with.dist) >= 0.8
      
    }
    
    # if fail test, then find failed addresses; otherwise, there are none
    if (!test.result) {
      # test failed - find failed addresses 
      
      if (destination.type == "bus") {
        
        failed.addresses <- AC.addresses %>%
          # addresses that exceed required distances
          filter(id %in% (AC.addresses.with.dist %>%
                            filter(bus > required.dist[1] &
                                     tram > required.dist[2] &
                                     train > required.dist[3]) %>%
                            .$id))
        
      } else {
        
        failed.addresses <- AC.addresses %>%
          # addresses that exceed required distance
          filter(id %in% (AC.addresses.with.dist %>%
                            filter(distance > required.dist) %>%
                            .$id))
        
      }
      
      # ggplot() +
      #   geom_sf(data = AC.addresses, colour = "blue") +
      #   geom_sf(data = failed.addresses, colour = "black") +
      #   geom_sf(data = AC.destinations, aes(shape = "AC.destinations"), colour = "red", size = 4) +
      #   scale_shape_manual(values = c("AC.destinations" = 15))  # 15 corresponds to a square
      
    } else {
      # test passed - there are no failed addresses
      failed.addresses <- c()
    }
    
  } else {
    
    # if no destinations, then test fails, and all AC addresses are failed addresses
    # ---------------------------------#
    
    test.result <- FALSE
    failed.addresses <- AC.addresses
    
  }
  
  # return output - result (T or F) and failed addresses
  # -----------------------------------#
  
  return(list(test.result, failed.addresses))
  
}
