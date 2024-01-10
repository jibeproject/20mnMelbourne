# function to test whether NAC meets test of 80% of residences within required distance

# function to find distances between destinations and addresses

testFailedNac <- function(NAC.addresses,
                          destination.type,
                          destination.locations,
                          network.nodes,
                          network.links,
                          g,
                          required.dist,
                          entry.nodes) {  
  
  # set up addresses and destinations
  # -----------------------------------#
   
  # unique NAC address network nodes
  address.nodes <- unique(NAC.addresses$address.n.node)   
  
  # buffer to required.dist, because only destinations within that distance can 
  # be reachable from the addresses
  NAC.buffer <- NAC.addresses %>%
    st_buffer(required.dist) %>%
    summarise()

  # nodes representing destinations: separate bus, tram and train for bus;
  # pre-calculated entry nodes for polygons;
  # otherwise nearest nodes to features
  
  if (destination.type == "bus") {
   
    # destinations within the NAC buffer
    NAC.destinations <- destination.locations %>%
      st_filter(NAC.buffer, .predicate = st_intersects)
    
    # nearest node to feature types
    NAC.destination.nodes.bus <- 
      network.nodes$id[st_nearest_feature(NAC.destinations %>%
                                            filter(dest_type == "bus"),
                                          network.nodes)] %>%
      unique()
    
    NAC.destination.nodes.tram <- 
      network.nodes$id[st_nearest_feature(NAC.destinations %>%
                                            filter(dest_type == "tram"),
                                          network.nodes)] %>%
      unique()
    
    NAC.destination.nodes.train <- 
      network.nodes$id[st_nearest_feature(NAC.destinations %>%
                                            filter(dest_type == "train"),
                                          network.nodes)] %>%
      unique()
    
    NAC.destination.nodes <- c(NAC.destination.nodes.bus,
                               NAC.destination.nodes.tram,
                               NAC.destination.nodes.train)
    
    
    
  } else if (destination.type %in% c("district_sport", "park")) {
    
    # entry nodes within the NAC buffer
    NAC.destination.nodes <- network.nodes %>%
      filter(id %in% entry.nodes) %>%
      st_intersection(NAC.buffer) %>%
      .$id
    
  } else {
    
    # destinations within the NAC buffer
    NAC.destinations <- destination.locations %>%
      st_filter(NAC.buffer, .predicate = st_intersects)
    
    # nearest nodes to features
    NAC.destination.nodes <- 
      network.nodes$id[st_nearest_feature(NAC.destinations, network.nodes)] %>%
      unique()
    
  }

  # ggplot() + 
  #   geom_sf(data = NAC.buffer) +
  #   geom_sf(data = NAC.addresses, colour = "blue") +
  #   geom_sf(data = NAC.destinations, colour = "red", size = 2)

    
  
  # run the test
  # -----------------------------------#
  
  if (length(NAC.destination.nodes) > 0) {
      
    # where some destination locations are present - do they cover 80%?
    # ---------------------------------#
    
    if (destination.type == "bus") {
      
      # for bus - separate distance calculations for each mode

      # bus distances, and minimum for each address node
      if (length(NAC.destination.nodes.bus) > 0) {
        dest.dist.bus <- distances(g,
                                   as.character(NAC.destination.nodes.bus),
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
      if (length(NAC.destination.nodes.tram) > 0) {
        dest.dist.tram <- distances(g,
                                   as.character(NAC.destination.nodes.tram),
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
      if (length(NAC.destination.nodes.train) > 0) {
        dest.dist.train <- distances(g,
                                   as.character(NAC.destination.nodes.train),
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
                             as.character(NAC.destination.nodes),
                             as.character(address.nodes))
      
      # minimum distance between destinations and addresses
      # lowest distance for each address node (columns are address nodes, so 2)
      min.dist <- apply(dest.dist, 2, min, na.rm = TRUE) %>%  
        as.data.frame() %>%
        cbind(id = as.numeric(row.names(.))) %>%
        rename("distance" = ".")  

    }
    
    # join to NAC addresses
    NAC.addresses.with.dist <- NAC.addresses %>%
      left_join(min.dist, by = c("address.n.node" = "id"))
    
    # test whether at least 80% of distances are within required.dist
    
    if (destination.type == "bus") {
      
      # if bus - are 80% within required distance for any mode ('required.dist'
      # is a vector with separate distances for each mode)
      
      test.result <- nrow(NAC.addresses.with.dist %>%
                            filter(bus <= required.dist[1] |
                                     tram <= required.dist[2] |
                                     train <= required.dist[3])) / 
        nrow(NAC.addresses.with.dist) >= 0.8
      
    } else {
      
      # otherwise (ie not bus) - are 80% within required distance
      
      test.result <- nrow(NAC.addresses.with.dist %>%
                            filter(distance <= required.dist)) /
        nrow(NAC.addresses.with.dist) >= 0.8
      
    }

    # if fail test, then find failed addresses; otherwise, there are none
    if (!test.result) {
      # test failed - find failed addresses 
      
      if (destination.type == "bus") {
        
        failed.addresses <- NAC.addresses %>%
          # addresses that exceed required distances
          filter(id %in% (NAC.addresses.with.dist %>%
                            filter(bus > required.dist[1] &
                                     tram > required.dist[2] &
                                     train > required.dist[3]) %>%
                            .$id))
        
      } else {
        
        failed.addresses <- NAC.addresses %>%
          # addresses that exceed required distance
          filter(id %in% (NAC.addresses.with.dist %>%
                            filter(distance > required.dist) %>%
                            .$id))
        
      }
      
      # ggplot() +
      #   geom_sf(data = NAC.addresses, colour = "blue") +
      #   geom_sf(data = failed.addresses, colour = "black") +
      #   geom_sf(data = NAC.destinations, aes(shape = "NAC.destinations"), colour = "red", size = 4) +
      #   scale_shape_manual(values = c("NAC.destinations" = 15))  # 15 corresponds to a square
      
    } else {
      # test passed - there are no failed addresses
      failed.addresses <- c()
    }

  } else {
    
    # if no destinations, then test fails, and all NAC addresses are failed addresses
    # ---------------------------------#
    
    test.result <- FALSE
    failed.addresses <- NAC.addresses
    
  }

  # return output - result (T or F) and failed addresses
  # -----------------------------------#
  
  return(list(test.result, failed.addresses))
  
}
