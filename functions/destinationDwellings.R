# function to find dwellings within required distance of destinations

destinationDwellings <- function(destinations,
                                 residential.addresses,
                                 network.nodes,
                                 network.links, 
                                 PROJECT.CRS) {
  
  # destinations = intervention.destinations
  # network.nodes = network.nodes.walk
  # network.links = network.links.walk
  
  # unpack "destinations"
  # ---------------------------------#
  
  print(paste(Sys.time(), "|", "setting up destination types"))
  
  # "destinations" is a list - first element is a vector of destination types,
  # and each subsequent element is a dataframe of the type (note that the listing
  # and unpacking process loses the crs, so needs to be added back in loop below)
  destination.types <- unlist(destinations[1])
  for (i in 1:length(destination.types)) {
    assign(destination.types[i], as.data.frame(destinations[i+1]))
  }
  
  
  # get unique residential addresses nodes
  # ---------------------------------#
  
  print(paste(Sys.time(), "| getting unique residential addresses nodes"))
  
  # unique residential address nodes
  residential.nodes <- unique(residential.addresses$address.n.node)   
  
  
  # create the graph for finding distances (undirected as used for walking distance catchment)
  # ---------------------------------#
  
  print(paste(Sys.time(), "|", "creating graph"))
  
  g.links <- network.links %>%
    st_drop_geometry() %>%
    mutate(weight = length) %>%
    dplyr::select(from_id, to_id, id, weight)
  
  g <- graph_from_data_frame(g.links, directed = F)
  
  
  # make directory to hold temporary outputs for each destination type
  # ---------------------------------#
  
  dir.create("./catchment dwellings")
  
  
  # loop to find distances for each destination
  # ---------------------------------#
  
  for (i in 1:length(destination.types)) {
    # for (i in c(13:13)) {
    
    # report progress
    print(paste(Sys.time(), "|", destination.types[i], "- loading destinations"))
    
    # load the relevant destinations
    destination <- get(destination.types[i]) %>%
      st_sf() %>%
      st_set_crs(PROJECT.CRS)
    
    # catchment distance for destination type
    if (destination.types[i] %in% c("convenience_store", "restaurant_cafe", "park", "bus")) {
      catchment.dist <- 400
    } else if (destination.types[i] == "tram") {
      catchment.dist <- 600
    } else {
      catchment.dist <- 800
    }
    
    # buffered links for parks (to find entry nodes)
    if (destination.types[i] == "park") {
      buffered.links <- st_buffer(network.links, 30)
    }
    
    for (j in 1:nrow(destination)) {
      # for (j in 1:10) {
      # relevant location
      new.location <- destination[j, ]
      
      # nearest node(s)
      if (destination.types[i] == "park") {
        from.node <- findEntryNodes("park",
                                    new.location,
                                    network.nodes,
                                    buffered.links)
      } else {
        from.node <- network.nodes$id[st_nearest_feature(new.location, network.nodes)]
      }
      
      # find distances from the destination node(s) ('from_node') to the residential nodes
      location.distances <- distances(g,
                                      as.character(from.node),
                                      as.character(residential.nodes))
      
      # if more than one from.node (ie parks), find minimum for each residential node
      # (that is, distance between the residential node and nearest park entry point)
      if (nrow(location.distances) > 1) {
        location.distances <- apply(location.distances, 2, min) %>% 
          as.data.frame() %>%
          t()
      }
      
      # find columns within the catchment distance; the nodes are the column names 
      catchment.nodes <- location.distances %>%
        # transpose and make into dataframe
        t() %>%
        as.data.frame() %>%
        # rename the single distance column, and add node column (former column names, now row names)
        rename(dist = 1) %>%
        mutate(node = as.integer(row.names(.))) %>%  
        # filter to those within catchment distance
        filter(dist <= catchment.dist)
      
      # for each catchment node, find the number of addresses for which it is the nearest node
      catchment.addresses <- nrow(residential.addresses %>%
                                    filter(address.n.node %in% catchment.nodes$node))
      
      destination$dwel_served[j] <- catchment.addresses
      
      if (j %% 50 == 0) {
        print(paste(Sys.time(), "|", destination.types[i], "- found dwellings served by",
                    j, "of", nrow(destination)))
      }
    }
    
    # write output to temporary directory
    saveRDS(destination, paste0("./catchment dwellings/dest_", destination.types[i], ".rds"))
  }
  
  # assemble table of dwellings served for all destination types
  # ---------------------------------#
  # report progress
  print(paste(Sys.time(), "|", "assembling dwellings served for all destination types"))
  
  for (i in 1:length(list.files("./catchment dwellings"))) {
    catch.dwel.file <- readRDS(paste0("./catchment dwellings/", list.files("./catchment dwellings")[i]))
    if (i == 1) {
      catchment.dwellings <- catch.dwel.file
    } else {
      catchment.dwellings <- bind_rows(catchment.dwellings,
                                       catch.dwel.file)
    }
  }
  
  # add a unique id field
  catchment.dwellings <- catchment.dwellings %>%
    mutate(dest_id = row_number())
  
  # remove the catchment dwellings folder
  unlink("./catchment dwellings", recursive = TRUE) 
  
  return(catchment.dwellings)
  
}

