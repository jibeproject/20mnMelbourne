# function to find dwellings within required distance of destinations

peopleServed <- function(destinations,
                         residential.addresses,
                         network.nodes,
                         network.links, 
                         PROJECT.CRS,
                         transport,
                         mode) {

    # destinations = baseline.destinations
    # network.nodes = network.nodes.walk
    # network.links = network.links.walk
    # transport = "walk"
    # mode = "people"
  
  # set up for 'transport' and 'mode' choices
  # ---------------------------------#
  
  # exit if transport or mode not correctly set
  if (!transport %in% c("walk", "cycle")) {
    print(paste0("Not configured for transport mode ", transport, "; terminating"))
    return()
  }
  
  if (!mode %in% c("people", "dwellings")) {
    print(paste0("Not configured for mode ", mode, "; terminating"))
    return()
  }
  
  # add unit column to residential addresses depending on mode 
  if (mode == "people") {
    residential.addresses <- residential.addresses %>%
      mutate(unit = pop_wt)
  } else if (mode == "dwellings") {
    residential.addresses <- residential.addresses %>%
      mutate(unit = dwel_wt)
  }
  
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
  if (transport == "walk") {
    residential.nodes <- unique(residential.addresses$walk_node)   
  } else if (transport == "cycle") {
    residential.nodes <- unique(residential.addresses$cycle_node)   
  }
  
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
  
  dir.create("./destination catchments")
  
  
  # loop to find distances for each destination
  # ---------------------------------#
  
  for (i in 1:length(destination.types)) {
    # for (i in c(13:16)) {
    
    # report progress
    print(paste(Sys.time(), "|", destination.types[i], "- loading destinations"))
    
    # load the relevant destinations
    destination <- get(destination.types[i]) %>%
      st_sf() %>%
      st_set_crs(PROJECT.CRS)
    
    # catchment distance for destination type and transport
    if (destination.types[i] %in% c("convenience_store", "restaurant_cafe", "park", "bus")) {
      if (transport == "walk") {
        catchment.dist <- 400
      } else if (transport == "cycle") {
        catchment.dist <- 1250
      }
    } else if (destination.types[i] == "tram") {
      if (transport == "walk") {
        catchment.dist <- 600
      } else if (transport == "cycle") {
        catchment.dist <- 1875
      }
    } else {
      if (transport == "walk") {
        catchment.dist <- 800
      } else if (transport == "cycle") {
        catchment.dist <- 2500
      }
    }
    
    # buffered links for parks (to find entry nodes)
    if (destination.types[i] == "park") {
      buffered.links <- st_buffer(network.links, 30)
    }
    
    
    # setup for parallel processing and progress reporting
    # cores <- detectCores()
    cores <- 4
    cluster <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cluster)
    pb <- txtProgressBar(max = nrow(destination), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    # report
    print(paste0("Finding people served by ", nrow(destination), " ", destination.types[i],
               "(s); parallel processing with ", cores, " cores"))
    
    # loop to find list of boundary points
    output <-
      foreach(j = 1:nrow(destination),
      # foreach(j = 1:10,
              .combine = rbind,
              .packages = c("dplyr", "sf", "igraph"),
              .options.snow = opts) %dopar% {
                
                # relevant location
                location <- destination[j, ]
                
                # nearest node(s)
                if (destination.types[i] == "park") {
                  from.node <- findEntryNodes("park",
                                              location,
                                              network.nodes,
                                              buffered.links)
                } else {
                  from.node <- network.nodes$id[st_nearest_feature(location, network.nodes)]
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
                
                # for each catchment node, find the addresses for which it is the nearest node
                if (transport == "walk") {
                  catchment.addresses <- residential.addresses %>%
                    filter(walk_node %in% catchment.nodes$node)
                } else if (transport == "cycle") {
                  catchment.addresses <- residential.addresses %>%
                    filter(cycle_node %in% catchment.nodes$node)
                }
                
                # complete the 'served' column with the number of people (or dwellings), using the weight
                location$served <- sum(catchment.addresses$unit)
 
                return(location)
              }
    
    saveRDS(output, paste0("./destination catchments/dest_", destination.types[i], ".rds"))
    
    
    # close the progress bar and cluster
    close(pb)
    stopCluster(cluster)
    
  }
    
    
  # assemble table of people (or dwellings) served for all destination types
  # ---------------------------------#
  # report progress
  print(paste(Sys.time(), "|", "assembling", mode, "served for all destination types"))
  
  for (i in 1:length(list.files("./destination catchments"))) {
    served.file <- readRDS(paste0("./destination catchments/", list.files("./destination catchments")[i]))
    if (i == 1) {
      served <- served.file
    } else {
      served <- bind_rows(served, served.file)
    }
  }
  
  # add a unique id field
  served <- served %>%
    mutate(dest_id = row_number())
  
  # remove the destination catchments folder
  unlink("./destination catchments", recursive = TRUE) 
  
  return(served)
  
}

