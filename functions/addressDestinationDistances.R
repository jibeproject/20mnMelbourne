# function to find distances between destinations and addresses

addressDestinationDistances <- function(destinations,
                                        residential.addresses,
                                        network.nodes,
                                        network.links, 
                                        PROJECT.CRS,
                                        multiple.destinations = NA,
                                        mode = "walk") {
# 
  # destinations = all.destinations
  # residential.addresses = residential.addresses
  # network.nodes = network.nodes.cycle
  # network.links = network.links.cycle
  # PROJECT.CRS = PROJECT.CRS
  # multiple.destinations <- list(c("restaurant_cafe", 4))
  # mode = cycle
  
  
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
  
  # unpack "multiple destinations"
  # ---------------------------------#
  mult.dest.types <- c()
  mult.dest.nos <- c()
  if (!is.na(multiple.destinations)[1]) {
    for (i in 1:length(multiple.destinations)) {
      mult.dest.types <- c(mult.dest.types, multiple.destinations[[i]][1])
      mult.dest.nos <- c(mult.dest.nos, multiple.destinations[[i]][2])
    }
  }
  
  # get unique residential addresses nodes
  # ---------------------------------#
  
  print(paste(Sys.time(), "| getting unique residential address nodes"))
  
  # unique residential address nodes
  if (mode == "walk") {
    residential.nodes <- unique(residential.addresses$address.n.node)   
  } else if (mode == "cycle") {
    # change address nodes to cycling nodes
    residential.nodes <- residential.addresses %>%
      mutate(address.n.node = network.nodes$id[st_nearest_feature(., network.nodes)]) %>%
      .$address.n.node %>%
      unique()
  } else {
    message(paste0("Network is not configured for the chosen mode '", mode, "'; terminating"))
    return(NULL)
  }
  
  
  # create the graph for finding distances (undirected where used for walking;
  # cycling also undirected as we're creating a cyclable distance catchment,
  # not a one-way route)
  # ---------------------------------#
  
  print(paste(Sys.time(), "|", "creating graph"))
  
  g.links <- network.links %>%
    st_drop_geometry() %>%
    mutate(weight = length) %>%
    dplyr::select(from_id, to_id, id, weight)
  
  g <- graph_from_data_frame(g.links, directed = F)
  
  
  # make directory to hold temporary outputs for distances to addresses
  # ---------------------------------#
  
  dir.create("./distances")
  
  
  # loop to find distances for each destination
  # ---------------------------------#
  
  for (i in 1:length(destination.types)) {
  # for (i in c(13:13)) {
    
    # load destinations
    # ---------------------------------#
    
    # report progress
    print(paste(Sys.time(), "|", destination.types[i], "- loading destinations"))
    
    # load the relevant destination types
    destination <- get(destination.types[i]) %>%
      st_sf() %>%
      st_set_crs(PROJECT.CRS)
    
    
    # find nearest nodes to destinations 
    # ---------------------------------#
    
    # find destination nodes (see findEntryNodes.R for details of how nodes
    # are located for park, which are polygons)

    if (destination.types[i] %in% c("park")) {
      
      buffered.links <- st_buffer(network.links, 30)
      
      dest.nodes <- findEntryNodes(destination.types[i],
                                   destination,
                                   network.nodes,
                                   buffered.links)
      
    } else {
      
      # nearest node to feature
      n.node <- network.nodes$id[st_nearest_feature(destination, network.nodes)]
      
      dest.nodes <- unique(n.node)
      
    }
    
    # measure distances from destinations to addresses 
    # ---------------------------------#
    
    # some destinations have large node numbers, so split into groups of 1000 - 
    # unless multiple destinations are required, in which case must be kept as one group
    
    if (!destination.types[i] %in% mult.dest.types) {
      
      # number of groups of 1000 destination nodes
      dest.groups <- ceiling(length(dest.nodes) / 1000)  # number of groups of up to 1000
      
      # assign each group of 1000 destination nodes to a 'dest group name'
      dest.group.names <- c()
      
      for (j in 1:dest.groups) {
        startno <- ((j-1) * 1000) + 1
        if (j != dest.groups) {
          endno <- j * 1000 
        } else {
          endno <- length(dest.nodes)
        }
        dest.group.name = paste0("dest.group.", j)
        assign(dest.group.name, dest.nodes[startno:endno])
        dest.group.names <- c(dest.group.names, dest.group.name)
      }
      
    } else {
      # single group where finding multiple destinations of same type
      dest.group.names <- "dest.group.1"
      dest.group.1 <- dest.nodes
    }
    
    # for each 'dest group name' group of 1000 destination nodes, get the 
    # distances to all of the residential address nodes - hold in memory
    # if up to 4 groups, or save to temporary folder if more
    
    if (length(dest.group.names) <= 4) {
      
      # vector to hold names of outputs
      dest.group.outputs <- c()
      
      # loop to find distances for each group
      for (j in 1:length(dest.group.names)) {
        
        # report progress
        print(paste(Sys.time(), "|", destination.types[i], 
                    "- finding distances from addresses; group", 
                    j, "of", length(dest.group.names)))
        
        # find distances for each group, and save output name in vector
        dest.group.output.name = paste0("dest.dist.", j)
        ## two alternatives for distances - one might work
        assign(dest.group.output.name,
               distances(g,
                         as.character(get(dest.group.names[j])),
                         as.character(residential.nodes)))
        ## some alternatives for distances in case of problems
        # assign(dest.group.output.name,
        #        distances(g,
        #                  as.character(get(dest.group.names[j])),
        #                  as.character(as.numeric(residential.nodes))))
        # assign(dest.group.output.name, 
        #        distances(g, 
        #                  get(dest.group.names[j]), 
        #                  residential.nodes))
        
        dest.group.outputs <- c(dest.group.outputs, dest.group.output.name)
        
      }
      
    } else {
      
      # temporary folder for outputs where > 4 groups
      dir.create("./distances/matrices")
      
      # parallel loop
      # setup for parallel processing - detect no of available cores and create cluster
      cores <- detectCores()
      cluster <- parallel::makeCluster(cores)
      doSNOW::registerDoSNOW(cluster)
      
      # set up progress reporting 
      pb <- txtProgressBar(max = length(dest.group.names), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
      
      # report progress
      print(paste(Sys.time(), "|", destination.types[i], 
                  "- finding distances from addresses for", length(dest.group.names),
                  "groups of destination nodes; parallel processing with", cores, "cores"))
      
      # loop to find distances - note that this will not actually produce
      # an 'output' file, because nothing is returned by the loop - instead, it
      # writes every output to the temporary 'distances/matrices' folder
      output <-
        foreach(j = 1:length(dest.group.names),
                # foreach(j = 59:73,
                .combine = rbind,
                .export = dest.group.names,
                .packages = c("dplyr", "sf", "igraph"), 
                .options.snow = opts) %dopar% {
                  
                  # find distances for each group, and save to the temporary folder
                  dest.group.output <- distances(g,
                                                 as.character(get(dest.group.names[j])),
                                                 as.character(residential.nodes))
                  ## some alternatives for distances in case of problems
                  # dest.group.output <- distances(g,
                  #                                as.character(get(dest.group.names[j])),
                  #                                as.character(as.numeric(residential.nodes)))
                  # dest.group.output <- distances(g, 
                  #                                get(dest.group.names[j]),
                  #                                residential.nodes)
                  
                  saveRDS(dest.group.output, paste0("./distances/matrices/dest.dist.", j, ".rds"))
                  
                }
      
      # close the progress bar and cluster
      close(pb)
      stopCluster(cluster)
      
    }
    
    # now find minimum distance to a feature for each address
    # ---------------------------------#
    
    if (length(dest.group.names) == 1) {
      
      # where there is only one group, find the distance minimum for each address
      # note that if multiple destinations are required, there is always only one group
      
      # get the sole output
      dest.dist <- get(dest.group.outputs[1])
      
      # report progress
      print(paste(Sys.time(), "|", destination.types[i], 
                  "- finding minimum distance for each address"))
      
      # minimum distance between destinations and addresses
      
      if (!destination.types[i] %in% mult.dest.types) {
        # lowest distance for each resid node (columns are resid nodes, so 2)
        min.dist <- apply(dest.dist, 2, min, na.rm = TRUE) %>%  
          as.data.frame() %>%
          cbind(id = as.numeric(row.names(.))) %>%
          rename(!!destination.types[i] := ".")  
        
      } else {
        # where multiple destinations required
        # number required, from mult.dest.nos, corresopnding to the relevant dest type
        no.reqd <- mult.dest.nos[which(mult.dest.types == destination.types[i])]
        
        # function to get the n lowest distances for each residential address
        get_n_lowest <- function(x, n) {
          sort(x, na.last = NA)[1:n]
        }
        
        # apply the function to get the 3 lowest distances for each residential address
        lowest_n_dist <- t(apply(dest.dist, 2, get_n_lowest, n = no.reqd))
        
        # create a data frame with the results
        min.dist <- data.frame(id = as.numeric(row.names(lowest_n_dist)), lowest_n_dist)
 
        # rename columns
        colnames(min.dist)[2] <- destination.types[i]
        colnames(min.dist)[3:ncol(min.dist)] <- paste0(destination.types[i], "_", 2:no.reqd)
      }
      
    } else {
      
      # find the minimum distance for each address in each group of 1000, 
      # then the minimum of the group minimums for that address
      
      for (j in 1:length(dest.group.names)) {
        
        # report progress
        print(paste(Sys.time(), "|", destination.types[i], 
                    "- finding minimum distance for each address; group",
                    j, "of", length(dest.group.names)))
        
        # load destination group from memory or, if > 4 groups, from file
        if (length(dest.group.names) <= 4){
          dest.group <- get(dest.group.outputs[j])
        } else {
          dest.group <- readRDS(paste0("./distances/matrices/", 
                                       list.files("./distances/matrices")[j]))
        }
        
        # find the minimum for the group of 1000
        min.dist.group <- apply(dest.group, 2, min, na.rm = TRUE) %>%  
          as.data.frame() %>%
          cbind(id = as.numeric(row.names(.)))
        
        # bind the minimum for the group of 1000 into a combined dataframe
        if (j == 1) {
          min.dist.groups <- min.dist.group
        } else {
          min.dist.groups <- min.dist.groups %>%
            left_join(min.dist.group, by = "id")
        }
      }
      
      # now find the minimum of the minimums
      
      # report progress
      print(paste(Sys.time(), "|", destination.types[i], 
                  "- finding minimum distance across all groups"))
      
      # minimum of the minimums
      min.dist <- min.dist.groups %>%
        rowwise() %>%
        mutate(min = min(across(-id), na.rm = TRUE)) %>%
        ungroup()%>%
        dplyr::select(id, min) %>%
        rename(!!destination.types[i] := min)
      
      # remove the temporary folder created where > 4 groups
      if (length(dest.group.names) > 4) {
        unlink("./distances/matrices", recursive = TRUE)
      }
    }
    
    # write output to temporary directory
    # ---------------------------------#
    
    saveRDS(min.dist, paste0("./distances/dest_", destination.types[i], ".rds"))
    
  }
  
  # assemble table of nodes and distances
  # ---------------------------------#
  # report progress
  print(paste(Sys.time(), "|", "assembling distances with residential address nodes"))
  
  for (i in 1:length(list.files("./distances"))) {
    distance.file <- readRDS(paste0("./distances/", list.files("./distances")[i]))
    if (i == 1) {
      address.destination.node.distances <- distance.file %>%
        # put id first
        select(id, everything())
    } else {
      address.destination.node.distances <- address.destination.node.distances %>%
        left_join(distance.file, by = "id")
    }
  }
  
  address.destination.node.distances <- address.destination.node.distances %>%
    rename(node_id = id)
  
  # remove the distances folder
  unlink("./distances", recursive = TRUE)
  
  
  return(address.destination.node.distances)
  
}


