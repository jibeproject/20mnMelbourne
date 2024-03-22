# create 400m or 800m walking distance buffers for ACs

makeAcCatchments <- function(ACs,
                              supermarkets,
                              network.nodes,
                              network.links,
                              residential.addresses,
                              BUFFDIST.SMALL,
                              BUFFDIST.MED.LARGE,
                              DENSIFICATION.DIST, 
                              temp.address.location,
                              temp.polygon.location) {
  
  # ACs = ACs %>% filter(CENTRESIZE != "Undeveloped")
  # supermarkets = st_read(POIs.location) %>% filter(Attribute == "supermarket")
  # residential.addresses = st_read(residential.address.location)
  # network.nodes = network.nodes.walk
  # network.links = network.links.walk
  # BUFFDIST.SMALL = BUFFDIST.SMALL
  # BUFFDIST.MED.LARGE = BUFFDIST.MED.LARGE
  # DENSIFICATION.DIST = DENSIFICATION.DIST
  # temp.address.location = temp.address.location
  # temp.polygon.location = temp.polygon.location
  
  # find anchor points for ACs 
  # -----------------------------------#
  # anchor(s) for ACs are supermarket(s) if any, or else centroids
  
  print(paste(Sys.time(), "|", "finding AC anchors (supermarkets or centroids)"))
  
  # buffer ACs by 30m, to catch supermarket locations placed in adjacent roads
  ACs.buffered <- ACs %>%
    st_buffer(30)
  
  supermarket.anchors <- st_intersection(ACs.buffered, supermarkets) %>%
    dplyr::select(CENTRE_NO, size)
  
  centroid.anchors <- ACs %>%
    # ACs that don't have supermarkets
    filter(!CENTRE_NO %in% supermarket.anchors$CENTRE_NO) %>%
    # centroid
    st_centroid() %>%
    dplyr::select(CENTRE_NO, size)

  anchors <- bind_rows(supermarket.anchors,
                            centroid.anchors) %>%
    # add nearest nodes (for supermarket-anchored ACs, these are used as the
    # supermarket location; for centroid-anchored ACs, the anchor will later
    # be re-snapped to nearest node in densified subnetwork)
    mutate(n.node = network.nodes$id[st_nearest_feature(., network.nodes)])
  
  
  # find address points within walking distance of anchors 
  # -----------------------------------#
  
  # parallel loop
  # setup for parallel processing - detect no of available cores and create cluster
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  
  # set up progress reporting 
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- txtProgressBar(max = nrow(ACs), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report
  print(paste(Sys.time(), "| Finding walkable catchments for", nrow(ACs), "ACs; parallel processing with", cores, "cores"))
  
  # loop to find walkable catchments - note that this will not actually produce
  # an 'output' file, because nothing is returned by the loop - instead, it
  # writes every stop to the temporary 'catchments' folders
  output <-
    foreach(i = 1:nrow(ACs),
            # foreach(i = 1:50,
            .combine = rbind,
            .export = c("densifySubNetwork"),
            .packages = c("dplyr", "sf", "igraph", "lwgeom"), 
            .options.snow = opts) %dopar% {
              
              # i=35  # Greensborough
              
              # AC and its anchor points
              AC <- ACs[i,]
              
              AC.anchors <- anchors %>%
                filter(CENTRE_NO == ACs$CENTRE_NO[i])
              
              if (AC$CENTRE_NO %in% supermarket.anchors$CENTRE_NO) {
                anchor.type <- "supermarket"
              } else {
                anchor.type <- "centroid"
              }
              
              # for centroid-anchored ACs, find distances to nearest nodes to 
              # anchors (anchor will snap to this node or, after densification,
              # a nearer node
              if (anchor.type == "centroid") {
                distances <- c()
                for (j in 1:nrow(AC.anchors)) {
                  AC.anchor <- AC.anchors[j, ]
                  n.node <- network.nodes %>% 
                    filter(id == AC.anchors$n.node[j])
                  dist <- st_distance(AC.anchor, n.node)
                  distances <- c(distances, dist)
                }
                max.n.node.dist <- max(distances)
              } else {
                max.n.node.dist <- 0
              }
               
              # buffer node to required size plus the highest distance
              # to the nearest node (all network distance points
              # must be within this euclidean distance buffer)
              if (AC$size == "small") {
                BUFFDIST <- BUFFDIST.SMALL + max.n.node.dist
              } else {
                BUFFDIST <- BUFFDIST.MED.LARGE + max.n.node.dist
              }
              
              # node buffer (dissolved if more than one anchor node)
              anchor.buffer <- st_buffer(AC.anchors, BUFFDIST) %>%
                summarise()
              
              # intersect the buffer with the network
              buffer.links <- network.links %>%
                st_filter(anchor.buffer, .predicate = st_intersects)
              
              # ggplot() +
              #   geom_sf(data = anchor.buffer) +
              #   geom_sf(data = buffer.links)
              
              # densify the intersecting network to 5m
              densified.subnetwork <- densifySubNetwork(buffer.links,
                                                        network.nodes,
                                                        DENSIFICATION.DIST)
              
              subnetwork.nodes <- densified.subnetwork[[1]]
              subnetwork.links <- densified.subnetwork[[2]]
              
              # for centroid-anchored ACs, revise n.node so it is the nearest 
              # node on the densified subnetwork
              if(anchor.type == "centroid") {
                AC.anchors <- AC.anchors %>%
                  mutate(n.node = subnetwork.nodes$id[st_nearest_feature(., subnetwork.nodes)])
              }
             
              # create the graph (undirected as used for walking)
              g.links <- subnetwork.links %>%
                st_drop_geometry() %>%
                mutate(weight = length) %>%
                dplyr::select(from_id, to_id, id, weight) 
              
              g <- graph_from_data_frame(g.links, directed = F)
              
              # distances from anchor nodes to others
              distances <- distances(g,
                                     as.character(AC.anchors$n.node),
                                     as.character(subnetwork.nodes$id))
              
              # lowest distance for each outside AC (columns are outside ACs, so 2)
              min.distances <- apply(distances, 2, min, na.rm = TRUE) %>% 
                as.data.frame() %>%
                cbind(id = as.numeric(row.names(.)))
              
              # reachable nodes are where  min dist is within BUFFDIST
              reachable.nodes <- min.distances %>%
                filter(. <= BUFFDIST) %>%
                .$id

              # ggplot() +
              #   geom_sf(data = subnetwork.nodes) +
              #   geom_sf(data = subnetwork.nodes %>%
              #             filter(id %in% reachable.nodes),
              #           colour = "blue") +
              #   geom_sf(data = subnetwork.nodes %>%
              #             filter(id %in% AC.anchors$n.node),
              #           colour = "red")

           
              # address nodes that are nearest to reachable nodes
              
              # addresses within buffer
              buffer.addresses <- residential.addresses %>%
                st_filter(anchor.buffer, .predicate = st_intersects) %>%
                dplyr::select(id)
              
              # ggplot() +
              #   geom_sf(data = subnetwork.nodes %>%
              #             filter(id %in% reachable.nodes),
              #           colour = "blue") +
              #   geom_sf(data = subnetwork.nodes %>%
              #             filter(id %in% AC.anchors$n.node),
              #           colour = "red") +
              #   geom_sf(data = buffer.addresses, colour = "green")
              
              
              # addresses where nearest node is a reachable node
              catchment.addresses <- buffer.addresses %>%
                # nearest subnetwork node
                mutate(nearest = subnetwork.nodes$id[st_nearest_feature(., subnetwork.nodes)]) %>%
                # filter to those where nearest node is reachable
                filter(nearest %in% reachable.nodes) %>%
                dplyr::select(id)
              
              # ggplot() +
              #   geom_sf(data = anchor.buffer) +
              #   geom_sf(data = buffer.links) +
              #   geom_sf(data = subnetwork.nodes %>%
              #             filter(id %in% reachable.nodes),
              #           colour = "blue") +
              #   geom_sf(data = subnetwork.nodes %>%
              #             filter(id %in% AC.anchors$n.node),
              #           colour = "red") +
              #   geom_sf(data = buffer.addresses, colour = "green") +
              #   geom_sf(data = catchment.addresses, colour = "purple")
              
            
              # catchment polygon for display (if there are any addresses)
              if (nrow(catchment.addresses) > 0) {
                # convex hull, which is outer bounds of catchment polygon
                catchment.hull <- 
                  st_convex_hull(st_union(catchment.addresses)) %>%
                  # convert from sfc to sf object
                  st_as_sf(.) %>%
                  # buffer to 15m for display
                  st_buffer(., 15)

                catchment.poly <-  st_voronoi(st_union(buffer.addresses)) %>%
                  st_cast() %>%  # splits geom collection into separate polygons 
                  st_sf() %>%
                  # join the intersecting addresses (containing the id field)
                  st_join(buffer.addresses, join = st_intersects) %>%
                  # filter to catchment addresses
                  filter(id %in% catchment.addresses$id) %>%
                  # dissolve
                  summarise() %>%
                  # # intersect with anchor buffer to omit any extensions from voronoi
                  # st_intersection(anchor.buffer)
                  # (better) intersect with catchment hull to omit any extensions from voronoi
                  st_intersection(catchment.hull)
                
              }

              # ggplot() +
              #   geom_sf(data = anchor.buffer) +
              #   geom_sf(data = catchment.poly, colour = "red") +
              #   # geom_sf(data = catchment.hull, colour = "blue") +
              #   geom_sf(data = buffer.links) +
              #   geom_sf(data = subnetwork.nodes %>%
              #             filter(id %in% reachable.nodes),
              #           colour = "blue") +
              #   geom_sf(data = subnetwork.nodes %>%
              #             filter(id %in% AC.anchors$n.node),
              #           colour = "red") +
              #   geom_sf(data = buffer.addresses, colour = "green") +
              #   geom_sf(data = catchment.addresses, colour = "purple")

              # write outputs to temporary folders
              if (nrow(catchment.addresses) > 0) {
                centre.no = AC$CENTRE_NO
                saveRDS(catchment.addresses %>% st_drop_geometry() %>% .$id, 
                        paste0(temp.address.location, "/", centre.no, ".rds"))
                saveRDS(catchment.poly, 
                        paste0(temp.polygon.location, "/", centre.no, ".rds"))
              }

            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  

}


