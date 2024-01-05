# function to  find entry nodes for polygons, with notional access points at 20m 
# intervals along roads within 30m of boundary - see
# https://github.com/carlhiggs/Australian-National-Liveability-Study-2018-datasets-supplementary-material/blob/main/Identifying%20public%20open%20space%20using%20OpenStreetMap.md
# and also including nodes within the polygon

findEntryNodes <- function(destination.type, 
                           polygons, 
                           nodes, 
                           links) {
  
  # destination.type = "district_sport"
  # polygons = baseline.locations[1:10, ]
  # nodes = network.nodes
  # links = network.links
  
  # report progress
  print(paste(Sys.time(), "| Finding entry nodes for", destination.type))
  
  # nodes within features
  internal.nodes <- network.nodes %>%
    st_intersection(polygons) %>%
    st_drop_geometry() %>%
    .$id
  
  # pseudo entry points at 20m intervals, within 30m of a road
  pseudo.entry.points <- polygons %>% 
    # convert destination polygons boundaries to linestring
    st_cast(to = "MULTILINESTRING") %>%
    st_cast(to = "LINESTRING") %>%
    # locate points at 20m along boundaries
    st_line_sample(., density = units::set_units(20, m)) %>%
    # intersect with roads buffered to 30m
    # st_cast(to = "POINT")
    st_intersection(st_buffer(network.links, 30))
  
  # nodes representing the pseudo entry points
  pseudo.entry.nodes <- 
    network.nodes$id[st_nearest_feature(pseudo.entry.points, network.nodes)]
  
  # entry nodes are internal nodes and pseudo entry nodes combined
  entry.nodes <- unique(c(internal.nodes, pseudo.entry.nodes))
  
  return(entry.nodes)
  
}


