# function to  find entry nodes for polygons, with notional access points at 20m 
# intervals along roads within 30m of boundary - see
# https://github.com/carlhiggs/Australian-National-Liveability-Study-2018-datasets-supplementary-material/blob/main/Identifying%20public%20open%20space%20using%20OpenStreetMap.md
# and also including nodes within the polygon
# (note that 'buffered.links' is a parameter - need to buffer links before invoking the function)

findEntryNodes <- function(destination.type, 
                           polygons, 
                           nodes, 
                           buffered.links) {
  
  # destination.type = "district_sport"
  # polygons = baseline.locations[1:10, ]
  # nodes = network.nodes
  # buffered.links = st_buffer(network.links, 30)
  
  # report progress
  print(paste(Sys.time(), "| Finding entry nodes for", destination.type))
  
  # nodes within features
  internal.nodes <- nodes %>%
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
    st_intersection(buffered.links)
  
  # nodes representing the pseudo entry points
  pseudo.entry.nodes <- 
    nodes$id[st_nearest_feature(pseudo.entry.points, nodes)]
  
  # entry nodes are internal nodes and pseudo entry nodes combined
  entry.nodes <- unique(c(internal.nodes, pseudo.entry.nodes))
  
  return(entry.nodes)
  
}


