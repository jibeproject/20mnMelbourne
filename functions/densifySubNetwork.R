# densify a network of links and nodes (assumed to consist of a subset of
# the overall network) by adding vertices at [1m] 5M intervals

densifySubNetwork <- function(links, network.nodes, DENSIFICATION.DIST) {
  
  # links = buffer.links
  # network.nodes = network.nodes
  # DENSIFICATION.DIST = DENSIFICATION.DIST
  
  nodes <- network.nodes %>%
    filter(id %in% links$from_id | id %in% links$to_id)
  
  network.crs <- st_crs(links)
  
  nodes_df <- nodes
  links_df <- links %>%
    mutate(tmp_id=row_number())
  
  segment_nodes <- links_df %>%
    st_line_sample(density = units::set_units(DENSIFICATION.DIST, m)) %>%
    st_sf()
  
  links_list <- links_df %>% st_geometry()
  nodes_list <- segment_nodes %>%
    st_buffer(0.001, endCapStyle="SQUARE") %>%
    st_geometry()
  links_list_segmented <- list()
  
  for (i in 1:length(links_list)) {
    links_list_segmented[i] <- st_difference(links_list[i],nodes_list[i])
  }
  
  # add flag for direction in which geometry of links_to_segmentize is recorded
  links_to_segmentize <- links_df %>%
    # join X & Y coordinates for from_id
    left_join(nodes_df %>%
                st_drop_geometry() %>%
                dplyr::select(id, node.fromx = x, node.fromy = y), 
              by = c("from_id" = "id")) %>%
    # check whether startpoint of geometry matches from_id ("forward" if yes, "reverse" if no)
    mutate(startpoint = st_coordinates(st_startpoint(GEOMETRY))) %>%
    rowwise() %>%
    mutate(direction = 
             ifelse(startpoint[[1]] == node.fromx & startpoint[[2]] == node.fromy, 
                    "forward", 
                    "reverse")) %>%
    ungroup()
  
  links_segmented <- links_to_segmentize %>%
    st_set_geometry(st_sfc(links_list_segmented)) %>%
    mutate(group_id = row_number()) %>%
    st_cast(to = "MULTILINESTRING") %>%
    # st_snap_to_grid(1) %>%
    st_sf() %>%
    st_cast(to = "LINESTRING") %>%
    mutate(new_node_id = row_number() + max(nodes_df$id, na.rm=T)) %>%
    group_by(group_id) %>%
    mutate(from_id = case_when(
      direction == "forward" & row_number() == 1 ~ from_id,
      direction == "forward" & row_number() != 1 ~ new_node_id - 1L,
      direction == "reverse" & row_number() != max(row_number()) ~ new_node_id,
      direction == "reverse" & row_number() == max(row_number()) ~ from_id
     )) %>%
    mutate(to_id = case_when(
      direction == "forward" & row_number() != max(row_number()) ~ new_node_id,
      direction == "forward" & row_number() == max(row_number()) ~ to_id,
      direction == "reverse" & row_number() == 1 ~ to_id,
      direction == "reverse" & row_number() != 1 ~ new_node_id - 1L,
    )) %>%
    mutate(length = round(as.numeric(st_length(GEOMETRY)), 3)) %>%
    dplyr::select(-group_id, -new_node_id, -fromx, -fromy, -startpoint)
  
  nodes_segmented_forward <- links_segmented %>%
    filter(direction == "forward") %>%
    dplyr::select(id = to_id) %>%
    filter(id > max(nodes_df$id, na.rm = T)) %>%
    st_set_geometry(st_endpoint(.))
  
  nodes_segmented_reverse <- links_segmented %>%
    filter(direction == "reverse") %>%
    dplyr::select(id = to_id) %>%
    filter(id > max(nodes_df$id, na.rm = T)) %>%
    st_set_geometry(st_startpoint(.))
  
  nodes_segmented <- rbind(nodes_segmented_forward,
                           nodes_segmented_reverse) %>%
    mutate(is_roundabout = 0, is_signal = 0)
  
  nodes_segmented <- bind_cols(nodes_segmented,
                               data.frame(st_coordinates(nodes_segmented))) %>%
    dplyr::select(id,is_roundabout, is_signal, X, Y) %>%
    st_set_crs(network.crs)
  
  links_segmented <- links_segmented %>%
    dplyr::select(-direction) %>%
    st_set_crs(network.crs)
  
  # combine the new segmented nodes with the original nodes
  nodes_combined <- bind_rows(
    nodes_segmented,
    nodes_df)
  
  return(list(nodes_combined, links_segmented))
}
