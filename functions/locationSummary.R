# function to summarise the number of destination locations of each type in
# a file of new destinations (eg intervention locations.sqlite)

locationSummary <- function(destinations.file) {
  
  # vector of names of all layers of new destinations file
  location.layers <- st_layers(destinations.file) %>%
    .$name
  
  # empty dataframe to hold locations
  locations <- c()
  
  # add each layer to the empty dataframe
  for (i in 1:length(location.layers)) {
    # read in the layer
    assign(location.layers[i], st_read(destinations.file, 
                                       layer = location.layers[i]))
    # layer without geometry
    layer.locations <- get(location.layers[i]) %>%
      st_drop_geometry()
    # add to the dataframe
    locations <- bind_rows(locations, layer.locations)
  }
  
  # summary of number of each new location placed
  location.summary <- locations %>%
    # number of each destination type
    group_by(dest_type) %>%
    summarise(added.dest = n()) %>%
    ungroup() %>%
    # add total row
    rbind(data.frame(dest_type = "total", added.dest = sum(.$added.dest)))

  return(location.summary)
  
}
