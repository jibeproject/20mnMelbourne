# function to load baseline destinations

# creates a list containing (1) a vector of 'destination types', and (2) a 
# dataframe (sf object) for each destination type, based on input files


loadBaselineDestinations <- function(POIs.location, 
                                     ANLS.dest.location,
                                     ANLS.pos.location,
                                     region_buffer) {
  
  # read in the destination files
  POIs <- st_read(POIs.location)
  
  ANLS.dest <- st_read(ANLS.dest.location, layer = "study_destinations") %>%
    st_transform(PROJECT.CRS)
  
  ANLS.pos <- st_read(ANLS.pos.location, layer = "public_open_space_osm_2018") %>%
    st_transform(PROJECT.CRS)
  

  # destination types
  destination.types <- c()
  
  # supermarket 
  destination.types <- c(destination.types, "supermarket")
  supermarket <- POIs %>%
    filter(Attribute == "supermarket")
  
  # convenience store
  destination.types <- c(destination.types, "convenience_store")
  convenience_store <- ANLS.dest %>%
    filter(dest_name == "convenience_osm") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # restaurant and cafe
  destination.types <- c(destination.types, "restaurant_cafe")
  restaurant_cafe <- POIs %>%
    filter(Attribute %in% c("restaurant", "cafe"))
  
  # pharmacy
  destination.types <- c(destination.types, "pharmacy")
  pharmacy <- POIs %>%
    filter(Attribute == "pharmacy")
  
  # post office
  destination.types <- c(destination.types, "post")
  post <- POIs %>%
    filter(Attribute == "post_office")
  
  # GP
  destination.types <- c(destination.types, "gp")
  gp <- ANLS.dest %>%
    filter(dest_name == "nhsd_2017_gp") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # maternal/child health centre
  destination.types <- c(destination.types, "maternal_child_health")
  maternal_child_health <- ANLS.dest %>%
    filter(dest_name == "nhsd_2017_mc_family_health") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # dentist
  destination.types <- c(destination.types, "dentist")
  dentist <- ANLS.dest %>%
    filter(dest_name == "nhsd_2017_dentist") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # childcare
  destination.types <- c(destination.types, "childcare")
  childcare <- POIs %>%
    filter(Attribute == "child care")
  
  # kindergarten
  destination.types <- c(destination.types, "kindergarten")
  kindergarten <- ANLS.dest %>%
    filter(dest_name == "childcare_preschool_2019") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # primary school
  destination.types <- c(destination.types, "primary")
  primary <- ANLS.dest %>%
    filter(dest_name %in% c("primary_schools2018", "P_12_Schools2018")) %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # community centre and library
  destination.types <- c(destination.types, "community_centre_library")
  community_centre_library <- POIs %>%
    filter(Attribute %in% c("community centre", "library"))
  
  # local parks
  destination.types <- c(destination.types, "park")
  park <- ANLS.pos %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # bus 
  destination.types <- c(destination.types, "bus")
  bus <- POIs %>%
    filter(Attribute == "bus")
  
  # tram 
  destination.types <- c(destination.types, "tram")
  tram <- POIs %>%
    filter(Attribute == "tram")
  
  #train
  destination.types <- c(destination.types, "train")
  train <- ANLS.dest %>%
    filter(dest_name == "gtfs_2018_stops_train") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  

  # output list, consisting of 'destination.types' and each destination dataframe
  output.list <- list(destination.types)
  for (i in 1:length(destination.types)) {
    # add each destination (for example, first destination type is item 2
    # in the list, after 'destination.types')
    output.list[[i + 1]] <- get(destination.types[i])
  }
  
  return(output.list)
  
}


