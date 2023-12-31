# function to load baseline destinations

# creates a list containing (1) a vector of 'destination types', and (2) a 
# dataframe (sf object) for each destination type, based on input files


loadBaselineDestinations <- function(POIs, ANLS.pos, ANLS.dest,
                                     temp_osm_2023.location,
                                     community.centre,
                                     community.health,
                                     region_buffer) {
  # destination types
  destination.types <- c()
  
  # restaurants and cafes
  destination.types <- c(destination.types, "restaurant_cafe")
  restaurant_cafe <- POIs %>%
    filter(Attribute %in% c("restaurant", "cafe"))
  
  # bus 
  destination.types <- c(destination.types, "bus")
  bus <- POIs %>%
    filter(Attribute == "bus")
  
  # tram 
  destination.types <- c(destination.types, "tram")
  tram <- POIs %>%
    filter(Attribute == "tram")
  
  destination.types <- c(destination.types, "train")
  train <- ANLS.dest %>%
    filter(dest_name == "gtfs_2018_stops_train") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # supermarket 
  destination.types <- c(destination.types, "supermarket")
  supermarket <- POIs %>%
    filter(Attribute == "supermarket")
  
  # convenience store
  destination.types <- c(destination.types, "convenience_store")
  convenience_store <- ANLS.dest %>%
    filter(dest_name == "convenience_osm") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # butcher
  destination.types <- c(destination.types, "butcher")
  butcher <- ANLS.dest %>%
    filter(dest_name == "meat_seafood_osm") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # bakery
  destination.types <- c(destination.types, "bakery")
  bakery <- ANLS.dest %>%
    filter(dest_name == "bakery_osm") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # pharmacy
  destination.types <- c(destination.types, "pharmacy")
  pharmacy <- POIs %>%
    filter(Attribute == "pharmacy")
  
  # post office
  destination.types <- c(destination.types, "post")
  post <- POIs %>%
    filter(Attribute == "post_office")
  
  # local parks
  destination.types <- c(destination.types, "park")
  park <- ANLS.pos %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # community centre  COME BACK TO THIS  
  destination.types <- c(destination.types, "community_centre")
  # community_centre <- POIs %>%
  #   filter(Attribute == "community centre")
  community_centre <- community.centre  %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # childcare
  destination.types <- c(destination.types, "childcare")
  childcare <- POIs %>%
    filter(Attribute == "child care")
  
  # kindergarten
  destination.types <- c(destination.types, "kindergarten")
  # kindergarten <- st_read(temp_osm_2023.location, layer = "kindergarten") %>%
  #   st_filter(region_buffer, .predicate = st_intersects)
  kindergarten <- ANLS.dest %>%
    filter(dest_name == "childcare_preschool_2019") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # primary school
  destination.types <- c(destination.types, "primary")
  # primary <- POIs %>%
  #   filter(Attribute %in% c("primary school", "primary/secondary school"))
  primary <- ANLS.dest %>%
    filter(dest_name %in% c("primary_schools2018", "P_12_Schools2018")) %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # district sport
  destination.types <- c(destination.types, "district_sport")
  district_sport <- ANLS.pos %>% 
    st_filter(region_buffer, .predicate = st_intersects) %>%
    mutate(area_ha = as.numeric(st_area(.)) / 10000) %>%
    filter(area_ha > 5) %>%
    st_filter(st_read(temp_osm_2023.location, layer = "sport"),
              .predicate = st_intersects)
  
  # community health centre
  destination.types <- c(destination.types, "community_health")
  # community_health <- POIs %>%
  #   filter(Attribute == "community health centre")
  community_health <- community.health %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # maternal/child health centre
  destination.types <- c(destination.types, "maternal_child_health")
  # maternal_child_health <- POIs %>%
  #   filter(Attribute == "maternal/child health centre")
  maternal_child_health <- ANLS.dest %>%
    filter(dest_name == "nhsd_2017_mc_family_health") %>%
    st_filter(region_buffer, .predicate = st_intersects)
  
  # GP
  destination.types <- c(destination.types, "gp")
  gp <- ANLS.dest %>%
    filter(dest_name == "nhsd_2017_gp") %>%
    st_filter(region_buffer, .predicate = st_intersects)

  # dentist
  destination.types <- c(destination.types, "dentist")
  dentist <- ANLS.dest %>%
    filter(dest_name == "nhsd_2017_dentist") %>%
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


