# calculate overall area coverage as percentage of people or dwellings with access to 
# each destination type within specified distance for Greater Melbourne, 
# all ACs, and large, medium and small ACs

calculateCoverage <- function(residential.addresses,
                              address.destination.distances,
                              ac.catchment.addresses,
                              ACs,
                              region,
                              mode) {
  
  # residential.addresses = residential.addresses
  # address.destination.distances = baseline.distances
  # ac.catchment.addresses = ac.catchment.addresses
  # ACs = ACs
  # region = region
  # mode = "people"
  
  # exit if mode not correctly set
  if (!mode %in% c("people", "dwellings")) {
    print(paste0("Not configured for mode ", mode, "; terminating"))
    return()
    
  }
  
  # join residential addresses and distances
  residential.distances <- residential.addresses %>%
    dplyr::select(id) %>%
    left_join(address.destination.distances, by = "id")
  
  # join ac catchment addresses and sizes
  ac.catchment.addresses.with.size <- ac.catchment.addresses %>%
    left_join(ACs %>% 
                st_drop_geometry() %>%
                dplyr::select(CENTRE_NO, size), by = "CENTRE_NO")
  
  # global areas (melb, ACs, and ACs by size)
  melb <- residential.distances %>%
    # filter to addresses in greater melbourne (excluding buffer) only
    st_filter(region, .predicate = st_intersects)
  
  AC <- residential.distances %>%
    # filter to all address ids comprising the ac catchments
    filter(id %in% (ac.catchment.addresses$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  large <- residential.distances %>%
    # filter to address ids comprising the large ac catchments
    filter(id %in% (ac.catchment.addresses.with.size %>%
                      filter(size == "large") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  medium <- residential.distances %>%
    # filter to address ids comprising the medium ac catchments
    filter(id %in% (ac.catchment.addresses.with.size %>%
                      filter(size == "medium") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  small <- residential.distances %>%
    # filter to address ids comprising the small ac catchments
    filter(id %in% (ac.catchment.addresses.with.size %>%
                      filter(size == "small") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  
  areas = c("melb", "AC", "large", "medium", "small")
  
  # build table
  area.coverage <- c()
  
  for (i in 1:length(areas)) {
    area_data <- get(areas[i]) %>% st_drop_geometry()
    
    # add unit column depending on mode
    if (mode == "people") {
      area_data <- area_data %>%
        mutate(unit = pop_wt)
    } else if (mode == "dwellings") {
      area_data <- area_data %>%
        mutate(unit = dwel_wt)
    }
    
    units = sum(area_data$unit)
    
    supermarket = sum(area_data %>% filter(supermarket <= 800) %>% .$unit)
    pharmacy = sum(area_data %>% filter(pharmacy <= 800) %>% .$unit)
    post = sum(area_data %>% filter(post <= 800) %>% .$unit)
    gp = sum(area_data %>% filter(gp <= 800) %>% .$unit)
    mat.child.health = sum(area_data %>% filter(maternal_child_health <= 800) %>% .$unit)
    dentist = sum(area_data %>% filter(dentist <= 800) %>% .$unit)
    childcare = sum(area_data %>% filter(childcare <= 800) %>% .$unit)
    kindergarten = sum(area_data %>% filter(kindergarten <= 800) %>% .$unit)
    primary = sum(area_data %>% filter(primary <= 800) %>% .$unit)
    comm.library = sum(area_data %>% filter(community_centre_library <= 800) %>% .$unit)
    convenience = sum(area_data %>% filter(supermarket <= 400 | convenience_store <= 400) %>% .$unit)
    rest.cafe = sum(area_data %>% filter(restaurant_cafe <= 400) %>% .$unit)
    park = sum(area_data %>% filter(park <= 400) %>% .$unit)
    bus.tram.train = sum(area_data %>% filter(bus <= 400 | tram <= 600 | train <= 800) %>% .$unit)
    
    output.row <- data.frame(
      area = areas[i],
      units = units,
      supermarket.800 = supermarket / units * 100,
      pharmacy.800 = pharmacy / units * 100,
      post.800 = post / units * 100,
      gp.800 = gp / units * 100,
      mat.child.health.800 = mat.child.health / units * 100,
      dentist.800 = dentist / units * 100,
      childcare.800 = childcare / units * 100,
      kindergarten.800 = kindergarten / units * 100,
      primary.800 = primary / units * 100,
      comm.library.800 = comm.library / units * 100,
      convenience.400 = convenience / units * 100,
      rest.cafe.400 = rest.cafe / units * 100,
      park.400 = park / units * 100,
      bus.400.tram.600.train.800 = bus.tram.train / units * 100
    )
    
    area.coverage <- rbind(area.coverage, output.row)
  }
  
  # move people column to end
  area.coverage <- area.coverage %>%
    relocate(units, .after = last_col())
  
  # rename units column according to mode
  if (mode == "people") {
    area.coverage <- area.coverage %>%
      rename(people = units)
  } else if (mode == "dwellings") {
    area.coverage <- area.coverage %>%
      rename(dwellings = units)
  }
  
  area.coverage <- area.coverage %>%
    
    # transpose table
    t() %>%
    as.data.frame() %>%
    
    # use first row for column names
    setNames(., .[1, ]) %>%
    .[-1, ] %>%
    
    # add new text column, based on row names
    mutate(dest.dist = row.names(.),
           dest.dist = case_when(
             dest.dist == "supermarket.800" ~ "Supermarket",
             dest.dist == "pharmacy.800"    ~ "Pharmacy",
             dest.dist == "post.800"        ~ "Post office",
             dest.dist == "gp.800"          ~ "GP",
             dest.dist == "mat.child.health.800"  ~ "Maternal & child health centre",
             dest.dist == "dentist.800"     ~ "Dentist",
             dest.dist == "childcare.800"   ~ "Childcare centre",
             dest.dist == "kindergarten.800"  ~ "Kindergarten",
             dest.dist == "primary.800"     ~ "Primary school",
             dest.dist == "comm.library.800"    ~ "Community centre or library",
             dest.dist == "convenience.400" ~ "Convenience store or supermarket",
             dest.dist == "rest.cafe.400"   ~ "Restaurant or cafe",
             dest.dist == "park.400"        ~ "Local park",
             dest.dist == "bus.400.tram.600.train.800"  ~ 
               "Bus stop, tram stop or train station",
             dest.dist == "people"      ~ "Number of people",
             dest.dist == "dwellings"   ~ "Number of dwellings"
           )) %>%
    
    # move text column to front
    relocate(dest.dist) %>%
    
    # reconvert relevant columns to numeric
    mutate(across(-dest.dist, as.numeric))
  
  return(area.coverage)
  
}
