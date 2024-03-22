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
  
  if (mode == "people") {
    
    # build table
    area.coverage <- c()
    
    for (i in 1:length(areas)) {
      area_data <- get(areas[i]) %>% st_drop_geometry()
      people = sum(area_data$pop_wt)
      
      supermarket = sum(area_data %>% filter(supermarket <= 800) %>% .$pop_wt)
      pharmacy = sum(area_data %>% filter(pharmacy <= 800) %>% .$pop_wt)
      post = sum(area_data %>% filter(post <= 800) %>% .$pop_wt)
      gp = sum(area_data %>% filter(gp <= 800) %>% .$pop_wt)
      mat.child.health = sum(area_data %>% filter(maternal_child_health <= 800) %>% .$pop_wt)
      dentist = sum(area_data %>% filter(dentist <= 800) %>% .$pop_wt)
      childcare = sum(area_data %>% filter(childcare <= 800) %>% .$pop_wt)
      kindergarten = sum(area_data %>% filter(kindergarten <= 800) %>% .$pop_wt)
      primary = sum(area_data %>% filter(primary <= 800) %>% .$pop_wt)
      comm.library = sum(area_data %>% filter(community_centre_library <= 800) %>% .$pop_wt)
      convenience = sum(area_data %>% filter(supermarket <= 400 | convenience_store <= 400) %>% .$pop_wt)
      rest.cafe = sum(area_data %>% filter(restaurant_cafe <= 800) %>% .$pop_wt)
      park = sum(area_data %>% filter(park <= 800) %>% .$pop_wt)
      bus.tram.train = sum(area_data %>% filter(bus <= 400 | tram <= 600 | train <= 800) %>% .$pop_wt)
      
      output.row <- data.frame(
        area = areas[i],
        people = people,
        supermarket.800 = supermarket / people * 100,
        pharmacy.800 = pharmacy / people * 100,
        post.800 = post / people * 100,
        gp.800 = gp / people * 100,
        mat.child.health.800 = mat.child.health / people * 100,
        dentist.800 = dentist / people * 100,
        childcare.800 = childcare / people * 100,
        kindergarten.800 = kindergarten / people * 100,
        primary.800 = primary / people * 100,
        comm.library.800 = comm.library / people * 100,
        convenience.400 = convenience / people * 100,
        rest.cafe.400 = rest.cafe / people * 100,
        park.400 = park / people * 100,
        bus.400.tram.600.train.800 = bus.tram.train / people * 100
      )
      
      area.coverage <- rbind(area.coverage, output.row)
    }
    
    # move people column to end
    area.coverage <- area.coverage %>%
      relocate(people, .after = last_col())

  } else if (mode == "dwellings") {
    
    # build table
    area.coverage <- c()
    
    for (i in 1:length(areas)) {
      area_data <- get(areas[i]) %>% st_drop_geometry()
      dwellings = sum(area_data$dwel_wt)
      
      supermarket = sum(area_data %>% filter(supermarket <= 800) %>% .$dwel_wt)
      pharmacy = sum(area_data %>% filter(pharmacy <= 800) %>% .$dwel_wt)
      post = sum(area_data %>% filter(post <= 800) %>% .$dwel_wt)
      gp = sum(area_data %>% filter(gp <= 800) %>% .$dwel_wt)
      mat.child.health = sum(area_data %>% filter(maternal_child_health <= 800) %>% .$dwel_wt)
      dentist = sum(area_data %>% filter(dentist <= 800) %>% .$dwel_wt)
      childcare = sum(area_data %>% filter(childcare <= 800) %>% .$dwel_wt)
      kindergarten = sum(area_data %>% filter(kindergarten <= 800) %>% .$dwel_wt)
      primary = sum(area_data %>% filter(primary <= 800) %>% .$dwel_wt)
      comm.library = sum(area_data %>% filter(community_centre_library <= 800) %>% .$dwel_wt)
      convenience = sum(area_data %>% filter(supermarket <= 400 | convenience_store <= 400) %>% .$dwel_wt)
      rest.cafe = sum(area_data %>% filter(restaurant_cafe <= 800) %>% .$dwel_wt)
      park = sum(area_data %>% filter(park <= 800) %>% .$dwel_wt)
      bus.tram.train = sum(area_data %>% filter(bus <= 400 | tram <= 600 | train <= 800) %>% .$dwel_wt)
      
      output.row <- data.frame(
        area = areas[i],
        dwellings = dwellings,
        supermarket.800 = supermarket / dwellings * 100,
        pharmacy.800 = pharmacy / dwellings * 100,
        post.800 = post / dwellings * 100,
        gp.800 = gp / dwellings * 100,
        mat.child.health.800 = mat.child.health / dwellings * 100,
        dentist.800 = dentist / dwellings * 100,
        childcare.800 = childcare / dwellings * 100,
        kindergarten.800 = kindergarten / dwellings * 100,
        primary.800 = primary / dwellings * 100,
        comm.library.800 = comm.library / dwellings * 100,
        convenience.400 = convenience / dwellings * 100,
        rest.cafe.400 = rest.cafe / dwellings * 100,
        park.400 = park / dwellings * 100,
        bus.400.tram.600.train.800 = bus.tram.train / dwellings * 100
      )
      
      area.coverage <- rbind(area.coverage, output.row)
    }
    
    # move dwellings column to end
    area.coverage <- area.coverage %>%
      relocate(dwellings, .after = last_col())
      
  } else {
    
    print(paste0("Not configured for mode ", mode, "; terminating"))
    return()
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
    relocate(dest.dist)
  
  return(area.coverage)
  
}
