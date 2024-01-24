# calculate overall area coverage as percentage of residences with access to 
# each destination type within specified distance for Greater Melbourne, 
# all ACs, and large, medium and small ACs

calculateCoverage <- function(residential.addresses,
                              address.destination.distances,
                              ac.catchment.addresses,
                              ACs,
                              region) {
  
  # residential.addresses = residential.addresses
  # address.destination.distances = baseline.distances
  # ac.catchment.addresses = ac.catchment.addresses
  # ACs = ACs
  # region = region
  
  # join residential addresses and distances
  residential.distances <- residential.addresses %>%
    left_join(address.destination.distances, by = "id")
  
  # join ac catchment addresses and sizes
  ac.catchment.addresses <- ac.catchment.addresses %>%
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
    filter(id %in% (ac.catchment.addresses %>%
                      filter(size == "large") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  medium <- residential.distances %>%
    # filter to address ids comprising the medium ac catchments
    filter(id %in% (ac.catchment.addresses %>%
                      filter(size == "medium") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  small <- residential.distances %>%
    # filter to address ids comprising the small ac catchments
    filter(id %in% (ac.catchment.addresses %>%
                      filter(size == "small") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  
  areas = c("melb", "AC", "large", "medium", "small")
  
  # build table
  area.coverage <- data.frame(area = areas) %>%
    # get number of residences within specified distance of each destination
    rowwise() %>%
    mutate(residences = get(area) %>% nrow(),
           supermarket.800 = get(area) %>%
             filter(supermarket <= 800) %>%  nrow() / residences * 100,
           pharmacy.800 = get(area) %>%
             filter(pharmacy <= 800) %>%  nrow() / residences * 100,
           post.800 = get(area) %>%
             filter(post <= 800) %>% nrow() / residences * 100,
           gp.800 = get(area) %>%
             filter(gp <= 800) %>%  nrow() / residences * 100,
           mat.child.health.800 = get(area) %>%
             filter(maternal_child_health <= 800) %>%  nrow() / residences * 100,
           dentist.800 = get(area) %>%
             filter(dentist <= 800) %>%  nrow() / residences * 100,
           childcare.800 = get(area) %>%
             filter(childcare <= 800) %>%  nrow() / residences * 100,
           kindergarten.800 = get(area) %>%
             filter(kindergarten <= 800) %>%  nrow() / residences * 100,
           primary.800 = get(area) %>%
             filter(primary <= 800) %>%  nrow() / residences * 100,
           comm.library.800 = get(area) %>%
             filter(community_centre_library <= 800) %>%  nrow() / residences * 100,
           convenience.400 = get(area) %>%
             filter(supermarket <= 400 | convenience_store <= 400) %>%
             nrow() / residences * 100,
           rest.cafe.400 = get(area) %>%
             filter(restaurant_cafe <= 400) %>%  nrow() / residences * 100,
           park.400 = get(area) %>%
             filter(park <= 400) %>%  nrow() / residences * 100,
           bus.400.tram.600.train.800 = get(area) %>% 
             filter(bus <= 400 | tram <= 600 | train <= 800) %>%  
             nrow() / residences * 100
    ) %>%
    ungroup() %>%  
    
    # move residences column to end
    relocate(residences, .after = last_col()) %>%
    
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
             dest.dist == "residences"      ~ "Number of residences"
           )) %>%
    
    # move text column to front
    relocate(dest.dist)
  
  return(area.coverage)
  
}
