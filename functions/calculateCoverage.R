# calculate overall area coverage as percentage of residences with access to 
# each destination type within specified distance for Greater Melbourne, 
# all NACs, and large, medium and small NACs

calculateCoverage <- function(residential.addresses,
                              address.destination.distances,
                              nac.catchment.addresses,
                              NACs,
                              region) {
  
  # residential.addresses = residential.addresses
  # address.destination.distances = baseline.distances
  # nac.catchment.addresses = nac.catchment.addresses
  # NACs = NACs
  # region = region
  
  # join residential addresses and distances
  residential.distances <- residential.addresses %>%
    left_join(address.destination.distances, by = "id")
  
  # join nac catchment addresses and sizes
  nac.catchment.addresses <- nac.catchment.addresses %>%
    left_join(NACs %>% 
                st_drop_geometry() %>%
                dplyr::select(CENTRE_NO, size), by = "CENTRE_NO")
  
  # global areas (melb, NACs, and NACs by size)
  melb <- residential.distances %>%
    # filter to addresses in greater melbourne (excluding buffer) only
    st_filter(region, .predicate = st_intersects)
  
  NAC <- residential.distances %>%
    # filter to all address ids comprising the nac catchments
    filter(id %in% (nac.catchment.addresses$address_ids %>% 
             unlist() %>% 
             unique()))
  
  large <- residential.distances %>%
    # filter to address ids comprising the large nac catchments
    filter(id %in% (nac.catchment.addresses %>%
                      filter(size == "large") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  medium <- residential.distances %>%
    # filter to address ids comprising the medium nac catchments
    filter(id %in% (nac.catchment.addresses %>%
                      filter(size == "medium") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  small <- residential.distances %>%
    # filter to address ids comprising the small nac catchments
    filter(id %in% (nac.catchment.addresses %>%
                      filter(size == "small") %>%
                      .$address_ids %>% 
                      unlist() %>% 
                      unique()))
  
  
  areas = c("melb", "NAC", "large", "medium", "small")
  
  # build table
  area.coverage <- data.frame(area = areas) %>%
    # get number of residences within specified distance of each destination
    rowwise() %>%
    mutate(residences = get(area) %>% nrow(),
           rest.cafe.400 = get(area) %>%
             filter(restaurant_cafe <= 400) %>%  nrow() / residences * 100,
           bus.400.tram.600.train.800 = get(area) %>% 
             filter(bus <= 400 | tram <= 600 | train <= 800) %>%  
             nrow() / residences * 100,
           supermarket.800 = get(area) %>%
             filter(supermarket <= 800) %>%  nrow() / residences * 100,
           convenience.400 = get(area) %>%
             filter(supermarket <= 400 | convenience_store <= 400) %>%
             nrow() / residences * 100,
           butcher.800 = get(area) %>%
             filter(butcher <= 800) %>%  nrow() / residences * 100,
           bakery.800 = get(area) %>%
             filter(bakery <= 800) %>%  nrow() / residences * 100,
           pharmacy.800 = get(area) %>%
             filter(pharmacy <= 800) %>%  nrow() / residences * 100,
           post.800 = get(area) %>%
             filter(post <= 800) %>% nrow() / residences * 100,
           distsport.800 = get(area) %>%
             filter(district_sport <= 800) %>%  nrow() / residences * 100,
           park.400 = get(area) %>%
             filter(park <= 400) %>%  nrow() / residences * 100,
           comm.ctr.800 = get(area) %>%
             filter(community_centre <= 800) %>%  nrow() / residences * 100,
           childcare.800 = get(area) %>%
             filter(childcare <= 800) %>%  nrow() / residences * 100,
           kindergarten.800 = get(area) %>%
             filter(kindergarten <= 800) %>%  nrow() / residences * 100,
           primary.800 = get(area) %>%
             filter(primary <= 800) %>%  nrow() / residences * 100,
           comm.health.800 = get(area) %>%
             filter(community_health <= 800) %>%  nrow() / residences * 100,
           mat.child.health.800 = get(area) %>%
             filter(maternal_child_health <= 800) %>%  nrow() / residences * 100,
           gp.400 = get(area) %>%
             filter(gp <= 400) %>%  nrow() / residences * 100,
           dentist.800 = get(area) %>%
             filter(dentist <= 800) %>%  nrow() / residences * 100
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
             dest.dist == "rest.cafe.400"   ~ "Restaurant or cafe within 400m",
             dest.dist == "bus.400.tram.600.train.800"  ~ 
               "Bus within 400m, tram with 600m or train within 800m",
             dest.dist == "supermarket.800" ~ "Supermarket within 800m",
             dest.dist == "convenience.400" ~ 
               "Convenience store or supermarket within 400m",
             dest.dist == "butcher.800"     ~ "Butcher within 800m",
             dest.dist == "bakery.800"      ~ "Bakery within 800m",
             dest.dist == "pharmacy.800"    ~ "Pharmacy within 800m",
             dest.dist == "post.800"        ~ "Post office within 800m",
             dest.dist == "distsport.800"   ~ "District sports facility within 800m",
             dest.dist == "park.400"        ~ "Local park within 400m",
             dest.dist == "comm.ctr.800"    ~ "Community centre within 800m",
             dest.dist == "childcare.800"   ~ "Childcare centre within 800m",
             dest.dist == "kindergarten.800"  ~ "Kindergarten within 800m",
             dest.dist == "primary.800"     ~ "Primary school within 800m",
             dest.dist == "comm.health.800" ~ "Community health centre within 800m",
             dest.dist == "mat.child.health.800"  ~ 
               "Maternal & child health centre within 800m",
             dest.dist == "gp.400"          ~ "GP within 400m",
             dest.dist == "dentist.800"     ~ "Dentist within 800m",
             dest.dist == "residences"      ~ "Number of residences"
           )) %>%
    
    # move text column to front
    relocate(dest.dist)
  
  return(area.coverage)
  
}
