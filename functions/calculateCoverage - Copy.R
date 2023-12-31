# calculate overall area coverage as percentage of residences with access to 
# each destination type within specified distance for Greater Melbourne, 
# all NACs, and large, medium and small NACs

calculateCoverage <- function(residential.addresses,
                              address.destination.distances,
                              nac.catchments) {
  
  # residential.addresses = residential.addresses
  # address.destination.distances = baseline.distances
  # nac.catchments = nac.catchments
  
  # join residential addresses and distances
  residential.distances <- residential.addresses %>%
    left_join(address.destination.distances, by = "id")
  
  # global areas (melb, NACs, and NACs by size)
  melb <- residential.distances
  
  NAC <- residential.distances %>%
    st_filter(st_union(nac.catchments), .predicate = st_intersects)
  
  large <- residential.distances %>%
    st_filter(st_union(nac.catchments %>%
                         filter(size == "large")),
              .predicate = st_intersects)
  
  medium <- residential.distances %>%
    st_filter(st_union(nac.catchments %>%
                         filter(size == "medium")),
              .predicate = st_intersects)
  
  small <- residential.distances %>%
    st_filter(st_union(nac.catchments %>%
                         filter(size == "small")),
              .predicate = st_intersects)
  
  areas = c("melb", "NAC", "large", "medium", "small")
  
  # build table - TO EXPAND FOR OTHER DESTINATIONS WHEN DONE
  area.coverage <- data.frame(area = areas) %>%
    # get number of residences within specified distance of each destination
    rowwise() %>%
    mutate(residences = get(area) %>% nrow(),
           rest.cafe.400 = get(area) %>%
             filter(restaurant_cafe <= 400) %>%  nrow() / residences * 100,
           bus.400.tram.600 = get(area) %>%  # EXPAND TO TRAIN
             filter(bus <= 400 | tram <= 600) %>%  nrow() / residences * 100,
           supermarket.800 = get(area) %>%
             filter(supermarket <= 800) %>%  nrow() / residences * 100,
           baker.800 = get(area) %>%
             filter(baker <= 800) %>%  nrow() / residences * 100,
           pharmacy.800 = get(area) %>%
             filter(pharmacy <= 800) %>%  nrow() / residences * 100,
           post.800 = get(area) %>%
             filter(post <= 800) %>% nrow() / residences * 100,
           park.400 = get(area) %>%
             filter(park <= 400) %>%  nrow() / residences * 100,
           comm.ctr.800 = get(area) %>%
             filter(community_centre <= 800) %>%  nrow() / residences * 100,
           childcare.800 = get(area) %>%
             filter(childcare <= 800) %>%  nrow() / residences * 100,
           primary.800 = get(area) %>%
             filter(primary <= 800) %>%  nrow() / residences * 100,
           comm.health.800 = get(area) %>%
             filter(community_health <= 800) %>%  nrow() / residences * 100,
           mat.child.health.800 = get(area) %>%
             filter(maternal_child_health <= 800) %>%  nrow() / residences * 100
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
    
    # add new text column, based on row names - EXPAND FOR NEW DESTINATIONS WHEN AVAILABLE
    mutate(dest.dist = row.names(.),
           dest.dist = case_when(
             dest.dist == "rest.cafe.400"   ~ "Restaurant or cafe within 400m",
             dest.dist == "bus.400.tram.600"  ~ "Bus within 400m or tram with 600m (to extend to train 800m)",
             dest.dist == "supermarket.800" ~ "Supermarket within 800m",
             dest.dist == "baker.800"       ~ "Bakery within 800m",
             dest.dist == "pharmacy.800"    ~ "Pharmacy within 800m",
             dest.dist == "post.800"        ~ "Post office within 800m",
             dest.dist == "park.400"        ~ "Local park within 400m",
             dest.dist == "comm.ctr.800"    ~ "Community centre within 800m",
             dest.dist == "childcare.800"   ~ "Childcare centre within 800m",
             dest.dist == "primary.800"     ~ "Primary school within 800m",
             dest.dist == "comm.health.800" ~ "Community health centre within 800m",
             dest.dist == "mat.child.health.800"  ~ "Maternal & child health centre within 800m",
             dest.dist == "residences"      ~ "Number of residences"
           )) %>%
    
    # move text column to front
    relocate(dest.dist)
  
  return(area.coverage)
  
}
