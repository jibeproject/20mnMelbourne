# function to calculate mean LGA utilisation from individual address utilisation scores

calculateLgaUtilisationScores <- function(util, LGAs) {
  
  output <- util %>%
    
    # convert polygons to centroids
    st_centroid() %>%
    
    # intersect with LGAs %>%
    st_join(., classifyLGAs(LGAs) %>% dplyr::select(NAME, group), .predicate = st_intersects) %>%
    mutate(LGA = case_when(NAME == "MERRI-BEK" ~ "Merri-bek",
                           TRUE ~ str_to_title(NAME))) %>%
    st_drop_geometry() %>%
    
    # find LGA average utilisation for each destination type
    group_by(NAME, group, LGA, dest_type) %>%
    summarise(mean_util = mean(utilisation)) %>%
    ungroup() %>%
    
    # arrange for display
    pivot_wider(names_from = dest_type,
                values_from = mean_util) %>%
    
    # filter to the main 31 Greater Melbourne LGAs
    filter(LGA %in% c("Banyule", "Bayside", "Boroondara", "Brimbank", "Cardinia",
                      "Casey", "Darebin", "Frankston", "Glen Eira", "Greater Dandenong",
                      "Hobsons Bay", "Hume", "Kingston", "Knox", "Manningham",
                      "Maribyrnong", "Maroondah", "Melbourne", "Melton", "Merri-bek",
                      "Monash", "Moonee Valley", "Mornington Peninsula", "Nillumbik", "Port Phillip",
                      "Stonnington", "Whitehorse", "Whittlesea", "Wyndham", "Yarra",
                      "Yarra Ranges")) %>%
    
    # arrange output columns in desired order
    dplyr::select(NAME, group, LGA, supermarket, convenience_store, cafe, pharmacy, 
                  post, gp, maternal_child_health, dentist, childcare, 
                  kindergarten, primary, community_centre, park, bus) %>%
    
    # arrange rows
    arrange(group, LGA)
  
  return(output)
  
}
