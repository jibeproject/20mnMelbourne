# function to calculate mean SA2 utilisation from individual address utilisation scores

calculateSA2UtilisationScores <- function(util, SA2s) {
  
  output <- util %>% 
    
    # convert polygons to centroids
    st_centroid() %>%
    
    # intersect with SA2s %>%
    st_join(., SA2s %>% dplyr::select(SA2_MAIN16), .predicate = st_intersects) %>%
    st_drop_geometry() %>%
    
    # find SA2 average utilisation for each destination type
    group_by(SA2_MAIN16, dest_type) %>%
    summarise(mean_util = mean(utilisation)) %>%
    ungroup() %>%
    
    # arrange for display
    pivot_wider(names_from = dest_type,
                values_from = mean_util) %>%
    
    # unlike LGAs, don't filter at this point - will control display through
    # using a Greater Melbourne mask
    
    # arrange output columns in desired order
    dplyr::select(SA2_MAIN16, supermarket, convenience_store, cafe, pharmacy, post,
                  gp, maternal_child_health, dentist, childcare, kindergarten,
                  primary, community_centre, park, bus)
  
  return(output)
  
}
