# functions to calculate accessibility scores, with both hard and soft thresholds

# hard score function - one point if within distance
score.hard <- function(dist, threshold) {
  return(ifelse(dist <= threshold, 1, 0))
}

# logistic decay function for soft threshold scores - up to one point (will be 0.5 if equal to distance)
# Higgs et al, The Urban Liveability Index: developing a policy-relevant urban
# liveability composite measure and evaluating associations with transport mode choice
# https://ij-healthgeographics.biomedcentral.com/articles/10.1186/s12942-019-0178-8
score.soft <- function(dist, threshold) {
  return(1 / (1 + exp(5 * (dist - threshold) / threshold)))
}

# function to allocate scores to dataframe of distances, using hard or soft scorefunction
scores <- function(distances, scorefunction, thresholds) {
  
  # if thresholds is c(400, 600, 800), then threshold[1] is 400,
  # threshold[2] is 600 and threshold[3] is 800
  
  output <- distances %>%
    rowwise() %>%
    mutate(
      # a single point for each destination type
      score_single = scorefunction(supermarket, thresholds[3]) +
        max(scorefunction(convenience_store, thresholds[1]), 
            scorefunction(supermarket, thresholds[1])) +
        scorefunction(restaurant_cafe, thresholds[1]) +
        scorefunction(pharmacy, thresholds[3]) +
        scorefunction(post, thresholds[3]) +
        scorefunction(gp, thresholds[3]) +
        scorefunction(maternal_child_health, thresholds[3]) +
        scorefunction(dentist, thresholds[3]) +
        scorefunction(childcare, thresholds[3]) +
        scorefunction(kindergarten, thresholds[3]) +
        scorefunction(primary, thresholds[3]) +
        scorefunction(community_centre_library, thresholds[3]) +
        scorefunction(park, thresholds[1]) +
        max(scorefunction(bus, thresholds[1]),
            scorefunction(tram, thresholds[2]),
            scorefunction(train, thresholds[3])),
      
      # weighted destination type values
      score_wt = scorefunction(supermarket, thresholds[3]) * 5 +
        max(scorefunction(convenience_store, thresholds[1]), 
            scorefunction(supermarket, thresholds[1])) * 3 +
        scorefunction(restaurant_cafe, thresholds[1]) * 2 +
        scorefunction(pharmacy, thresholds[3]) * 3 +
        scorefunction(post, thresholds[3]) * 2 +
        scorefunction(gp, thresholds[3]) * 3 +
        scorefunction(maternal_child_health, thresholds[3]) +
        scorefunction(dentist, thresholds[3]) +
        scorefunction(childcare, thresholds[3]) +
        scorefunction(kindergarten, thresholds[3]) +
        scorefunction(primary, thresholds[3]) +
        scorefunction(community_centre_library, thresholds[3]) +
        scorefunction(park, thresholds[1]) +
        max(scorefunction(bus, thresholds[1]),
            scorefunction(tram, thresholds[2]),
            scorefunction(train, thresholds[3])),
      
      # weighted, with extra points for extra restaurants, up to 4
      score_wt_mult = score_wt +
        scorefunction(restaurant_cafe_2, thresholds[1]) * 2 +
        scorefunction(restaurant_cafe_3, thresholds[1]) * 2 +
        scorefunction(restaurant_cafe_4, thresholds[1]) * 2) %>%
    
    ungroup() %>%
    
    dplyr::select(node_id, score_single, score_wt, score_wt_mult)
  
  return(output)
  
}

# function to calculate and combine hard and soft scores
calculateAccessibilityScores <- function(distances, mode) {
  
  if (mode == "walk") {
    thresholds <- c(400, 600, 800)
  } else if (mode == "cycle") {
    thresholds <- c(1000, 1500, 2000)
  } else {
    message(paste0("Scoring is not configured for the chosen mode '", mode, "'; terminating"))
    return(NULL)
  }
  
  return(scores(distances, score.hard, thresholds) %>%
           left_join(scores(distances, score.soft, thresholds),
                     by = "node_id", suffix = c("_hard", "_soft")))
}

