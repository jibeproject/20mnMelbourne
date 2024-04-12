# function to calculate LGA accessibility cores, being the average of the 
# accessibility scores for address points within the LGA

# output is filtered to the main 31 Greater Melbourne LGAs

calculateSA2AccessibilityScores <- function(address.scores, mode) {
  
  # address.scores = address.scores.walk
  # mode = "people"
  
  # exit if mode not correctly set
  if (!mode %in% c("people", "dwellings")) {
    print(paste0("Not configured for mode ", mode, "; terminating"))
    return()
  }
  
  # add unit column depending on mode
  if (mode == "people") {
    address.scores <- address.scores %>%
      mutate(unit = pop_wt)
  } else if (mode == "dwellings") {
    address.scores <- address.scores %>%
      mutate(unit = dwel_wt)
  }
  
  output <- address.scores %>%
    group_by(SA2_MAIN16) %>%
    summarise(
      # single point scores
      score_single_hard_base = weighted.mean(score_single_hard_base, w = unit),
      score_single_hard_int = weighted.mean(score_single_hard_int, w = unit),
      score_single_hard_diff = score_single_hard_int - score_single_hard_base,
      
      score_single_soft_base = weighted.mean(score_single_soft_base, w = unit),
      score_single_soft_int = weighted.mean(score_single_soft_int, w = unit),
      score_single_soft_diff = score_single_soft_int - score_single_soft_base,

      # weighted scores
      score_wt_hard_base = weighted.mean(score_wt_hard_base, w = unit),
      score_wt_hard_int = weighted.mean(score_wt_hard_int, w = unit),
      score_wt_hard_diff = score_wt_hard_int - score_wt_hard_base,

      score_wt_soft_base = weighted.mean(score_wt_soft_base, w = unit),
      score_wt_soft_int = weighted.mean(score_wt_soft_int, w = unit),
      score_wt_soft_diff = score_wt_soft_int - score_wt_soft_base,

      # weighted with multiple scores
      score_wt_mult_hard_base = weighted.mean(score_wt_mult_hard_base, w = unit),
      score_wt_mult_hard_int = weighted.mean(score_wt_mult_hard_int, w = unit),
      score_wt_mult_hard_diff = score_wt_mult_hard_int - score_wt_mult_hard_base,

      score_wt_mult_soft_base = weighted.mean(score_wt_mult_soft_base, w = unit),
      score_wt_mult_soft_int = weighted.mean(score_wt_mult_soft_int, w = unit),
      score_wt_mult_soft_diff = score_wt_mult_soft_int - score_wt_mult_soft_base
    ) %>%
    
    ungroup()
    
  return(output)
}
