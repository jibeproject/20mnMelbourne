# function to calculate LGA accessibility cores, being the average of the 
# accessibility scores for address points within the LGA

# output is filtered to the main 31 Greater Melbourne LGAs

calculateLgaAccessibilityScores <- function(address.scores) {
  output <- address.scores %>%
    group_by(LGA, NAME) %>%
    summarise(
      # single point scores
      score_single_hard_base = mean(score_single_hard_base),
      score_single_hard_int = mean(score_single_hard_int),
      score_single_hard_diff = score_single_hard_int - score_single_hard_base,
      
      score_single_soft_base = mean(score_single_soft_base),
      score_single_soft_int = mean(score_single_soft_int),
      score_single_soft_diff = score_single_soft_int - score_single_soft_base,
      
      # weighted scores
      score_wt_hard_base = mean(score_wt_hard_base),
      score_wt_hard_int = mean(score_wt_hard_int),
      score_wt_hard_diff = score_wt_hard_int - score_wt_hard_base,
      
      score_wt_soft_base = mean(score_wt_soft_base),
      score_wt_soft_int = mean(score_wt_soft_int),
      score_wt_soft_diff = score_wt_soft_int - score_wt_soft_base,
      
      # weighted with multiple scores
      score_wt_mult_hard_base = mean(score_wt_mult_hard_base),
      score_wt_mult_hard_int = mean(score_wt_mult_hard_int),
      score_wt_mult_hard_diff = score_wt_mult_hard_int - score_wt_mult_hard_base,
      
      score_wt_mult_soft_base = mean(score_wt_mult_soft_base),
      score_wt_mult_soft_int = mean(score_wt_mult_soft_int),
      score_wt_mult_soft_diff = score_wt_mult_soft_int - score_wt_mult_soft_base
    ) %>%
    
    ungroup() %>%
    
    # filter to the main 31 Greater Melbourne LGAs
    filter(LGA %in% c("Banyule", "Bayside", "Boroondara", "Brimbank", "Cardinia",
                      "Casey", "Darebin", "Frankston", "Glen Eira", "Greater Dandenong",
                      "Hobsons Bay", "Hume", "Kingston", "Knox", "Manningham",
                      "Maribyrnong", "Maroondah", "Melbourne", "Melton", "Merri-bek",
                      "Monash", "Moonee Valley", "Mornington Peninsula", "Nillumbik", "Port Phillip",
                      "Stonnington", "Whitehorse", "Whittlesea", "Wyndham", "Yarra",
                      "Yarra Ranges"))
  
  return(output)
}
