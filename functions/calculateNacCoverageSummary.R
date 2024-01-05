# calculate summary of number and percentage of all, large, medium and small
# NACs with 80% of residences with access to each destination type within
# specified distance

calculateNacCoverageSummary <- function(NAC.pct.coverage) {
  
  # NAC.pct.coverage <- baseline.NAC.coverage
  
  all <- NAC.pct.coverage
  large <- NAC.pct.coverage %>% filter(size == "large")
  medium <- NAC.pct.coverage %>% filter(size == "medium")
  small <- NAC.pct.coverage %>% filter(size == "small")
  groups <- c("all", "large", "medium", "small")
  
  for (i in 1:length(groups)) {
    # get the group
    group <- get(groups[i])
    
    # no of nac catchments in the group
    n = nrow(group)
    
    group.name  = groups[i]
    
    # summarise the group
    summary_group <- group %>%
      
      # number of  NACs satifying 80% condition for each destination
      dplyr::select(-centre_no, -size) %>%
      # replace NAs with 0
      mutate_all(~ ifelse(is.na(.), 0, .)) %>%
      # counts - note that sum(condition) is no of rows satisfying condition
      summarise(across(everything(), ~sum(. >= 80))) %>%
      # add n column
      mutate(nacs = n) %>%
      # transpose and add column name ('t' sets it to 'V1')
      t() %>%
      as.data.frame() %>%
      rename(no = "V1") %>%
      
      # percentages of NACs satifying 80% condition for each destination
      mutate(pct = no / n * 100) %>%
      
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
               dest.dist == "nacs"             ~ "Number of NACs"
             )) %>%
      
      # move text column to front
      relocate(dest.dist)
    
    # set column.names
    colnames(summary_group) <- c("dest.dist",
                                 paste0(groups[i], ".no"),
                                 paste0(groups[i], ".pct"))
    
    # bind into summary dataframe
    if (i == 1) {
      NAC.summary <- summary_group
    } else {
      NAC.summary <- left_join(NAC.summary, summary_group)
      
    }
  
  }
  
  # rearrange for display ()
  NAC.summary.output <- NAC.summary %>%
    # keep the pct column for all only
    dplyr::select(dest.dist, large.no, medium.no, small.no, all.no, all.pct) %>%
    # add a shortfall column
    mutate(all.shortfall = nrow(NAC.pct.coverage) - all.no)
    
  return(NAC.summary.output)
  
}
