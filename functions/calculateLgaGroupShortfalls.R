# function to calculate the percentage, by inner/middle/outer LGA groups, of
# ACs not meeting the target of with 80% of residences with 
# access to each destination type within specified distance

calculateLgaGroupShortfalls <- function(AC.coverage) {
  
  # AC.coverage = baseline.AC.coverage.pop

  inner <- AC.coverage %>% filter(group == "Inner")
  middle <- AC.coverage %>% filter(group == "Middle")
  outer <- AC.coverage %>% filter(group == "Outer")
  groups <- c("inner", "middle", "outer")
  
  for (i in 1:length(groups)) {
    # get the group
    group <- get(groups[i])
    
    # no of ac catchments in the group
    n = nrow(group)
    
    group.name  = groups[i]
    
    # summarise the group
    summary_group <- group %>%
      
      # number of  ACs satifying 80% condition for each destination
      dplyr::select(-centre_no, -size, -NAME, -group) %>%
      # replace NAs with 0
      mutate_all(~ ifelse(is.na(.), 0, .)) %>%
      # counts - note that sum(condition) is no of rows satisfying condition,
      # so it's the no of rows NOT MEETING the 80% test
      summarise(across(everything(), ~sum(. < 80))) %>%
      # add n column
      mutate(acs = n) %>%
      # transpose and add column name ('t' sets it to 'V1')
      t() %>%
      as.data.frame() %>%
      rename(no = "V1") %>%
      
      # percentages of ACs NOT satifying 80% condition for each destination
      mutate(pct = no / n * 100) %>%
      
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
               dest.dist == "acs"             ~ "Number of ACs"
             )) %>%
      
      # move text column to front
      relocate(dest.dist)
    
    # set column.names
    colnames(summary_group) <- c("dest.dist",
                                 paste0(groups[i], ".no"),
                                 paste0(groups[i], ".pct"))
    
    # bind into summary dataframe
    if (i == 1) {
      AC.summary <- summary_group
      row.names <- row.names(AC.summary)
    } else {
      AC.summary <- left_join(AC.summary, summary_group, by = "dest.dist")
      
    }
    
  }
  
  return(AC.summary)
  
}
