# function to add a 'group' column to LGAs indicating whether they are inner 
# (generally up to 10km from CBD), middle (10-20km) or outer (>20km)

classifyLGAs <- function(LGAs) {
  return(LGAs %>%
           
    mutate(group = case_when(
      NAME %in% toupper(c("Maribyrnong", "Melbourne", "Port Phillip",
                          "Stonnington", "Yarra")) ~ "Inner",
      NAME %in% toupper(c("Banyule", "Bayside", "Boroondara", "Brimbank", "Darebin", 
                          "Glen Eira", "Greater Dandenong", "Hobsons Bay",  
                          "Manningham", "Merri-bek", "Monash", "Moonee Valley", 
                          "Whitehorse")) ~ "Middle",
      NAME %in% toupper("Kingston") & STATE == "VIC" ~ "Middle",  # there is also a Kingston in SA
      TRUE ~ "Outer")))
  
}
