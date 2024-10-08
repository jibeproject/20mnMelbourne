# assemble files from temporary catchment folders (created by 
# makeAcCatchments.R) into output files

assembleCatchmentOutputs <- function(temp.address.location,
                                     temp.polygon.location) {
  
  # combine individual address and polygon outputs  
  # -----------------------------------#
  
  # combine all the address and polygon files saved to 'catchments' directories
  
  print(paste(Sys.time(), "|", "combining outputs for individual ACs"))
  
  # vectors to hold all  variables
  address.nos <- c()
  poly.nos <- c()
  
  
  # read in all the files in 'catchments' folders, with each given a name from the
  # file minus .rds (that is, '1.rds' read in as '1', etc - note that
  # the numbers are the centre_no's)
  
  # read in addresses
  for (i in 1:length(list.files(temp.address.location))) {
    # create the ac number variable (eg '1.rds' is '1_address')
    var.name <- paste0(gsub(".rds", "", list.files(temp.address.location)[i]),
                       "_address")
    # read in the rds file and assign it to the variable
    assign(var.name,
           readRDS(paste0(temp.address.location, "/", 
                          list.files(temp.address.location)[i])))
    # add the variable name to the vector
    address.nos <- c(address.nos, var.name)
    
  }
  
  # read in polygons
  for (i in 1:length(list.files(temp.polygon.location))) {
    # create the ac number variable (eg '1.rds' is '1_poly')
    var.name <- paste0(gsub(".rds", "", list.files(temp.polygon.location)[i]),
                       "_poly")
    # read in the rds file and assign it to the variable
    assign(var.name,
           readRDS(paste0(temp.polygon.location, "/", 
                          list.files(temp.polygon.location)[i])))
    # add the variable name to the vector
    poly.nos <- c(poly.nos, var.name)
    
  }
  
  
  # combine the individual files
  
  # combine addresses
  # begin with the first variable (with 'CENTRE_NO' as numeric part of 
  # the name, eg 'poly_29' becomes '29')
  ac.catchment.addresses <- cbind(CENTRE_NO = as.numeric(gsub("\\D", "", poly.nos[1])),
                                  address_ids = list(get(address.nos[1])))
  # then add the others
  for (i in 2:length(address.nos)) {
    ac.catchment.addresses <- rbind(ac.catchment.addresses,
                                    cbind(CENTRE_NO = as.numeric(gsub("\\D", "", poly.nos[i])),
                                          address_ids = list(get(address.nos[i]))))
  }
  
  # convert CENTRE_NO column to numeric
  ac.catchment.addresses <- as.data.frame(ac.catchment.addresses) %>%
    mutate(CENTRE_NO = as.numeric(CENTRE_NO))
  
  # combine polygons
  # begin with the first variable (with 'CENTRE_NO' as numeric part of 
  # the name, eg 'poly_29' becomes '29')
  ac.catchment.polygons <- get(poly.nos[1]) %>%
    mutate(CENTRE_NO = gsub("\\D", "", poly.nos[1]))
  # then add the others
  for (i in 2:length(poly.nos)) {
    ac.catchment.polygons <- 
      rbind(ac.catchment.polygons, 
            get(poly.nos[i]) %>%
              mutate(CENTRE_NO = gsub("\\D", "", poly.nos[i])))
    
  }
  # convert CENTRE_NO column to numeric
  ac.catchment.polygons <- ac.catchment.polygons %>%
    mutate(CENTRE_NO = as.numeric(CENTRE_NO))
  
  
  # # NOTE - TO ACCESS THE ADDRESS LIST FOR AN INDIVIDUAL AC:
  # indiv.ac.add <- ac.catchment.addresses %>%
  #   filter(CENTRE_NO == 29) %>%
  #   .$address_ids %>%
  #   unlist()
  
  return(list(ac.catchment.addresses, ac.catchment.polygons))
  
  
}
