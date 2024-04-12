# find percentage of residences in each AC with access to each destination type 
# within specified walking distance

calculateAcCoverage <- function(address.destination.distances,
                                ac.catchments,
                                ACs,
                                mode) {
  
  # address.destination.distances = baseline.distances
  # ac.catchment.addresses = ac.catchment.addresses
  # ACs = ACs
  # mode = "people"
  
  # exit if mode not correctly set
  if (!mode %in% c("people", "dwellings")) {
    print(paste0("Not configured for mode ", mode, "; terminating"))
    return()
  }
  
  # join AC catchment addresses and sizes
  ac.catchment.addresses <- ac.catchment.addresses %>%
    left_join(ACs %>% 
                st_drop_geometry() %>%
                dplyr::select(CENTRE_NO, size), by = "CENTRE_NO")
  
  # add unit column depending on mode
  if (mode == "people") {
    address.destination.distances <- address.destination.distances %>%
      mutate(unit = pop_wt)
  } else if (mode == "dwellings") {
    address.destination.distances <- address.destination.distances %>%
      mutate(unit = dwel_wt)
  }

  # parallel loop
  # setup for parallel processing - detect no of available cores and create cluster
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  
  # set up progress reporting 
  pb <- txtProgressBar(max = nrow(ac.catchment.addresses), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report progress
  print(paste(Sys.time(), "|", 
              "Finding % of residences within walking distance of destinations for",
              nrow(ac.catchment.addresses), "ACs; parallel processing with", cores, "cores"))
  
  # loop to percentage of residences within walking distance in each AC
  AC.pct.coverage <-
    foreach(i = 1:nrow(ac.catchment.addresses),
            # foreach(i = 501:1000,
            # foreach(i = 1001:nrow(ac.catchment.addresses),
            .combine = rbind,
            .packages = c("dplyr", "sf"), 
            .options.snow = opts) %dopar% {
              
              # distances for addresses that make up the AC
              ac.distances <- address.destination.distances %>%
                filter(id %in% ac.catchment.addresses$address_ids[[i]])
              
              # summary of distances for the AC
                units = sum(ac.distances$unit)
                
                supermarket = sum(ac.distances %>% filter(supermarket <= 800) %>% .$unit)
                pharmacy = sum(ac.distances %>% filter(pharmacy <= 800) %>% .$unit)
                post = sum(ac.distances %>% filter(post <= 800) %>% .$unit)
                gp = sum(ac.distances %>% filter(gp <= 800) %>% .$unit)
                mat.child.health = sum(ac.distances %>% filter(maternal_child_health <= 800) %>% .$unit)
                dentist = sum(ac.distances %>% filter(dentist <= 800) %>% .$unit)
                childcare = sum(ac.distances %>% filter(childcare <= 800) %>% .$unit)
                kindergarten = sum(ac.distances %>% filter(kindergarten <= 800) %>% .$unit)
                primary = sum(ac.distances %>% filter(primary <= 800) %>% .$unit)
                comm.library = sum(ac.distances %>% filter(community_centre_library <= 800) %>% .$unit)
                convenience = sum(ac.distances %>% filter(supermarket <= 400 | convenience_store <= 400) %>% .$unit)
                rest.cafe = sum(ac.distances %>% filter(restaurant_cafe <= 400) %>% .$unit)
                park = sum(ac.distances %>% filter(park <= 400) %>% .$unit)
                bus.tram.train = sum(ac.distances %>% filter(bus <= 400 | tram <= 600 | train <= 800) %>% .$unit)
                
                output.row <- data.frame(
                  centre_no = ac.catchment.addresses$CENTRE_NO[[i]],
                  size = ac.catchment.addresses$size[[i]],
                  supermarket.800 = supermarket / units * 100,
                  pharmacy.800 = pharmacy / units * 100,
                  post.800 = post / units * 100,
                  gp.800 = gp / units * 100,
                  mat.child.health.800 = mat.child.health / units * 100,
                  dentist.800 = dentist / units * 100,
                  childcare.800 = childcare / units * 100,
                  kindergarten.800 = kindergarten / units * 100,
                  primary.800 = primary / units * 100,
                  comm.library.800 = comm.library / units * 100,
                  convenience.400 = convenience / units * 100,
                  rest.cafe.400 = rest.cafe / units * 100,
                  park.400 = park / units * 100,
                  bus.400.tram.600.train.800 = bus.tram.train / units * 100
                )

              return(output.row)
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  
  return(AC.pct.coverage)
  
}
