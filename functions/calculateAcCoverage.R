# find percentage of residences in each AC with access to each destination type 
# within specified walking distance

calculateAcCoverage <- function(address.destination.distances,
                                 ac.catchments,
                                 ACs) {
  
  # address.destination.distances = baseline.distances
  # ac.catchment.addresses = ac.catchment.addresses
  # ACs = ACs
  
  # # join residential addresses and distances
  # residential.distances <- residential.addresses %>%
  #   left_join(address.destination.distances, by = "id")
  
  # join AC catchment addresses and sizes
  ac.catchment.addresses <- ac.catchment.addresses %>%
    left_join(ACs %>% 
                st_drop_geometry() %>%
                dplyr::select(CENTRE_NO, size), by = "CENTRE_NO")
  
  
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
              output.row <- data.frame(
                centre_no = ac.catchment.addresses$CENTRE_NO[[i]],
                size = ac.catchment.addresses$size[[i]],
                supermarket.800 = ac.distances %>%
                  filter(supermarket <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                pharmacy.800 = ac.distances %>%
                  filter(pharmacy <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                post.800 = ac.distances %>%
                  filter(post <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                gp.800 = ac.distances%>%
                  filter(gp <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                mat.child.health.800 = ac.distances%>%
                  filter(maternal_child_health <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                dentist.800 = ac.distances%>%
                  filter(dentist <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                childcare.800 = ac.distances %>%
                  filter(childcare <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                kindergarten.800 = ac.distances %>%
                  filter(kindergarten <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                primary.800 = ac.distances %>%
                  filter(primary <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                comm.library.800 = ac.distances %>%
                  filter(community_centre_library <= 800) %>%
                  nrow() / nrow(ac.distances) * 100,
                convenience.400 = ac.distances %>%
                  filter(supermarket <= 400 | convenience_store <= 400) %>%
                  nrow() / nrow(ac.distances) * 100,
                rest.cafe.400 = ac.distances %>%
                  filter(restaurant_cafe <= 400) %>%
                  nrow() / nrow(ac.distances) * 100,
                park.400 = ac.distances %>%
                  filter(park <= 400) %>%
                  nrow() / nrow(ac.distances) * 100,
                bus.400.tram.600.train.800 = ac.distances %>% 
                  filter(bus <= 400 | tram <= 600 | train <= 800) %>%
                  nrow() / nrow(ac.distances) * 100
              )
              
              return(output.row)
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  return(AC.pct.coverage)
  
}
