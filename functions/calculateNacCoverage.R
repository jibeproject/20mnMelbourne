# find percentage of residences in each NAC with access to each destination type 
# within specified walking distance

calculateNacCoverage <- function(address.destination.distances,
                                 nac.catchments,
                                 NACs) {
  
  # address.destination.distances = baseline.distances
  # nac.catchment.addresses = nac.catchment.addresses
  # NACs = NACs
  
  # # join residential addresses and distances
  # residential.distances <- residential.addresses %>%
  #   left_join(address.destination.distances, by = "id")
  
  # join nac catchment addresses and sizes
  nac.catchment.addresses <- nac.catchment.addresses %>%
    left_join(NACs %>% 
                st_drop_geometry() %>%
                dplyr::select(CENTRE_NO, size), by = "CENTRE_NO")
  
  
  # parallel loop
  # setup for parallel processing - detect no of available cores and create cluster
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  
  # set up progress reporting 
  pb <- txtProgressBar(max = nrow(nac.catchment.addresses), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report progress
  print(paste(Sys.time(), "|", 
              "Finding % of residences within walking distance of destinations for",
              nrow(nac.catchment.addresses), "NACs; parallel processing with", cores, "cores"))
  
  # loop to percentage of residences within walking distance in each NAC
  NAC.pct.coverage <-
    foreach(i = 1:nrow(nac.catchment.addresses),
            # foreach(i = 1:10,
            .combine = rbind,
            .packages = c("dplyr", "sf"), 
            .options.snow = opts) %dopar% {
              
              # distances for addresses that make up the NAC
              nac.distances <- address.destination.distances %>%
                filter(id %in% nac.catchment.addresses$address_ids[[i]])
              
              # summary of distances for the NAC
              output.row <- data.frame(
                centre_no = nac.catchment.addresses$CENTRE_NO[[i]],
                size = nac.catchment.addresses$size[[i]],
                rest.cafe.400 = nac.distances %>%
                  filter(restaurant_cafe <= 400) %>%
                  nrow() / nrow(nac.distances) * 100,
                bus.400.tram.600.train.800 = nac.distances %>% 
                  filter(bus <= 400 | tram <= 600 | train <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                supermarket.800 = nac.distances %>%
                  filter(supermarket <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                convenience.400 = nac.distances %>%
                  filter(supermarket <= 400 | convenience_store <= 400) %>%
                  nrow() / nrow(nac.distances) * 100,
                butcher.800 = nac.distances %>%
                  filter(butcher <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                bakery.800 = nac.distances %>%
                  filter(bakery <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                pharmacy.800 = nac.distances %>%
                  filter(pharmacy <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                post.800 = nac.distances %>%
                  filter(post <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                distsport.800 = nac.distances %>%
                  filter(district_sport <= 800) %>%  
                  nrow() / nrow(nac.distances) * 100,
                park.400 = nac.distances %>%
                  filter(park <= 400) %>%
                  nrow() / nrow(nac.distances) * 100,
                comm.ctr.800 = nac.distances %>%
                  filter(community_centre <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                childcare.800 = nac.distances %>%
                  filter(childcare <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                kindergarten.800 = nac.distances %>%
                  filter(kindergarten <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                primary.800 = nac.distances %>%
                  filter(primary <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                comm.health.800 = nac.distances %>%
                  filter(community_health <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                mat.child.health.800 = nac.distances%>%
                  filter(maternal_child_health <= 800) %>%
                  nrow() / nrow(nac.distances) * 100,
                gp.400 = nac.distances%>%
                  filter(gp <= 400) %>%
                  nrow() / nrow(nac.distances) * 100,
                dentist.800 = nac.distances%>%
                  filter(dentist <= 800) %>%
                  nrow() / nrow(nac.distances) * 100
              )
              
              return(output.row)
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  return(NAC.pct.coverage)
  
}
