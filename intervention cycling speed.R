# reduce speed on residential streets to 40 km/h, for 20mn intervention

# 1 Setup ----
#------------------------------------------------------------------------------#
## 1.1 Libraries ----
## ------------------------------------#
library(dplyr)
library(fs)
library(sf)

## 1.2 Functions ----
## ------------------------------------#
dir_walk(path = "./functions/", source, recurse = T, type = "file")


## 1.3 Load network edges ----
## ------------------------------------#
# TO DO - work out which actual network we need.  Probably no need for nodes,
# unless writing back to a combined sqlite file

# links <- st_read("../data/processed/network.sqlite", layer = "links")
# nodes <- st_read("../data/processed/network.sqlite", layer = "nodes")

links <- st_read("../data/processed/edgesMelbourne.gpkg")
# nodes <- st_read("../data/processed/nodesMelbourne.gpkg")


# 2 Reduce residential speed to 40 km/h ----
#------------------------------------------------------------------------------#

updated.links <- links %>%
  # freespeed (metres per second) dropped to 40 km/h
  mutate(freespeed = case_when(
    highway %in% c("residential", "living_street", "service") &
      freespeed > 40/3.6              ~ 40/3.6, 
    TRUE                              ~ freespeed)) %>%
  # maxspeed (freespeed, in metres per second, converted to miles per hour)
  mutate(maxspeed = freespeed * 2.23694)

# re-check https://github.com/jibeproject/networkMelbourne/blob/02af3c85bac99f4ae99d174d15ab13b4a4cb3ec7/adjustNetwork.R#L92
# which is currently rounding maxspeed - but the field in links didn't seem to be rounded
#   mutate(maxspeed = round(freespeed * 2.23694)) %>%

# may not be final list of those requiring change - see also
# avg_wdt_mt, avg_wdt_mp, avg_wt, bffrdst and speedKPH



# save output to required location - TO DO - CHECK FINAL LOCATION
st_write(updated.links, "../output/edgesMelbourneUpdatedSpeed.gpkg")

