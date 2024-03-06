This repository contains code for the 20 minute neighbourhood intervention.  The intervention has two components:
- the destinations component, which adds community destination locations in and around defined activity centres.
- the cycling component, which reduces cycling speed on residential streets.

# Overview
There are 4 main files.
- `baseline.R` assesses the baseline status of access to destinations.
- `intervention destination.R` identifies locations for additional community destinations in and around activity centres.
- `analysis.R` undertakes an accessibility analysis and an underutilisation analysis of the results of adding the additional destination locations.
- `intervention cycling speed.R` alters the network by reucing cycling speed on residential streets.

# Input files
The code requires the following input files, which are available [to authorised users] at [*insert location when known*].  The code assumes that the files are located in a `data` directory which sits beside the directory in which the files are located ("../data/").

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/original/MICLUP-NACs.zip* |Activity Centres: location of activity centres in and around which new destinations will be located |
|*data/processed/region.sqlite* |Region: the study area where the intervention is to take place |
|*data/processed/region_buffer.sqlite* |Region, buffered: the region, plus a buffer to limit edge effects |
|*data/processed/edgesMelbourne.gpkg* & *data/processed/nodesMelbourne.gpkg* |Edges and nodes making up the road network |
|*data/original/VIC_ADDRESS_DEFAULT_GEOCODE_psv.psv* |Geocoded National Address File (GNAF) points for Victoria |
|*data/original/1270055001_mb_2016_vic_shape.zip* |ABS 2016 census meshblocks for Victoria|
|*data/original/1270055001_sa2_2016_aust_shape.zip* |ABS 2016 census SA2s       |
|*data/original/LGAs.zip* |Local government areas for Victoria                  |
|*data/processed/Weighted POIs/poi.gpkg* |Baseline destinations locations for certain destination types |
|*data/processed/ANLS 2018 - Destinations and Public Open Space.gpkg* |Baseline destination locations for public open space and certain other destination types |

# Output files
The code produces the following output files, which are saved to the `output` directory.

#### `baseline.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*residential_addresses.sqlite* |Residential address locations with their nearest network node IDs |
|*ac_catchment_addresses.rds* |IDs of the residential addresses that comprise each Activity Centre catchment |
|*ac_catchment_polygons.sqlite* |Polygons surrounding the residential addresses that comprise the Activity Centre catchments |
|*node_distances_baseline.csv* |Distance from each residential address node to the nearest destination of each type |
|*20mn baseline area coverage.csv* |Coverage as a percentage of residences with access to each destination type within specified distance, summarised for Greater Melbourne and for Activity Centres combined |
|*20mn baseline AC coverage.csv* |Coverage as a percentage of residences with access to each destination type within specified distance, for each Activity Centre |
|*20mn baseline AC coverage summary.csv* |Summary of Activity Centre coverage, by size of Activity Centre |

#### `intervention destinations.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*intervention locations.sqlite* |Locations of the new destinations placed in and around Activity Centres |
|*20mn intervention location summary.csv* |Summary of number of new destinations of each type |

#### `analysis.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*node_distances_baseline.csv* |Baseline walking distance from each residential address node to the nearest destination of each type (created in `baseline.R`)|
|*node_distances_baseline_cycle.csv* |Baseline cycling distance from each residential address node to the nearest destination of each type |
|*node_distances_intervention_walk.csv* |Intervention walking distance from each residential address node to the nearest destination of each type |
|*node_distances_intervention_cycle.csv* |Intervention cycling distance from each residential address node to the nearest destination of each type |
|*dwel accessibility scores walk.csv* |Walking accessibility scores for each residential address |
|*dwel accessibility scores cycle.csv* |Cycling accessibility scores for each residential address |
|*LGA accessibility scores walk.csv* |Walking accessibility scores, summarised by local government area |
|*LGA accessibility scores cycle.csv* |Cycling accessibility scores, summarised by local government area |
|*intervention_destinations_with_dwellings.sqlite* |Number of residential addresses in catchment of each new destination location |
|*underutilisation LGA.csv* |Underutilisation scores for each destination type and local government area |
|*underutilisation SA2.csv* |Underutilisation scores for each destination type and SA2 |


#### `intervention cycling speed.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*edgesMelbourneUpdatedSpeed.gpkg* |Road network edges with updated speeds      |

