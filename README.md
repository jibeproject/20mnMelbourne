This repository contains code for the 20 minute neighbourhood intervention.  The intervention has two components:
- the destinations component, which adds community destination locations in and around defined activity centres ('ACs').
- the cycling component, which reduces cycling speed on residential streets.

# Overview
There are 4 main script files.
- `baseline.R` assesses the baseline status of access to destinations.
- `intervention destination.R` identifies locations for additional community destinations in and around ACs.
- `analysis.R` undertakes an accessibility analysis and an underutilisation analysis of the results of adding the additional destination locations.
- `intervention cycling speed.R` alters the network by reducing cycling speed on residential streets.

# Input files
The code requires the following input files, which are available [to authorised users] at [*insert location when known*].  The code assumes that the input files are located in a `data` directory ("../data/") which sits beside the directory in which the script files are located .

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/original/MICLUP-NACs.zip* |Activity Centres: location of activity centres in and around which new destinations will be located |
|*data/processed/region.sqlite* |Region: the study area where the intervention is to take place |
|*data/processed/region_buffer.sqlite* |Region, buffered: the region, plus a buffer to limit edge effects |
|*data/processed/edgesMelbourne.gpkg* & *data/processed/nodesMelbourne.gpkg* |Edges and nodes making up the road network |
|*data/original/VIC_ADDRESS_DEFAULT_GEOCODE_psv.psv* |Geocoded National Address File (GNAF) points for Victoria |
|*data/original/1270055001_mb_2016_vic_shape.zip* |ABS 2016 census meshblocks for Victoria|
|*data/original/2016 census mesh block counts.csv* |ABS 2016 census meshblocks population and dwelling counts|
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
|*ac_catchment_addresses_boundary.rds* |IDs of the residential addresses that comprise each Activity Centre catchment, under an alternative approach with larger AC catchments |
|*ac_catchment_polygons.sqlite* |Polygons surrounding the residential addresses that comprise the Activity Centre catchments |
|*ac_catchment_polygons.sqlite_boundary* |Polygons surrounding the residential addresses that comprise the Activity Centre catchments, under an alternative approach with larger AC catchments |
|*node_distances_baseline_walk.csv* |Distance from each residential address node to the nearest destination of each type |
|*baseline assessment.xlsx* |Results of the baseline assessment, as described below |

The `baseline assessment.xlsx` file contains the following tables.
| Table              | Content                                                  |
|--------------------|----------------------------------------------------------|
|*area coverage pop* |Overall area coverage as percentage of population with access to each destination type within specified distance for Greater Melbourne, all ACs, and large, medium and small ACs |
|*area coverage dwel*|Same, but as percentage of dwellings with access rather than population |
|*AC coverage pop*   |Percentage of population in each AC with access to each destination type within specified walking distance |
|*AC coverage dwel*  |Same, but as percentage of dwellings with access rather than population |
|*AC coverage boundary*|Same, but as percentage of population with access based on AC catchments measured from AC boundaries rather than AC anchors |
|*AC coverage summ pop*|Summary of number and percentage of all, large, medium and small ACs with 80% of population with access to each destination type within specified distance |
|*AC coverage summ dwel*|Same, but as number and percentage of dwellings rather than population |
|*AC coverage summ boundary*|Same, but as number and percentage of population with access based on AC catchments measured from AC boundaries rather than AC anchors |
|*AC shortfall comp*  |Comparison table of the numbers of ACs failing to meet the 80% target from each of the 'pop', 'dwel' and 'boundary' summary tables |
|*AC shortfall comp grp*|Breakdown of the percentages of ACs failing to meet the 80% target for each of the 'pop' and 'boundary' methods, by inner/middle/outer LGA groups|

#### `intervention destinations.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*intervention locations.sqlite* |Locations of the new destinations placed in and around ACs |
|*intervention locations small first.sqlite* |Same, but trialling the processing of small ACs first |
|*intervention locations large first.sqlite* |Same, but trialling the processing of large ACs first |
|*intervention tables.xlsx* |Tables relating to the placement of new destinations, as described below |

The `intervention tables.xlsx` file contains the following tables.
| Table              | Content                                                  |
|--------------------|----------------------------------------------------------|
|*added destinations*|Number of added destinations of each type, joined to baseline results for numbers of ACs meeting, or not meeting, the 80% target for each destination type |
|*order comparison*  |Numbers of added destinations under each of the 'small first' and 'large first' approaches|


#### `analysis.R` [TO BE UPDATED]
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
|*SA2 accessibility scores walk.csv* |Walking accessibility scores, summarised by SA2 |
|*SA2 accessibility scores cycle.csv* |Cycling accessibility scores, summarised by SA2 |
|*intervention_destinations_with_dwellings.sqlite* |Number of residential addresses in catchment of each new destination location |
|*underutilisation LGA.csv* |Underutilisation scores for each destination type and local government area |
|*underutilisation SA2.csv* |Underutilisation scores for each destination type and SA2 |


#### `intervention cycling speed.R`[TO BE UPDATED]
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*edgesMelbourneUpdatedSpeed.gpkg* |Road network edges with updated speeds      |

