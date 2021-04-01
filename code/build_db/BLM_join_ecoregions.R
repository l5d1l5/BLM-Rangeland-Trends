# Merge BLM allotments with EcoRegion 
# Add BLM Field Office IDs and District IDs
rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)

allotment_shapes <- readRDS(file = 'output/BLM_cleaned_shape_sf.RDS')
allotment_info <- allotment_shapes %>% st_drop_geometry()
allotment_centers <- allotment_shapes %>% st_centroid() %>% select(uname)

m2_per_ACRE <- 4046.8564224

ER <- read_sf('data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')

ER <- ER %>% 
  st_transform(crs = 5070)

# Add EPA Ecoregion info to the BLM Allotments 

allotment_centers <- 
  allotment_centers %>% 
  st_join(ER, left = T, join = st_within )

allotment_info <- 
  allotment_centers %>% 
  st_drop_geometry( ) %>% 
  left_join(allotment_info) %>% 
  rowwise() %>% 
  mutate( ADMIN_ST = ifelse( ADMIN_ST == ' ', state.abb[ state.name == STATE_NAME], ADMIN_ST)) %>%
  ungroup() 

# Add BLM District Boundaries and BLM field offices 
lnames <- st_layers('data/BLM_National_Administrative_Units/admu.gdb/')
lnames
# blm_districts <- sf::read_sf('data/BLM_National_Administrative_Units/admu.gdb/', 
#                              layer = 'blm_natl_admu_dist_poly_webpub')

blm_offices <- sf::read_sf('data/BLM_National_Administrative_Units/admu.gdb/', 
                           layer = 'blm_natl_admu_field_poly_webpub')

# blm_districts <- blm_districts %>% filter( ADMIN_ST != 'AK')
blm_offices <- blm_offices %>% filter( ADMIN_ST != 'AK')

blm_offices <- blm_offices %>% st_transform('epsg:4269')
# blm_districts <- blm_districts %>% st_transform('epsg:4269')

blm_offices <- blm_offices %>% st_transform(crs = 5070)

blm_offices <- 
  blm_offices %>% 
  st_simplify(preserveTopology = T, dTolerance = 20) %>% 
  st_make_valid()

# blm_districts <- blm_districts %>% st_transform(crs = 5070)

blm_offices <- 
  blm_offices %>% 
  select( ADM_UNIT_CD, 
          ADMU_NAME, BLM_ORG_TYPE, 
          PARENT_CD, PARENT_NAME, ADMIN_ST)

# Allotment office codes are inconsistent with field office codes in some cases. 
# Do spatial join to connect allotments with field office names and BLM districts. 

allotment_centers <- 
  allotment_centers  %>% 
  select(uname) %>%
  st_join(blm_offices, join = st_within)

allotment_info <- 
  allotment_centers %>% 
  st_drop_geometry() %>%
  left_join(allotment_info, by = 'uname')

allotment_info <- 
  allotment_info %>%
  group_by( uname ) %>% 
  filter( row_number() == 1  ) %>% # If there are duplicates (AZ) than keep the field office row %>% 
  ungroup()  

stopifnot ( ! any( duplicated(allotment_info ) ) )

allotment_shapes <- 
  allotment_shapes %>%
  select( uname )  %>%
  left_join(allotment_info, by = 'uname')

stopifnot( !any( allotment_shapes %>% duplicated()  ) )

# Write out 
allotment_shapes %>% 
  write_rds(file = 'output/BLM_cleaned_shape_sf.RDS')

unlink('output/BLM_allotments', recursive = T)
dir.create('output/BLM_allotments')

allotment_shapes %>% 
  st_write('output/BLM_allotments/allotments.shp', append = F)

 