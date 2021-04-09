# Find ecoregion for each allotment using spatial join  
# Find BLM Field Office IDs and District IDs using spatial join
rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)

# input -------------------------- # 
allotment_shapes <- readRDS(file = 'output/BLM_cleaned_shape_sf.RDS')
state_bounds <- tigris::states(resolution = '20m')
ER <- read_sf('data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')

m2_per_ACRE <- 4046.8564224

#--------------------------------- # 

# convert to points for faster geospatial join 
allotment_centers <- 
  allotment_shapes %>% 
  st_centroid()

ER <- ER %>% 
  st_transform(crs = st_crs(allotment_shapes))

# 
sel_states <- state.name[ state.abb %in% c('OR', 'WA', 'MT', 
                                           'ID', 'ND', 'SD', 
                                           'UT', 'CO', 'AZ', 
                                           'NM', 'CA', 'NV', 'WY', 'NE') ] 
state_bounds <- 
  state_bounds %>% 
  filter( NAME %in% sel_states ) %>%
  st_transform(crs = st_crs(ER)) 

# Use Level I Ecoregions in all cases except: 
# 1. Break deserts to level II ecoregions: warm deserts and cold deserts
# 2. Lump Southern Semi-Arid Highlands with Temperate Sierras = "S. Mts."

ecoregion_def <- 
  ER %>%
  filter( STATE_NAME %in% sel_states ) %>% 
  st_drop_geometry() %>% 
  mutate( Ecoregion = NA_L1NAME ) %>% 
  mutate( Ecoregion = 
            ifelse( Ecoregion %in% 
                      c('TEMPERATE SIERRAS', 
                        'SOUTHERN SEMI-ARID HIGHLANDS'), 
                    'AZ/NM HIGHLANDS', 
                    Ecoregion)) %>%
  mutate( Ecoregion =  
            ifelse( Ecoregion == 'NORTH AMERICAN DESERTS', 
                    NA_L2NAME, 
                    Ecoregion))  %>%
  mutate( Ecoregion = 
            ifelse( Ecoregion == "COLD DESERTS",
                    NA_L3NAME,
                    Ecoregion)) %>% 
  mutate( Ecoregion = 
            ifelse( Ecoregion %in% 
                      c('Wyoming Basin', 
                        'Colorado Plateaus', 
                        'Arizona/New Mexico Plateau'),
                    'EASTERN COLD DESERTS', 
                    Ecoregion)) %>% 
  mutate( Ecoregion = 
            ifelse( Ecoregion %in% 
                      c( 'Central Basin and Range', 
                         'Northern Basin and Range', 
                         'Columbia Plateau', 
                         'Snake River Plain'),
                    'WESTERN COLD DESERTS', 
                    Ecoregion))

ecoregion_labels <- 
  data.frame( 
    ecoregion_def %>% 
      distinct(Ecoregion) %>% arrange( Ecoregion)) 

# "Ecogroup" is new field for lumping some ecoregions together and 
# splitting some ecoregions I into sub regions for analysis 
ecoregion_labels$ecogroup <- 
  c('AZ/NM Highlands', 
    'E Cold Deserts', 
    'Great Plains',
    'Coastal Forests',
    'Mediterranean California',
    'NW Forested Mts', 
    'Warm Deserts',
    'W Cold Deserts')

ecoregion_def <- 
  ecoregion_def %>%
  left_join(ecoregion_labels , by = 'Ecoregion') 

ER <- ER %>% left_join(ecoregion_def ) # Add new labels 

# Get allotment ecoregions 
allotment_centers <- 
  allotment_centers %>% 
  st_join(ER, left = T, join = st_within )

allotment_centers %>%
  rowwise() %>% 
  mutate( ADMIN_ST = ifelse( ADMIN_ST == ' ', state.abb[ state.name == STATE_NAME], ADMIN_ST)) %>%
  ungroup() %>% 
  select( -starts_with('US'), -contains('KEY'), -contains('CODE')) %>% 
  write_rds(file = 'data/allotment_centers_with_ecoregion.rds')
