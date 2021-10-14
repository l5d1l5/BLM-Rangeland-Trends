# Find ecoregion for each allotment using spatial join  
# Find BLM Field Office IDs and District IDs using spatial join
rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)
source('code/analysis/functions.R')
source('code/analysis/parameters.R')

# input -------------------------- # 
allotment_centers <- readRDS(file = 'data/temp/BLM_allotments_sf.rds') %>%
  ungroup() %>%
  st_centroid() %>%
  select( uname ) 

state_bounds <- tigris::states(resolution = '20m')
ER <- read_sf('data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')
#--------------------------------- # 

# add ecogroup column to allotment table: 
# Get ecogroup 

ER <- ER %>% 
  st_transform(crs = st_crs(allotment_centers))

# 
allotment_centers_with_ecogroup <- 
  allotment_centers %>% 
  st_join(ER, left = T, join = st_within )

# Allotment distribution with ecogroup I
allotment_centers_with_ecogroup %>% 
  group_by( NA_L1NAME ) %>% 
  summarise( n_allotments = n_distinct(uname))  %>% 
  st_drop_geometry( )  %>% 
  write_csv('output/tables/ecoregion_I_allotment_dist.csv')

allotment_centers_with_ecogroup %>% 
  group_by( NA_L2NAME ) %>% 
  summarise(  n_allotments = n_distinct(uname)) %>% 
  st_drop_geometry( )  %>% 
  write_csv('output/tables/ecoregion_II_allotment_dist.csv')

# Custom Ecoregion classification, keep n similar between ecoregions 
# Use Level I Ecoregions in all cases except: 
# 1. Break deserts to level II ecoregions: warm deserts and cold deserts
# 2. Lump Southern Semi-Arid Highlands with Temperate Sierras = "S. Mts."
# 3. Split Cold Deserts (ERII) into West and East  
# 4. Split Great Plains (I) into Northern and Southern 

allotment_centers_with_ecogroup <- 
  allotment_centers_with_ecogroup %>%
  st_drop_geometry() %>% 
  mutate( ecogroup = NA_L1NAME ) %>% 
  mutate( ecogroup = 
            ifelse( ecogroup %in% 
                      c('TEMPERATE SIERRAS', 
                        'SOUTHERN SEMI-ARID HIGHLANDS'), 
                    'AZ/NM HIGHLANDS', 
                    ecogroup)) %>%
  mutate( ecogroup =  
            ifelse( ecogroup == 'NORTH AMERICAN DESERTS', 
                    NA_L2NAME, 
                    ecogroup))  %>%
  mutate( ecogroup = 
            ifelse( ecogroup == "COLD DESERTS",
                    NA_L3NAME,
                    ecogroup)) %>% 
  mutate( ecogroup = 
            ifelse( ecogroup %in% 
                      c('Wyoming Basin', 
                        'Colorado Plateaus', 
                        'Arizona/New Mexico Plateau'),
                    'E COLD DESERTS', 
                    ecogroup)) %>% 
  mutate( ecogroup = 
            ifelse( ecogroup %in% 
                      c( 'Central Basin and Range', 
                         'Northern Basin and Range', 
                         'Columbia Plateau', 
                         'Snake River Plain'),
                    'W COLD DESERTS', 
                    ecogroup)) %>% 
  mutate( ecogroup = 
            ifelse( ecogroup == 'GREAT PLAINS', 
                    NA_L2NAME, 
                    ecogroup)) %>% 
  mutate( ecogroup = 
            ifelse( ecogroup %in% 
                      c('WEST-CENTRAL SEMI-ARID PRAIRIES', 'TEMPERATE PRAIRIES'), 
                    'N GREAT PLAINS', 
                    ecogroup)) %>% 
  mutate( ecogroup = 
            ifelse( ecogroup == 'SOUTH CENTRAL SEMI-ARID PRAIRIES', 
                    'S GREAT PLAINS', 
                    ecogroup))  %>% 
  mutate( ecogroup = 
            ifelse( ecogroup == 'NORTHWESTERN FORESTED MOUNTAINS', 'Forested Mts', ecogroup))


allotment_ecogroups <- 
  allotment_centers_with_ecogroup %>%
  mutate( ecogroup = stringr::str_to_title(ecogroup)) %>%
  mutate( ecogroup = ifelse( ecogroup == 'Az/Nm Highlands' , 'AZ/NM Highlands',  ecogroup )) %>%
  select( uname, ecogroup, US_L3CODE:L1_KEY) 

allotment_ecogroups %>% 
  group_by( ecogroup ) %>% 
  summarise(num_allotments = n_distinct( uname )) %>% 
  write_csv('output/tables/ecogroup_allotment_dist.csv')

# now add ecogroup information to the allotments table 
read_rds('data/temp/allotment_info.rds') %>%
  select( - starts_with('ecogroup')) %>% # remove old ecogroups 
  left_join(allotment_ecogroups %>% 
              select( uname, ecogroup ), by  = 'uname') %>% 
  write_rds('data/temp/allotment_info.rds') # write back out


# Save ecogroup shapefile 
# Save Ecogroup shapes 

EG <- ER %>% 
  left_join( allotment_centers_with_ecogroup , by = 'US_L3CODE') %>%
  filter( !is.na(ecogroup )) %>% 
  select( ecogroup, geometry) %>%
  st_simplify(preserveTopology = T, dTolerance = 500) %>% 
  st_make_valid() %>% 
  group_by( ecogroup) %>%
  summarise( geometry = st_union( geometry )) %>%
  st_make_valid()


EG %>%
  write_rds(file = 'data/temp/simplified_ecogroup_shapefile.rds')


# Save States shapefile 
sel_states <- state.name[ state.abb %in% c('OR', 'WA', 'MT', 
                                           'ID', 'ND', 'SD', 
                                           'UT', 'CO', 'AZ', 
                                           'NM', 'CA', 'NV', 'WY', 'NE') ] 
state_bounds <- 
  state_bounds %>% 
  filter( NAME %in% sel_states ) %>%
  st_transform(crs = st_crs(allotment_centers)) 

state_bounds %>%
  st_transform(crs = st_crs(ER)) %>% 
  filter( STUSPS %in% WESTERN_STATES) %>% 
  write_rds(file = 'data/temp/western_states_outlines_shapefile.rds')



