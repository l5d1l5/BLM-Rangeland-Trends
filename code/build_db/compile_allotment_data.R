rm(list = ls())
library(sf)
library(tidyverse)
library(ggridges)
library(lubridate)

allotment_id <- read_csv('output/allotment_table.csv') %>% 
  select( uname, ADMIN_ST, ADM_OFC_CD, ALLOT_NO, ALLOT_NAME ) %>%
  mutate( `Bill Allot Number` = paste0( ADMIN_ST, ALLOT_NO))

# allotment_sf <- readRDS(file = 'output/BLM_cleaned_shape_sf.RDS') %>%
#   left_join(allotment_id , by = c('uname', 'ADMIN_ST', 'ADM_OFC_CD', 'ALLOT_NAME'))

elevation <- read_csv('data/RAP_EE_exports/allotment_elevation_by_feature.csv') %>% 
  select( mean, uname ) %>% 
  rename( elevation = mean)

climate <- read_csv('data/RAP_EE_exports/allotment_climate_by_feature.csv') %>% 
  select( pdsi, pr, tavg, uname, year )

production <- read_csv('data/RAP_EE_exports/allotment_biomass_by_feature.csv') %>% 
  select( uname, year, contains('agb'))

cover <- read_csv( 'data/RAP_EE_exports/allotment_cover_by_feature.csv') %>% 
  select( uname, year, AFGC, BG, LTR, PFGC, SHR, TREE)

burns <- read_csv('data/RAP_EE_exports/allotment_burns_by_feature.csv') %>% 
  select(uname, tidyr::matches(match = '^\\d+')) %>% 
  pivot_longer(cols = -uname, names_to = 'year', values_to = 'burned') %>%
  mutate( year = as.numeric( str_extract( year, '\\d{4}$'))) 

grazing <- read_rds('data/aum_all.rds') %>% 
  left_join( 
    allotment_id %>% 
    select( `Bill Allot Number`, uname, ADMIN_ST, ADM_OFC_CD, ALLOT_NAME, ALLOT_NO), 
  by = "Bill Allot Number") %>% 
  mutate( year = year( mdy(`Bill Begin Date`)))

grazing %>%
  select( uname, year , starts_with('Bill'), ADM_OFC_CD, ALLOT_NAME, ALLOT_NO) %>%
  arrange( uname, year ) %>% 
  write_rds('data/allotment_grazing_data.rds')

# Clean up allotment administrative information 

# allotment_sf %>% 
#   select( uname:acres, NA_L3NAME, NA_L2NAME, NA_L1NAME, STATE_NAME) %>% 
#   mutate( centroids = st_transform( st_centroid(SHAPE), crs = 'epsg:4326')) %>%
#   mutate( lon = st_coordinates(centroids)[,1], lat = st_coordinates(centroids)[,2]) %>% 
#   select( - centroids) %>%
#   st_drop_geometry() %>% 
#   left_join(elevation) %>% 
#   write_rds(file = 'data/basic_allotment_info.rds')
elevation %>% 
  select( uname, elevation) %>%
  mutate( elevation = as.numeric(elevation)) %>% 
  write_rds(file = 'data/elevation.rds')

# Join the response data together into one long data frame 
climate %>%
  left_join( production) %>% 
  left_join(cover ) %>%
  left_join( burns ) %>% 
  select(uname, year, pdsi, pr, tavg, afgAGB:pfgAGB, AFGC:TREE, burned) %>% 
  pivot_longer(cols = c(pdsi:TREE, burned), names_to = 'type', values_to = 'value')  %>%
  filter( complete.cases(.)) %>%
  write_rds( file = 'data/allotment_data_long.rds')

# 
