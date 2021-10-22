# load files into Postgres database 
rm(list = ls())
library(tidyverse)
library(sf)

#  Load annual data exported from Earth Engine 
cover <- read_csv( 'data/RAP_EE_exports/allotment_cover_by_year.csv') %>% 
  select( uname, year, AFGC, BG, LTR, PFGC, SHR, TREE) %>% 
  mutate( unit = 'cover')

AGB <- read_csv('data/RAP_EE_exports/allotment_production_by_year.csv') %>% 
  select( uname, year, contains('agb')) %>% 
  mutate( unit = 'production')

elevation <- read_csv('data/RAP_EE_exports/allotment_elevation.csv')  %>% 
  select( uname, mean )

# Join the response data together into one long data frame 
annual_data <- AGB %>% 
  pivot_longer( contains('AGB'), values_to = 'value' ) %>% 
  bind_rows(
    cover %>% 
      pivot_longer( AFGC:TREE, values_to = 'value')
  ) 

write_rds(annual_data, 'data/temp/annual_data.rds')

# Write Elevation 
elevation <- elevation %>% rename( 'elevation' = mean )

read_csv('data/temp/allotment_info.csv') %>% 
  left_join( elevation, by = 'uname') %>% 
  write_csv( 'data/temp/allotment_info.csv')

# CLIMATE ------------------------------------- # 
climate <- read_csv('data/RAP_EE_exports/allotment_climate_by_year.csv') %>%
  select( uname, year, pdsi:tavg )

climate %>% 
  write_rds('data/temp/annual_climate.rds')
