# Merge BLM allotments with Ecoregion and State Data 
rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)

BLM <- readRDS(file = 'output/BLM_cleaned_shape_sf.RDS')

m2_per_ACRE <- 4046.8564224

ER <- sf::read_sf('data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')

ER <- ER %>% st_transform(crs = 5070)

# Add EPA Ecoregion info to the BLM Allotments 
BLM <- 
  BLM %>% 
  st_join(ER, left = T, largest = T)


BLM %>% 
  write_rds(file = 'output/BLM_cleaned_shape_sf.RDS')

unlink('output/BLM_allotments', recursive = T)
dir.create('output/BLM_allotments')

BLM %>%
  st_write('output/BLM_allotments/allotments.shp', append = F)

 