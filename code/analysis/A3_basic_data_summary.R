rm(list = ls())

library(tidyverse)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

load('data/analysis_data/allotments.rda')
#load('data/analysis_data/cover.rda')

allotments %>% 
  group_by( ecogroup ) %>%
  summarise( 
    Districts = n_distinct(district_label), 
    `Field Offices` = n_distinct(office_label), 
    Allotments = n_distinct(uname), 
    `Total Area (10^6 ha)` = sum(hectares)/10^6, 
    `Average Area (ha)` = mean(hectares), 
    `% BLM` = 100*sum(BLM)/sum(hectares), 
    `% Private` = 100*sum(Private)/sum(hectares), 
    `% Other` = 100*sum(Other)/sum(hectares), 
    `Average Elevation (m)` = mean(elevation)) %>% 
  ungroup( ) %>% 
  rename( Ecoregion = ecogroup )  %>% 
  bind_rows(
    allotments %>% 
  summarise( 
    Districts = n_distinct(district_label), 
    `Field Offices` = n_distinct(office_label), 
    Allotments = n_distinct(uname), 
    `Total Area (10^6 ha)` = sum(hectares)/10^6, 
    `Average Area (ha)` = mean(hectares), 
    `% BLM` = 100*sum(BLM)/sum(hectares), 
    `% Private` = 100*sum(Private)/sum(hectares), 
    `% Other` = 100*sum(Other)/sum(hectares), 
    `Average Elevation (m)` = mean(elevation)) %>% 
    mutate( Ecoregion = 'Total')) %>% 
  kableExtra::kable(digits = c(0,0, 0, 0,2,0,0,0,1,0)) %>%
  kableExtra::save_kable(file = 'output/tables/Allotment_stats.html')
