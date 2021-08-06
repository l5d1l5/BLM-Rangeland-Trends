# Get data for app 
rm(list= ls())
library(tidyverse)
library(sf)

veg <- read_csv(file = 'output/export_data/annual_veg_data.csv')
load('app/data/mapdata.rda')

allotments <- map_data %>% sf::st_drop_geometry() 

last_year <- max(veg$year)
first_year <- min(veg$year)

veg <- veg %>% 
  mutate( unit = NA) %>% 
  mutate( unit = ifelse(str_detect(type, 'AGB$'), 'production', unit )) %>% 
  mutate( unit = ifelse( is.na(unit), 'cover', unit )) %>% 
  mutate( type_label = factor( type )) %>% 
  mutate( type_label = factor( type_label, 
          labels = c('Annual', 'Annual', 'Bare Ground', 'Herbaceous', 
                     'Litter', 'Perennial', 'Perennial', 'Shrub', 'Tree'))) 

ecoregion_veg <- veg %>% 
  left_join(allotments, by = 'uname') %>% 
  group_by( Ecoregion, year, type_label, unit ) %>% 
  summarise( median = median( value), 
             uq = quantile(value, 0.75), 
             lq= quantile(value, 0.25))

district_veg <- veg %>% 
  left_join(allotments, by = 'uname' ) %>% 
  group_by( District, year, type_label, unit) %>% 
  summarise( median = median( value), 
             uq = quantile(value, 0.75), 
             lq= quantile(value, 0.25))

field_office_veg <- veg %>% 
  left_join(allotments, by = 'uname' ) %>% 
  group_by( `Field Office`, year, type_label, unit) %>% 
  summarise( median = median( value), 
             uq = quantile(value, 0.75), 
             lq= quantile(value, 0.25))


save(veg, 
     ecoregion_veg, 
     district_veg, 
     field_office_veg, 
     last_year, 
     first_year, 
     file = 'app/data/vegdata.rda')
