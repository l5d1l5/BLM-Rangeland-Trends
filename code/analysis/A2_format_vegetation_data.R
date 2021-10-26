rm(list = ls())
library(tidyverse)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')
allotments <- read_csv('data/temp/allotment_info.csv')

allotments <- read_csv('data/temp/allotment_info.csv') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, hectares, area, climate_region, 
        Other, Private, BLM, elevation.x ) %>%
  rename( elevation = elevation.x ) %>%
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) %>% 
  filter( ecogroup != 'Marine West Coast Forest')

# ---------------------------- # 
annual_data <- 
  read_rds('data/temp/annual_data.rds') %>% 
  rename( type = name )  %>%
  filter( year  > 1990 ) 

year <- annual_data %>% distinct(year ) %>% pull( year )
type <- annual_data %>% distinct( type ) %>% pull(type)
uname <- allotments %>% distinct( uname )  %>% pull( uname)

units <- 
  annual_data %>%
  distinct( type, unit )

annual_data <-
  expand.grid( uname = uname, year =  year, type = type) %>%
  left_join( annual_data , by = c('uname', 'year', 'type'))  %>% 
  select( - unit ) %>% 
  left_join( units ) 

# ------------------------ # 
# Cover: 

cover <- 
  annual_data %>%  
  filter( unit == 'cover') %>% 
  filter( value > 0 ) %>% 
  filter( !is.na(value)) %>% 
  group_by( type, uname) %>% 
  filter( n() > 25 ) %>% 
  filter( min(value, na.rm = T) > 0.25 ) %>%
  ungroup() %>% 
  left_join( allotments) %>% 
  filter( !is.na(ecogroup ) ) %>% 
  split(f = .$type ) 

cover <- cover %>% lapply( 
  function(x) { 
    x %>% mutate( value2 = scale(log(value))) %>%
      mutate( year2 = scale(year), 
              area2 = scale(area, center = F)) }) 

save(cover, file = 'data/analysis_data/cover.rda')

rm(cover) 

agb <- annual_data %>% 
  filter( unit == 'production') %>% 
  filter( value > 0 ) %>% 
  filter( !is.na(value)) %>% 
  group_by( type, uname) %>% 
  filter( n() > 25 ) %>% 
  filter( min(value, na.rm = T) > 0.25 ) %>%
  ungroup() %>% 
  left_join( allotments) %>% 
  filter( !is.na(ecogroup)) %>%
  split(f = .$type ) 

agb <- 
  agb %>% 
  lapply( function(x){ 
    x %>% 
      mutate( value2 = scale(log(value)), 
              value1 = log(value)) %>%
      mutate( year2 = scale(year), 
              area2 = scale(area, center = F ))
  }) 

save(agb, file = 'data/analysis_data/agb.rda')
rm(agb)

save(allotments, 
     file = 'data/analysis_data/allotments.rda')
