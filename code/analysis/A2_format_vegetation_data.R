rm(list = ls())
library(tidyverse)
unloadNamespace('raster')
unloadNamespace('papeR')

source('code/analysis/functions.R')
source('code/analysis/parameters.R')
allotments <- read_csv('data/temp/allotment_info.csv')

allotments <- read_csv('data/temp/allotment_info.csv') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, hectares, area, climate_region, 
        Other, Private, BLM, elevation ) %>%
  rename( elevation = elevation ) %>%
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
woody_cover <- annual_data %>% 
  filter( type %in% c('TREE', 'SHR')) %>% 
  group_by( uname, year , unit ) %>% 
  summarise( value = sum(value )) %>% 
  mutate( type = 'WOODY')

annual_data <- annual_data %>%
  bind_rows(woody_cover)

annual_data %>% 
  group_by( type, uname ) %>%
  filter(value > 0 ) %>%
  summarise( nyears = n_distinct(year )) %>% 
  filter( nyears < 30 ) %>% 
  group_by( type, nyears) %>% 
  summarise( n())  %>% View 

annual_data %>% 
  group_by( type, uname ) %>%
  filter(value > 0 , !is.na(value)) %>%
  mutate( nyears = n_distinct( year )) %>% 
  filter( nyears > 29 ) %>%
  group_by(type, uname) %>%
  mutate( above_thresh = min(value) > 0.25 ) %>% 
  group_by( type, above_thresh) %>% 
  summarise( n_distinct(uname) ) 

cover <- 
  annual_data %>%  
  filter( unit == 'cover') %>% 
  filter( value > 0 ) %>% 
  filter( !is.na(value)) %>% 
  group_by( type, uname) %>% 
  filter( n() > 29 ) %>% 
  filter( min(value) > 0.25 ) %>%
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
  filter( n() > 29 ) %>% 
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

save(allotments, 
     file = 'data/analysis_data/allotments.rda')

rm(agb, allotments)
