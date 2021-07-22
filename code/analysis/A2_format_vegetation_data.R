rm(list = ls())
library(tidyverse)
library(dbplyr)
#require( RPostgres  )
#library(sf)
#library(lme4)
#library(emmeans)
#library(optimx)
#library(dfoptim)
#library(parallel)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)


allotments <- tbl(con, 'allotments') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, acres, area, climate_region) %>% 
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) 

annual_data <- 
  tbl(con, 'annual_data') %>% 
  filter( year  > 1990 ) %>%
  filter( value > 0 ) %>% 
  left_join(allotments, by = 'uname') %>% 
  filter( ecogroup != "Coastal Forests")

# 

# Cover: 
cover <- 
  annual_data %>%  
  filter( type %in% c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR')) %>% 
  pivot_wider(names_from = type, values_from = value ) %>%
  mutate( HERB = AFGC + PFGC ) %>%
  pivot_longer( c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR', 'HERB'), 
                names_to = 'type', values_to = 'value' )  %>% 
  group_by( type, uname) %>% 
  filter( min(value, na.rm = T) > 0.5 ) %>%
  select( type, uname, year,value, admin_st, district_label, office_label, ecogroup, area, climate_region) %>% 
  collect() %>% 
  as.data.frame() %>% 
  group_by( type, uname) %>% 
  filter( !any(is.na(value))) %>% 
  ungroup() %>% 
  split(f = .$type ) 

cover <- cover %>% lapply( 
  function(x) { 
    x %>% mutate( value2 = scale(log(value))) %>%
      mutate( year2 = scale(year), 
              area2 = scale(area, center = F)) }) 

save(cover, file = 'data/temp/cover.rda')

rm(cover) 

agb <- annual_data %>% 
  filter( type %in% c('pfgAGB', 'afgAGB')) %>% 
  pivot_wider(names_from = type, values_from = value ) %>%
  mutate( herb_agb = pfgAGB + afgAGB ) %>%
  pivot_longer(c('pfgAGB', 'afgAGB', 'herb_agb'), 
               names_to = 'type', values_to = 'value' ) %>% 
  group_by( type, uname) %>% 
  filter( min(value, na.rm = T) > 0.5 ) %>%
  select( type, uname, year,value, admin_st, district_label, office_label, ecogroup, climate_region, area) %>% 
  collect() %>% 
  as.data.frame() %>% 
  group_by( type, uname) %>% 
  filter( !any(is.na(value))) %>% 
  ungroup() %>% 
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

save(agb, file = 'data/temp/agb.rda')
rm(agb)