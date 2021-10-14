# load files into Postgres database 
rm(list = ls() )
library(tidyverse)
library(sf)

climate_regions <- list(
  NW = c('OR', 'WA', 'ID'), 
  NR = c('MT', 'WY'), 
  SW = c('UT', 'CO', 'NM', 'AZ'), 
  W = c('CA', 'NV') )

climate_regions <- stack(climate_regions ) %>% 
  rename( 'climate_region' = ind, 'admin_st' = values  )

allotment_info <- 
  readRDS(file = 'data/temp/allotment_info.rds') %>% 
  mutate( hectares = as.numeric(hectares))

allotment_ownership <- 
  read_csv(file = 'data/RAP_EE_exports/ownership_area_by_allotment.csv')

allotment_ownership <- allotment_ownership %>% 
  pivot_longer(`3`:`2000`, 'owner', values_to = 'hectares' ) %>% 
  mutate( hectares = replace_na(hectares, 0)) %>% 
  mutate( PADUS_class = as.numeric(owner)) %>% 
  mutate( owner = factor(PADUS_class, labels = c('State', 'Private', 'Private Conservation', 
                                   'Forest Service', 'BLM', 'other')))  %>% 
  mutate( simple_class = factor(PADUS_class)) 

levels(allotment_ownership$simple_class) <- 
  list('Other' = c(3, 11, 2000), 
          'Private' = c(6,7), 
          'BLM' = 21) 

allotment_ownership <- 
  allotment_ownership %>% 
  select( uname, simple_class, hectares ) %>% 
  group_by( uname, simple_class ) %>% 
  summarise( hectares = sum(hectares)) %>%
  spread( simple_class, hectares )

allotment_info <- 
  allotment_info %>% 
  select( uname, allot_no, allot_name, 
                           last_date, area, hectares, 
                           adm_unit_cd, admin_st, admu_name,
                           parent_cd, parent_name, lon, lat) %>% 
  left_join( climate_regions) %>% 
  left_join( allotment_ownership)


allotment_info %>% 
  write_rds('data/temp/allotment_info.rds')

