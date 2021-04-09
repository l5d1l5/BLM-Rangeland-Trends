rm(list = ls())

library(tidyverse)
library(sf)

adm_districts <- sf::read_sf('data/BLM_National_Administrative_Units/admu.gdb/', 
                           layer = 'blm_natl_admu_dist_poly_webpub') %>% 
  filter( ADMIN_ST != 'AK' ) 

adm_offices <- sf::read_sf('data/BLM_National_Administrative_Units/admu.gdb/', 
                           layer = 'blm_natl_admu_field_poly_webpub') %>%
  filter( ADMIN_ST != 'AK')

allotment_centers <- read_rds('data/allotment_centers_with_ecoregion.rds')

allotment_centers <- 
  allotment_centers %>%
  distinct(uname)  %>%
  left_join(
  allotment_centers %>% 
  ungroup() %>%
  st_drop_geometry() %>% distinct() )


adm_offices <- adm_offices %>% 
  st_transform(crs = st_crs(allotment_centers))

adm_districts <- adm_districts %>% 
  st_transform(crs = st_crs(allotment_centers))

adm_offices <- 
  adm_offices %>% 
  st_simplify(preserveTopology = T, dTolerance = 100) %>%
  st_make_valid()

adm_districts <- 
  adm_districts %>% 
  st_simplify(preserveTopology = T, dTolerance = 100) %>%
  st_make_valid()

adm_offices <- 
  adm_offices %>% 
  select( ADM_UNIT_CD, ADMU_NAME, BLM_ORG_TYPE, PARENT_CD, PARENT_NAME, ADMIN_ST) %>% 
  filter( BLM_ORG_TYPE == 'Field')

adm_districts <- 
  adm_districts %>%
  select( PARENT_CD, ADMIN_ST, PARENT_NAME, Shape) 

allotment_centers <- 
  allotment_centers %>% 
  st_join(adm_districts, left = T, join = st_within) %>% 
  mutate( ADMIN_ST = ifelse( ADMIN_ST.x != ADMIN_ST.y , ADMIN_ST.y, ADMIN_ST.x ))

allotment_centers <- 
  allotment_centers %>%
  select( uname, 
          ALLOT_NAME, 
          PARENT_CD, 
          PARENT_NAME, 
          ADM_OFC_CD, ADMIN_ST, STATE_NAME, acres:ecogroup)

allotment_centers <- 
  allotment_centers %>% 
  st_join(
    ( adm_offices %>% 
      select( ADM_UNIT_CD, ADMU_NAME ) ), left = T, join = st_within) %>% 
  select( uname, ALLOT_NAME, ADMU_NAME, ADM_UNIT_CD, PARENT_CD:ecogroup) %>% 
  filter( ! (uname==13474 & ADMIN_ST == 'CA') ) 

# Fill in missing state abreviations: 
allotment_centers <- 
  allotment_centers %>% 
  rowwise() %>%
  mutate( ADMIN_ST = ifelse( is.na(ADMIN_ST), state.abb[ STATE_NAME == state.name] , ADMIN_ST ))

stopifnot( !any( allotment_centers %>% duplicated() ) ) # test for duplicated 

names( allotment_centers) <- str_to_lower(names(allotment_centers))
st_geometry(allotment_centers) <- 'shape'

allotment_centers <- 
  allotment_centers %>% 
  mutate( allot_name = str_to_title(allot_name), 
          admu_name = str_to_title(admu_name), 
          parent_name = str_to_title(parent_name))

# Fill in missing information from Las Cruces NM and Paria River UT. 
allotment_centers <- 
  allotment_centers %>%
  mutate( admu_name = ifelse( is.na(admu_name), parent_name, admu_name)) %>%
  mutate( adm_unit_cd = ifelse( is.na(adm_unit_cd), adm_ofc_cd, adm_unit_cd)) 


allotment_centers %>% 
  write_rds( file = 'data/allotment_centers_with_ecogroup_and_BLM_office.rds')

