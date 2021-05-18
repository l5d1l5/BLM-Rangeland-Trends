rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)
library(parallel)

m2_per_ACRE <- 4046.8564224

find_intersection_area <- function(x, y, m2_per_ACRE) {
  # Function for finding intersecting area of two shapes
  O <- try(expr = st_intersection(x, y), silent = T)
  if (class(O)[1] == 'try-error') {
    x <- x %>% st_make_valid()
    y <- y %>% st_make_valid()
    O <- st_intersection(x, y)
  }
  A_o <- st_area(O) %>% as.numeric / m2_per_ACRE
  
  return(A_o)
}

# Pull allotment geometries and find intersections
BLM <-
  readRDS('data/BLM_sf.rds') %>%
  filter( ADMIN_ST == 'MT') %>%
  #filter( ADM_OFC_CD == 'C02000') %>% 
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() 

BLM <- 
  BLM  %>%  
  group_by( uname) %>% 
  filter( LAST_DATE == max(LAST_DATE))  %>% 
  ungroup() %>% 
  group_by( uname, LAST_DATE ) %>% 
  mutate( n_dups = n() ) %>% 
  ungroup() 

dir.create('output/BLM_allotments_cleaned')

BLM %>%
  st_write('output/BLM_allotments_cleaned/allotments_cleaned.shp',
           append = F)
