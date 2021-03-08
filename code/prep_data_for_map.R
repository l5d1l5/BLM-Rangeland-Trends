rm(list = ls())

library(tidyverse)
library(sf)

cover_trends <- 
  read_csv('data/MT_cover_trends.csv') %>% 
  select(ADM_OFC_CD, uname, AFGC, BG:TREE) %>% 
  pivot_longer( cols = AFGC:TREE, names_to = 'type', values_to = 'trend') %>% 
  group_by( ADM_OFC_CD, type ) %>% 
  mutate( district_average = mean(trend) , 
          scaled_trend = as.numeric( scale(trend) )) %>% 
  pivot_longer

cover <- read_csv( 'data/MT_mean_allotment_cover.csv') %>% 
  filter( year >= 2000 ) %>% 
  select( AFGC:year) %>% 
  pivot_longer( cols = AFGC:TREE, names_to = 'type', values_to = 'cover')

MT_allotments <- 
  readRDS('output/BLM_cleaned_shape_sf.RDS') %>% 
  st_transform(4326) %>%
  filter( ADMIN_ST == 'MT') %>% 
  left_join( cover_trends, by = c('ADM_OFC_CD', 'uname'))

save( list = c('cover', 
               'MT_allotments'), 
      file = 'app/data/mapdata.rda')
