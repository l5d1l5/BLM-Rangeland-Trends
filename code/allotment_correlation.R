rm(list = ls())

library(tidyverse)
library(sf)

cover <- read_csv( 'data/MT_mean_allotment_cover.csv') %>% 
  filter( year >= 1990 ) %>% 
  select( AFGC:year) %>% 
  pivot_longer( cols = AFGC:TREE, names_to = 'type', values_to = 'cover')

MT_allotments <- readRDS('output/BLM_cleaned_shape_sf.RDS') %>% 
  st_transform(4326) %>%
  filter( ADMIN_ST == 'MT')


index <- cover$uname %in% ( MT_allotments %>% 
                     filter( ADM_OFC_CD == 'C01000') %>% 
                     pull(uname) ) %>% which

cover_wide <- cover[ index, ] %>% 
  filter(type == 'AFGC') %>% 
  spread( uname, cover)

cover_matrix <- cover_wide %>%select( `6595`:`6972`) %>% as.matrix



