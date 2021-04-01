rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)

m2_per_ACRE <- 4046.8564224

BLM <- sf::read_sf('data/BLM_National_Grazing_Allotments/gra.gdb/', 
                   layer = 'gra_allot_poly')

BLM <- 
  BLM %>%
  mutate( ALLOT_NAME = str_to_upper(str_trim(ALLOT_NAME))) %>% 
  mutate( ALLOT_NAME = str_remove( ALLOT_NAME, '\\.')) %>%
  arrange( ADMIN_ST, ADM_OFC_CD, ALLOT_NAME, ALLOT_NO) %>% 
  mutate( uid = row_number() ) %>% 
  mutate( uname = as.numeric(as.factor(paste0( ADMIN_ST, ADM_OFC_CD, ALLOT_NAME, ALLOT_NO)))) %>% 
  mutate( ushape = as.numeric(as.factor(as.character( SHAPE ))))

BLM <- 
  BLM %>% 
  mutate( LAST_DATE = date(last_edited_date )) %>%
  select(uid, uname, ushape, ADMIN_ST, ADM_OFC_CD, ALLOT_NO, ALLOT_NAME, LAST_DATE ) 

allotment_table <- BLM %>% st_drop_geometry()

# View duplicate shapes 
allotment_table %>% 
  group_by( ushape) %>% 
  mutate( n_dup = n()) %>% 
  filter( n_dup > 1 )

# View duplicate names 
# some are multipolygons with non-overlapping shapes 
allotment_table %>% 
  group_by( uname ) %>% 
  mutate( n_dup = n() ) %>% 
  filter( n_dup > 1 )

# Keep only first record of duplicate polygons 
BLM <- 
  BLM %>% 
  group_by( ushape) %>% 
  arrange( ushape )  %>% 
  filter( row_number() == 1 )

# drop really small polygons
BLM <-
  BLM %>%
  ungroup() %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate( area = st_area(SHAPE) %>% as.numeric ) %>%
  mutate( acres = area /m2_per_ACRE ) %>%
  filter( acres > 1 )

# Histogram 
BLM %>% 
  ggplot( aes( x = acres )) + 
  geom_histogram() + 
  scale_y_continuous(name = 'Number of Allotments') + 
  scale_x_log10(name = 'Allotment Size (acres)', 
                breaks = c(10, 40, 100, 1000, 10000, 100000), label = scales::comma) + 
  theme_bw() + 
  theme( panel.grid = element_blank()) + 
  ggsave('output/allotment_histogram.png')

# Example of redundant allotment 
BLM %>% 
  filter( ALLOT_NO == '10291') %>% 
  ggplot(aes( fill = paste0( ALLOT_NAME, ': ', uid))) + 
  geom_sf(alpha = 0.2) + 
  ggsave(filename = 'output/BakingPowder_10291_duplicate_example.png')

BLM %>% 
  saveRDS(file = 'data/BLM_sf.rds')

allotment_table %>% 
  write_csv('output/allotment_table.csv')
