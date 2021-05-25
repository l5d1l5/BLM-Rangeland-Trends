rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)

m2_per_ACRE <- 4046.8564224

BLM <- sf::read_sf('data/BLM_National_Grazing_Allotments_NEW/gra.gdb/', 
                   layer = 'gra_allot_poly')

BLM <- 
  BLM %>%
  mutate( ALLOT_NAME = str_to_upper(str_squish(str_trim(ALLOT_NAME)))) %>% 
  mutate( ALLOT_NAME = str_remove( ALLOT_NAME, '\\.')) %>%
  mutate( ALLOT_NAME = str_replace( ALLOT_NAME, 'CR$', 'CREEK')) %>%  # for flying my buffalo creek 
  arrange( ADMIN_ST, ADM_OFC_CD, ALLOT_NAME, ALLOT_NO) %>% 
  mutate( uid = row_number() )  %>% 
  mutate( ushape = as.numeric(as.factor(as.character( SHAPE )))) %>% 
  mutate( LAST_DATE = date(last_edited_date )) %>%
  select(uid, ushape, ADMIN_ST, ADM_OFC_CD, ALLOT_NO, ALLOT_NAME, LAST_DATE ) 

temp_allotment_table <- BLM %>% st_drop_geometry()

# View duplicate shapes 
temp_allotment_table %>% 
  group_by( ushape) %>% 
  mutate( n_dup = n()) %>% 
  filter( n_dup > 1 )

# View duplicate names 
# Some are multipolygons with non-overlapping shapes 
# Some are different allotments but with same name
# And some are duplicates of the same allotment with 
# different boundaries
temp_allotment_table %>% 
  group_by( ALLOT_NAME ) %>% 
  mutate( n_dup = n() ) %>% 
  filter( n_dup > 1 ) %>% View

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
  filter( ALLOT_NAME == 'WHISKEY RIDGE') %>%
  mutate( uid = factor(paste( uid, LAST_DATE, sep = '--'))) %>%
  ggplot() + 
  geom_sf(aes( fill = uid , color = uid), alpha = 0.2) + 
  ggtitle('Whiskey Ridge Allotment, MT')+ 
  theme_bw() + 
  ggsave('output/figures/Whiskey_Ridge_Duplicate_example.png', 
         height = 6, width = 8, units = 'in')


# Read in BLM districts and populate missing allotment ADMIN_ST field 
# with ADMIN_ST from BLM district shapefile 
# Spatial Join allotment centers within admin_dist
st_layers('data/BLM_National_Administrative_Units/admu.gdb/' )

districts <- st_read( 'data/BLM_National_Administrative_Units/admu.gdb/', 
         layer = 'blm_natl_admu_dist_poly_webpub')

districts <- districts %>% st_transform(crs = 'epsg:5070') 
districts <- districts %>% select( ADMIN_ST, Shape)

allotment_centers <- 
  BLM %>% 
  st_transform(crs = 'epsg:5070') %>% 
  group_by( uid ) %>% 
  summarise( SHAPE = st_centroid(SHAPE) )

allotment_centers <- 
  allotment_centers %>% 
  st_join( districts, join = st_within , left = T) 

uid_with_states <- 
  allotment_centers %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  rename("CTR_STATE" = ADMIN_ST)

BLM <- 
  BLM %>% 
  mutate( ADMIN_ST = str_trim(str_squish( ADMIN_ST))) %>% 
  mutate( ADMIN_ST = ifelse( ADMIN_ST == '' , NA, ADMIN_ST)) %>% 
  left_join(uid_with_states, by = 'uid') %>%
  mutate( ADMIN_ST = ifelse( is.na( ADMIN_ST), CTR_STATE, ADMIN_ST)) 

BLM <- 
  BLM %>% 
  st_transform('epsg:5070') %>%
  ungroup() %>% 
  mutate( uname = as.numeric(as.factor(paste0( ADMIN_ST, ADM_OFC_CD, ALLOT_NAME, ALLOT_NO)))) %>% 
  group_by(uname, ADMIN_ST, ADM_OFC_CD, ALLOT_NO, ALLOT_NAME) %>% 
  summarise( SHAPE = st_union(SHAPE), LAST_DATE = max(LAST_DATE), FIRST_DATE = min(LAST_DATE) )  %>% 
  ungroup() %>%
  st_make_valid() %>%
  mutate( area = st_area( SHAPE)) %>% 
  mutate( acres = area/m2_per_ACRE )

# Check for duplicates 
stopifnot(
  BLM %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  group_by( uname ) %>% 
  filter( n() > 1 ) %>%nrow() == 0 )

# create if don't exit 

dir.create('data/temp')
dir.create('data/temp/BLM_allotments_cleaned')

BLM %>% st_write(dsn = 'data/temp/BLM_allotments_cleaned/allotments.shp', 
                              layer = 'allotments', append = F)

BLM %>% saveRDS(file = 'data/temp/BLM_allotments_sf.rds')

