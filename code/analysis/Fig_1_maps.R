# load files into Postgres database 
rm(list = ls() )
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )
require( rpostgis )
library(sf)

source('code/analysis/plot_tools.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

allotment_centers <- read_sf(con, 'allotment_centroids') %>%
  st_transform('epsg:4326')

allotments <- tbl(con, 'allotments') %>% 
  filter( ecogroup != 'Coastal Forests') %>%
  collect()

allotment_centers <- 
  allotment_centers %>% 
  left_join(allotments, by = 'uname') %>% 
  mutate( X = st_coordinates(shape)[,1], Y = st_coordinates(shape)[,2]) 

field_office_stats <- 
  allotment_centers %>% 
  group_by( admu_name ) %>%
  st_drop_geometry( ) %>%
  summarise(x = mean(X), y = mean(Y), n = n() ) %>%
  st_as_sf(coords = c('x', 'y'), crs = 'epsg:4326') %>% 
  st_transform(crs = 'epsg:5070')

field_office_stats <- 
  field_office_stats %>% 
  mutate( `Field Office` = str_remove( admu_name, ' Field Office$'))

district_stats <- 
  allotment_centers %>% 
  group_by( parent_name ) %>%
  st_drop_geometry( ) %>%
  summarise(x = mean(X), y = mean(Y), n = n() ) %>%
  st_as_sf(coords = c('x', 'y'), crs = 'epsg:4326') %>% 
  st_transform(crs = 'epsg:5070')

BLM_districts <- read_rds('data/temp/cleaned_BLM_district_shapes.rds')
BLM_offices <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds')
ecogroups <- read_rds('data/temp/simplified_ecogroup_shapefile.rds')
ecogroups <- ecogroups %>% filter( ecogroup != 'Coastal Forests') 
state_layer <- read_rds('data/temp/western_states_outlines_shapefile.rds')  

BLM_districts <- BLM_districts %>% 
  filter( !PARENT_NAME  %in% c('NORTHEASTERN STATES DISTRICT OFFICE', 
                               'SOUTHEASTERN STATES DISTRICT OFFICE' )) 

gg_ecogroup  <- 
  ecogroups %>% 
  ggplot() + 
  geom_sf(aes( fill = ecogroup), color = NA, size = 0.2)

gg_ecogroup + 
  geom_sf( data = BLM_districts, aes( fill = NA) , alpha = 0.5, size = 0.1) +  
  scale_fill_manual(values = ecogroup_colors, name = 'Ecoregion') + 
  theme( legend.position = c(0.85, 0.2)) + 
  geom_sf(data = state_layer %>%
                       ungroup() %>%
                       st_union(), aes( fill = NA))  
  # ggsave( filename = 'output/figures/fig_1_region_map.png', 
  #         height = 7, width = 8, dpi = 'print')

#allotment_shapes <- st_read(con, 'allotment_shapes')
# read allotment info with ER info
# join with allotment shapes cleaned 
# Union allotments within same ecogroups 
# map unioned allotment regions 
# fill allotments by ecoregion type

  geom_sf( data = adm_districts, aes( fill = NA) , alpha = 0.5, size = 0.1) +  
  scale_fill_manual(values = ecogroup_colors, name = 'Ecoregion') + 
  theme( legend.position = c(0.85, 0.2)) + 
  geom_sf(data = state_layer %>%
            ungroup() %>%
            st_union(), aes( fill = NA)) +
  geom_sf(data = allotments_by_ecoregion, aes( fill = ecogroup), size = 0.2) + 
  scale_color_manual(values = ecogroup_colors, name = 'Ecoregion') + 
  ggsave( filename = 'output/figures/fig_1_region_map_with_allotments.png',
          width = 8, height = 7, units = 'in', dpi = 'print')
  
