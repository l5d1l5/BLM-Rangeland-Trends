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

allotments <- tbl(con, 'allotments') %>% 
  filter( ecogroup != 'Coastal Forests') %>%
  collect()

BLM_districts <- read_rds('data/temp/cleaned_BLM_district_shapes.rds')
BLM_offices <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds')
ecogroups <- read_rds('data/temp/simplified_ecogroup_shapefile.rds')
ecogroups <- ecogroups %>% filter( ecogroup != 'Coastal Forests') 
state_layer <- read_rds('data/temp/western_states_outlines_shapefile.rds')  
allotment_shapes <- read_rds('data/temp/BLM_allotments_sf.rds')

clim_regions <- data.frame( STUSPS = state_layer$STUSPS, 
            clim_region = c('NW', 'SW', 'W', 'NW','NR', 'NW', 'SW', 'SW', 
                                                  'NR', 'NR', 'W', 'NR', 
                                                  'NR', 'SW'))

clim_region_layer <- state_layer %>% 
  left_join(clim_regions)  %>% 
  group_by( clim_region ) %>% 
  summarise( geometry = st_union(geometry)) %>% 
  st_make_valid() %>% 
  st_transform(crs = st_crs(allotment_shapes)) %>% 
  st_simplify(dTolerance = 100) %>% 
  st_make_valid()


allotment_shapes <- 
  allotment_shapes %>% 
  select(uname) %>%
  left_join(allotments %>% select(uname, ecogroup), by = 'uname') %>% 
  filter( !is.na(ecogroup)) %>% 
  mutate( SHAPE = st_buffer(SHAPE, 200)) %>% 
  st_simplify(dTolerance = 200) %>%
  st_make_valid()



allotment_shapes_ecogroup <- 
  allotment_shapes %>% 
  group_by( ecogroup ) %>% 
  summarise( SHAPE = st_union( SHAPE)) %>% 
  st_make_valid()

# save out for other use 
#write_rds( allotment_shapes_ecogroup, file = 'data/temp/allotments_by_ecogroup_for_mapping.RDS' )
#allotment_shapes_ecogroup <- read_rds(file = 'data/temp/allotments_by_ecogroup_for_mapping.RDS')

# Make Maps 
BLM_districts <- BLM_districts %>% 
  filter( !PARENT_NAME  %in% c('NORTHEASTERN STATES DISTRICT OFFICE', 
                               'SOUTHEASTERN STATES DISTRICT OFFICE' )) %>% 
  st_transform(crs = st_crs(allotment_shapes_ecogroup)) %>% 
  st_simplify(dTolerance = 1000) %>% 
  st_make_valid()

BLM_offices <- BLM_offices %>%
  filter( BLM_ORG_TYPE == 'Field', PARENT_NAME != 'OKLAHOMA FIELD OFFICE') %>% 
  st_transform(crs = st_crs(allotment_shapes_ecogroup)) %>% 
  st_simplify(dTolerance = 100) %>% 
  st_make_valid()

state_layer <- state_layer %>% 
  st_transform(crs = st_crs(allotment_shapes_ecogroup)) %>% 
  st_simplify(dTolerance = 1000) %>%
  st_make_valid()

allotment_shapes_ecogroup <- allotment_shapes_ecogroup %>% 
  mutate( ecogroup = factor( ecogroup, labels = 
                               c('AZ/NM Highlands', 
                                 'E Cold Deserts', 
                                 'Great Plains', 
                                 'Mediterranean California', 
                                 'Forested Mts', 
                                 'W Cold Deserts', 
                                 'Warm Deserts')))


regional_map <-
  clim_region_layer %>% 
  ggplot() + 
  geom_sf( fill = 'white', color = NA) + 
  geom_sf( data = allotment_shapes_ecogroup, aes( fill = ecogroup), 
           alpha = 0.8, color= NA)  + 
  geom_sf( data= allotment_shapes_ecogroup, aes( color = ecogroup), size = 0.1, 
           fill = NA) + 
  scale_fill_manual( name = 'Ecogroup', values = ecogroup_colors) + 
  scale_color_manual(name = 'Ecogroup', values = ecogroup_colors) + 
  #geom_sf(data = BLM_districts, fill = NA, color = NA) + 
  geom_sf(data = BLM_offices %>% distinct(Shape), fill = NA, size = 0.1, alpha = 0.5) + 
  geom_sf(fill = NA, size = 0.5) + 
  theme( legend.position = c(0.85, 0.25), 
         legend.box.background = element_rect(color = 1, size = 0.2)) 



regional_map + 
  ggsave( filename = 'output/figures/Fig_1_Ecoregion_Map.png',
          width = 8, height = 7, units = 'in', dpi = 600)
