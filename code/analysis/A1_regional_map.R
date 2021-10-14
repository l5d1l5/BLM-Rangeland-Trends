# load files into Postgres database 
rm(list = ls() )
library(tidyverse)
library(sf)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

allotment_info <- read_rds('data/temp/allotment_info.rds') %>% 
  filter( ecogroup != 'Marine West Coast Forest')

BLM_districts <- read_rds('data/temp/cleaned_BLM_district_shapes.rds')
BLM_offices <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds')
ecogroups <- read_rds('data/temp/simplified_ecogroup_shapefile.rds') %>% 
  filter( ecogroup != 'Marine West Coast Forest') 
state_layer <- read_rds('data/temp/western_states_outlines_shapefile.rds')  
allotment_shapes <- read_rds('data/temp/BLM_allotments_sf.rds')

allotment_shapes <- 
  allotment_shapes %>% 
  select(uname) %>%
  left_join(allotment_info %>% select(uname, ecogroup), by = 'uname') %>% 
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


regional_map <- 
  state_layer %>% 
    ggplot() + 
    geom_sf( fill = 'white', size = 0.8) + 
    geom_sf( data = allotment_shapes_ecogroup, aes( fill = ecogroup), 
             alpha = 0.9, color= NA) + 
    scale_fill_manual( name = 'Ecogroup', values = ecogroup_colors) + 
    # geom_sf( data= allotment_shapes_ecogroup, aes( fill = ecogroup), size = 0.1,
    #         fill = NA, alpha = 0.9) +
    scale_color_manual(name = 'Ecogroup', values = ecogroup_colors) + 
    #geom_sf(data = BLM_districts, fill = NA, color = NA) + 
    geom_sf(data = BLM_offices %>% distinct(Shape), 
            fill = NA, size = 0.1, alpha = 0.5) + 
    geom_sf(fill = NA, size = 0.5, alpha = 0.5) + 
    theme( legend.position = c(0.85, 0.25), 
           legend.box.background = element_rect(color = 1, size = 0.2)) 


regional_map + 
  ggsave( filename = 'output/figures/Fig_1_Ecoregion_Map.png',
          width = 8, height = 7, units = 'in', dpi = 600)
