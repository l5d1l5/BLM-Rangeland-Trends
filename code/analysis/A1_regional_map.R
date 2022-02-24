rm(list = ls() )
library(tidyverse)
library(sf)
unloadNamespace( 'raster')

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

allotment_info <- read_csv('data/temp/allotment_info.csv') %>% 
  filter( ecogroup != 'Marine West Coast Forest')

BLM_districts <- read_rds('data/temp/cleaned_BLM_district_shapes.rds')
BLM_offices <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds')
ecogroups <- read_rds('data/temp/simplified_ecogroup_shapefile.rds') %>% 
  filter( ecogroup != 'Marine West Coast Forest') 
state_layer <- read_rds('data/temp/western_states_outlines_shapefile.rds')  
allotment_shapes <- sf::read_sf('data/temp/BLM_allotments_cleaned/allotments.shp')


allotment_shapes_ecogroup <- 
  allotment_shapes %>% 
  select(uname) %>%
  left_join(allotment_info %>% select(uname, ecogroup), by = 'uname') %>% 
  filter( !is.na(ecogroup)) %>% 
  mutate( geometry = st_buffer(geometry, 200)) %>% 
  st_simplify(dTolerance = 200) %>%
  st_make_valid() %>% 
  group_by( ecogroup ) %>% 
  summarise( geometry = st_union( geometry)) %>% 
  st_make_valid() 
  

allotment_shapes_ecogroup <- allotment_shapes_ecogroup %>%
  mutate(ecogroup = factor(ecogroup)) %>%
  mutate(ecogroup = factor(
    ecogroup,
    labels = c(
      'AZ/NM Highlands',
      'East Cold Deserts',
      'Forested Mountains',
      'Mediterranean California',
      'North Great Plains',
      'South Great Plains',
      'West Cold Deserts',
      'Warm Deserts'
    )
  ))

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
  st_simplify(dTolerance = 100) %>%
  st_make_valid()

names( ecogroup_colors)[ order(names( ecogroup_colors)) ]  <- 
  c(
    'AZ/NM Highlands',
    'East Cold Deserts',
    'Forested Mountains',
    'Mediterranean California',
    'North Great Plains',
    'South Great Plains',
    'West Cold Deserts',
    'Warm Deserts'
    )


state_layer <- state_layer %>% 
  st_transform(crs = 'epsg:4326')

allotment_shapes_ecogroup <- allotment_shapes_ecogroup %>% 
  st_transform(crs = 'epsg:4326')

BLM_offices <- BLM_offices %>% 
  st_transform(crs = 'epsg:4326')


full_map <- 
  state_layer %>% 
    ggplot() + 
    geom_sf( fill = 'white', size = 0.2) + 
    geom_sf( data = allotment_shapes_ecogroup, aes( fill = ecogroup), 
             alpha = 0.9, color= NA) + 
    scale_fill_manual( name = 'Ecoregion', values = ecogroup_colors) + 
    # geom_sf( data= allotment_shapes_ecogroup, aes( fill = ecogroup), size = 0.1,
    #         fill = NA, alpha = 0.9) +
    scale_color_manual(name = 'Ecoregion', values = ecogroup_colors) + 
    #geom_sf(data = BLM_districts, fill = NA, color = NA) + 
    xlim( c(-126, -95)) + 
    ylim( c(31.5, 48.5)) + 
    geom_sf(data = BLM_offices %>% distinct(Shape), 
            fill = NA, size = 0.1, alpha = 0.5) + 
    geom_sf(fill = NA, size = 0.2, alpha = 0.5) + 
    theme( legend.position = c(0.9, 0.3), 
           legend.box.background = element_rect(color = 1, size = 0.2)) 


full_map + 
  ggsave( filename = 'output/figures/Fig_1_Ecoregion_Map.png',
          width = 10, height = 5.5, units = 'in', dpi = 400)


# Ecoregion Map 

west_cold_deserts_allots <- allotment_shapes %>% 
  filter( ecogroup == 'W Cold Deserts') %>%
  st_transform(crs = "EPSG:4326") 

ecogroup_colors

regional_map <- 
  state_layer %>% 
  ggplot() + 
  geom_sf( fill = 'white', size = 0.2, color = 'white') + 
  geom_sf( data = west_cold_deserts_allots, 
           fill = ecogroup_colors["West Cold Deserts"], 
           color = 'white', size = 0.05) + 
  xlim( c(-121, -111)) + 
  ylim( c(36.2, 45.5)) + 
  geom_sf(data = BLM_offices %>% distinct(Shape), 
          fill = NA, size = 0.1, alpha = 0.8) + 
  geom_sf(fill = NA, size = 0.5, alpha = 0.5) + 
  geom_sf(data = BLM_offices %>% distinct(Shape) %>% 
            filter( ADM_UNIT_CD == "IDT01000"), 
          fill = NA, size = 0.4, color = 'blue', alpha = 0.3) + 
  ggspatial::annotation_scale( location = 'br')  + 
  theme( legend.position = 'none', 
         panel.grid = element_blank(), 
         #axis.text = element_blank(), 
         #axis.ticks = element_blank(), 
         panel.border = element_rect(fill = NA), 
         legend.box.background = element_rect(color = 1, size = 0.2)) 


regional_map  + 
  theme(axis.text = element_text( size = 6)) + 
  ggsave( 'output/figures/West_Cold_Desert_Map.png', 
          width = 4, height = 5, units = 'in')


# field office map
library(raster)
focal_allot <- raster::brick( 'data/RAP_EE_exports/example_allotment_cover (1).tif')
focal_allot <- projectRaster(focal_allot , crs = CRS('EPSG:4326')) 

afgc0 <- 
  focal_allot$AFGC %>% 
  as.data.frame(xy = T) 

afgc0$AFGC[afgc0$AFGC == 0 ] <- NA

afgc1 <- 
  focal_allot$AFGC_1 %>% 
  as.data.frame(xy = T) %>%
  rename( 'AFGC' = 'AFGC_1')

afgc1$AFGC[afgc1$AFGC == 0 ] <- NA

afgc2 <- 
  focal_allot$AFGC_2 %>% 
  as.data.frame(xy = T) %>%
  rename( 'AFGC' = 'AFGC_2')

afgc2$AFGC[afgc2$AFGC == 0 ] <- NA

unloadNamespace('raster')
source('code/analysis/parameters.R')

BLM_offices <- 
  read_rds('data/temp/cleaned_BLM_field_office_shapes.rds') %>% 
  st_transform( crs = 'EPSG:4326') %>% 
  filter( str_detect( ADM_UNIT_CD , 'ID|NV'))

#
Jarbidge_FO <- 
  BLM_offices %>% 
  filter( ADM_UNIT_CD == "IDT01000" ) 

Jarbidge_allotments <- 
  allotment_shapes %>% 
  filter( ADMIN_ST == "ID", ADM_OFC_CD == 'T01000') %>% 
  st_transform(crs = 'EPSG:4326')


Jarbidge_FO <- 
  Jarbidge_allotments %>% 
  dplyr::select( geometry) %>%
  bind_rows(
  Jarbidge_FO %>% 
    dplyr::select(Shape) %>% 
    rename('geometry' = Shape) ) %>% 
  summarise( geometry = st_union(geometry))  %>%
  st_make_valid()
  
Jarbidge_FO %>% 
  ggplot() + 
  geom_sf(size = 0.5, 
          fill = 'white') + 
  geom_sf(data = Jarbidge_allotments, 
          size = 0.2, 
          fill = ecogroup_colors['W Cold Deserts'], 
          color = 'white')  + 
  geom_sf(data = Jarbidge_allotments %>% filter( uname == 5339), 
          size = 0.8, 
          fill = ecogroup_colors['W Cold Deserts'], 
          color = 'blue', show.legend = F)  + 
  geom_tile( data = afgc0, aes( x = x, y= y, fill = AFGC ), 
             color = NA, show.legend = F) + 
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.8, -114.7) ) + 
  ylim( c(41.9, 43.0)) + 
  geom_sf(size = 0.5, 
          color = 'black', 
          fill = NA) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
    axis.title =  element_blank()) +
  ggspatial::annotation_scale( location = 'br')  + 
  ggsave( 'output/figures/JarbidgeFieldOffice.png', 
          width = 4, height = 5)

# -------------------------------- # 

# single allotment --------- # 
Jarbidge_FO %>% 
  ggplot() + 
  geom_sf(size = 0.5, 
          fill = 'white') + 
  geom_sf(data = Jarbidge_allotments, 
          size = 0.2, 
          alpha = 0.5, 
          fill = ecogroup_colors['W Cold Deserts'], 
          color = 'white')  + 
  geom_tile( data = afgc0, aes( x = x, y= y, fill = AFGC ), 
             color = NA, show.legend = F, alpha = 0.8) +   
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = ecogroup_colors['West Cold Deserts'], color = 'black', size = 0.5) + 
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.08, -114.94) ) + 
  ylim( c(42.135, 42.26)) + 
  #xlim( c(-115.08, -114.8) ) + 
  #ylim( c(42.085, 42.26)) + 
  geom_sf(size = 0.5, 
          color = 'black', 
          fill = NA) + 
  theme_bw() + 
  theme(#panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title =  element_blank()) +
  #ggspatial::annotation_scale( location = 'br')  + 
  ggsave( 'output/figures/SingleAllotment.png', bg = 'transparent', 
          width = 4, height = 5)

# Floating image on white 
Jarbidge_FO %>%
  ggplot() + 
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = ecogroup_colors['West Cold Deserts'], color = 'black', size = 0.5) + 
  geom_tile( data = afgc0, aes( x = x, y= y, fill = AFGC ), 
             color = NA, show.legend = F, alpha = 0.8) +   
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = NA, color = 'black', size = 0.6) +
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.08, -114.94) ) + 
  ylim( c(42.135, 42.26)) + 
  theme_bw() + 
  theme( panel.border = element_blank(),
         axis.line = element_blank(), 
         panel.grid = element_blank(), 
         panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
         plot.background = element_rect(fill = 'transparent', colour = NA),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title =  element_blank()) +
  #ggspatial::annotation_scale( location = 'br')   + 
  ggsave( 'output/figures/SingleAllotment_year1.png', 
          width = 4, height = 5, bg = 'transparent')


Jarbidge_FO %>%
  ggplot() + 
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = ecogroup_colors['West Cold Deserts'], color = 'black', size = 0.5) + 
  geom_tile( data = afgc1, aes( x = x, y= y, fill = AFGC ), 
             color = NA, show.legend = F, alpha = 0.8) +   
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = NA, color = 'black', size = 0.6) +
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.08, -114.94) ) + 
  ylim( c(42.135, 42.26)) + 
  theme_bw() + 
  theme( panel.border = element_blank(),
    axis.line = element_blank(), 
         panel.grid = element_blank(), 
         panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
         plot.background = element_rect(fill = 'transparent', colour = NA),
         axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title =  element_blank()) +
  #ggspatial::annotation_scale( location = 'br')   + 
  ggsave( 'output/figures/SingleAllotment_year2.png', 
        width = 4, height = 5, bg = 'transparent')

#

Jarbidge_FO %>%
  ggplot() + 
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = ecogroup_colors['West Cold Deserts'], color = 'black', size = 0.5) + 
  geom_tile( data = afgc2, aes( x = x, y= y, fill = AFGC ), 
             color = NA, show.legend = F, alpha = 0.8) +   
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = NA, color = 'black', size = 0.1) +
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.08, -114.94) ) + 
  ylim( c(42.135, 42.26)) + 
  theme_bw() + 
  theme( panel.border = element_blank(), 
         axis.line = element_blank(), 
         panel.grid = element_blank(), 
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title =  element_blank(), 
         panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
         plot.background = element_rect(fill = 'transparent', colour = NA)) +
  ggspatial::annotation_scale( location = 'br')  + 
  ggsave( 'output/figures/SingleAllotment_year3.png', 
          width = 1.79, height = 2.24, bg = 'transparent')




