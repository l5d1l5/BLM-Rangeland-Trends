rm(list = ls())

library(tidyverse)
library(sf)

# Run "clean_allotment_shapes" first 

allotment_info <- readRDS(file = 'data/temp/BLM_allotments_sf.rds') %>%
  ungroup() %>%
  st_centroid() %>%
  st_transform( "epsg:4326") %>%
  mutate( 
    xy = st_coordinates(SHAPE)) %>% 
  st_drop_geometry()  %>% 
  mutate( lon = xy[,1], lat = xy[,2]) %>% 
  select( - xy )

adm_districts <- sf::read_sf('data/BLM_National_Administrative_Units/admu.gdb/', 
                           layer = 'blm_natl_admu_dist_poly_webpub') %>% 
  filter( ADMIN_ST != 'AK' )

adm_offices <- sf::read_sf('data/BLM_National_Administrative_Units/admu.gdb/', 
                           layer = 'blm_natl_admu_field_poly_webpub') %>%
  filter( ADMIN_ST != 'AK')  

# Add Las Cruces District/Office: missing from field office Layer
# but is present in District layer 
Las_Cruces <- adm_districts %>% 
  filter( ADMIN_ST == 'NM') %>% 
  filter( PARENT_CD == 'NML00000') 

adm_offices <- adm_offices %>% 
  rbind( 
    Las_Cruces %>%
      mutate( ADM_UNIT_CD = PARENT_CD) %>%
      mutate( ADMU_NAME = PARENT_NAME) %>%
      mutate( BLM_ORG_TYPE = 'Field' ) %>% 
      mutate( APPRV_DT = NA, EFF_DT = NA, ADMU_ST_URL = NA) ) 

# Standardize admin names 
# A couple of ADMU_UNIT_CD's have more than one entry
# group by ADMU_UNIT_CD and take top row within group (1 per group)
adm_offices <- 
  adm_offices %>% 
  group_by( ADM_UNIT_CD ) %>% 
  arrange( ADM_UNIT_CD , Shape_Area ) %>% 
  filter( row_number() == 1 ) %>% 
  mutate( ADMU_NAME = str_to_upper(str_trim(str_squish(ADMU_NAME))), 
          PARENT_NAME = str_to_upper( str_trim(str_squish(PARENT_NAME)))) %>%
  select( ADM_UNIT_CD, ADMU_NAME, BLM_ORG_TYPE, PARENT_CD, PARENT_NAME)

adm_districts <- 
  adm_districts %>% 
  mutate( PARENT_NAME = str_to_upper(str_trim(str_squish(PARENT_NAME))))

# all parent names match except Oklahoma 
adm_offices$PARENT_NAME[ !adm_offices$PARENT_NAME %in% adm_districts$PARENT_NAME ] 

# Minor edits to field office IDs so that they can be joined to District
allotment_info <- 
  allotment_info %>% 
  mutate( ADM_OFC_CD = ifelse(ADM_OFC_CD == '02000', 'A02000', ADM_OFC_CD) ) %>%
  mutate( ADM_OFC_CD_PREFIX = str_sub(ADM_OFC_CD, 1, 2), 
          ADM_OFC_CD_NUMBER = as.numeric( str_extract( allotment_info$ADM_OFC_CD, '[0-9]{4}$') )) %>% 
  mutate( ADM_OFC_CD_NUMBER = str_pad(floor( as.numeric( ADM_OFC_CD_NUMBER)/1000), 4, side = 'right', pad = '0' )) %>% 
  mutate( ADM_OFC_CD = paste0( ADM_OFC_CD_PREFIX, ADM_OFC_CD_NUMBER)) 

allotment_info <- 
  allotment_info %>% 
  mutate( ADM_UNIT_CD_original = paste0( ADMIN_ST, ADM_OFC_CD)) %>%
  mutate( ADM_UNIT_CD = 
            str_replace_all(ADM_UNIT_CD_original, c('AZA02000' = 'AZA01000', 
                                           'AZG03000' = 'AZG02000', 
                                           'AZP03000' = 'AZP01000', 
                                           'AZP04000' = 'AZP02000', 
                                           'CON04000' = 'COG02000', 
                                           'COS08000' = 'COG01000', 
                                           'UTC04000' = 'AZA01000', 
                                           'UTN05000' = 'CON05000', 
                                           'UTN06000' = 'AZA01000', 
                                           'MT000000' = 'MTB05000', 
                                           'NM000000' = 'NMP02000', 
                                           'ORB00000' = 'ORB05000', 
                                           'UTP00000' = 'UTP02000')))

stopifnot( all( !is.na( allotment_info$ADM_OFC_CD ) ))
stopifnot( all( str_length(allotment_info$ADM_UNIT_CD) == 8 ))

# Test that Admin codes from allotments match office admin codes 
adm_unit_cd <- allotment_info$ADM_UNIT_CD %>% unique 

adm_unit_cd_field_poly <- 
  adm_offices %>% 
  st_drop_geometry( ) %>% 
  pull(ADM_UNIT_CD ) %>% unique()

adm_unit_cd[ !adm_unit_cd %in% adm_unit_cd_field_poly ] %>% sort 
adm_unit_cd_field_poly [ !adm_unit_cd_field_poly %in% adm_unit_cd] %>% sort

#
admin_office_info <- 
  adm_offices %>% st_drop_geometry()

allotment_info <- 
  allotment_info %>% 
  select( uname, ALLOT_NO, ALLOT_NAME, LAST_DATE, area, acres, ADM_UNIT_CD, ADMIN_ST, lat, lon ) %>% 
  left_join(admin_office_info, by = 'ADM_UNIT_CD') 

# check for duplicates 
allotment_info %>%
  group_by(uname ) %>%
  filter( n() > 1 )

allotment_info %>% 
  filter( is.na(PARENT_NAME))

allotment_info %>% 
  filter( is.na( ADMU_NAME))

# Fix North Dakota and South Dakota field office allotments: 

allotment_info %>% 
  filter( ADMIN_ST == 'MT') %>% 
  group_by(ADMIN_ST, ADMU_NAME  ) %>%
  summarise( n_distinct(uname) ) # North dakota only has one allotment! 

admin_office_info %>% filter( ADMU_NAME == 'NORTH DAKOTA FIELD OFFICE')
admin_office_info %>% filter( ADMU_NAME == 'SOUTH DAKOTA FIELD OFFICE')

# Show distribution of allotment field offices in ND and SD
allotment_info %>% 
  filter( ADMU_NAME %in% c( 'SOUTH DAKOTA FIELD OFFICE', 'NORTH DAKOTA FIELD OFFICE')) %>% 
  select( uname, ADMU_NAME, lat, lon ) %>% 
  st_as_sf(coords = c('lon', 'lat'), 
           crs = 'epsg:4326') %>% 
  st_transform(crs = st_crs( adm_offices)) %>% 
  ggplot() + 
  geom_sf(data = adm_offices %>% 
            filter(ADMU_NAME %in% c( 'SOUTH DAKOTA FIELD OFFICE', 'NORTH DAKOTA FIELD OFFICE') ), 
          aes( color = ADMU_NAME) ) + 
  geom_sf( aes( color = ADMU_NAME)) + 
  ggtitle("N. Dakota and S. Dakota -- original field office designation") +
  ggsave(filename = 'output/figures/ND_SD_field_offices.png', 
         width = 8, height = 8, units = 'in',dpi = 'print')

# Re-assign all allotments in SD Field Office North of 46.3 Lon to ND field office
allotment_info <- 
  allotment_info %>% 
  mutate( ADMU_NAME = ifelse( ADMU_NAME == 'SOUTH DAKOTA FIELD OFFICE' & lat >= 46.3, 
                              'NORTH DAKOTA FIELD OFFICE', ADMU_NAME)) 
#  ------------------------- # 

allotment_info %>% 
  filter( ADMIN_ST == "MT", 
          ADMU_NAME %in% c( 'SOUTH DAKOTA FIELD OFFICE', 'NORTH DAKOTA FIELD OFFICE')) %>% 
  select( uname, ADMU_NAME, lat, lon ) %>% 
  st_as_sf(coords = c('lon', 'lat'), 
           crs = 'epsg:4326') %>% 
  st_transform(crs = st_crs( adm_offices)) %>% 
  ggplot() +  
  geom_sf(data = adm_offices %>% 
            filter(ADMU_NAME %in% c( 'SOUTH DAKOTA FIELD OFFICE', 'NORTH DAKOTA FIELD OFFICE') ), 
          aes( color = ADMU_NAME), alpha = 0.1 ) + 
  geom_sf( aes( color = ADMU_NAME)) + 
  ggtitle("N. Dakota and S. Dakota -- re-assigned field office designation") +
  ggsave(filename = 'output/figures/ND_SD_field_offices2.png', 
         width = 8, height = 8, units = 'in',dpi = 'print')


allotment_info %>% 
  filter( ADMIN_ST == 'MT') %>% 
  group_by(ADMIN_ST, ADMU_NAME  ) %>%
  summarise( n_distinct(uname) ) # Now there are 52 North Dakota allotments

# Write as table.  
# Join with allotment shapes on uname later for maps 
# Join with summary stats by uname from google earth engine later 
names( allotment_info ) <- str_to_lower(str_trim(str_squish(names(allotment_info))))

allotment_info %>% 
  write_rds( file = 'data/temp/allotment_info.rds')

#
adm_offices %>% 
  write_rds(file = 'data/temp/cleaned_BLM_field_office_shapes.rds')

adm_districts %>% 
  write_rds(file = 'data/temp/cleaned_BLM_district_shapes.rds')
