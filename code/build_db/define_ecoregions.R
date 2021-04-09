rm(list = ls())

library(tidyverse)
library(sf)

state_bounds <- tigris::states(resolution = '20m')

ecoregions <- read_sf('data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')

sel_states <- state.name[ state.abb %in% c('OR', 'WA', 'MT', 
                                           'ID', 'ND', 'SD', 
                                           'UT', 'CO', 'AZ', 
                                           'NM', 'CA', 'NV', 'WY', 'NE') ] 
state_bounds <- 
  state_bounds %>% 
  filter( NAME %in% sel_states ) %>%
  st_transform(crs = st_crs(ecoregions)) 

# Use Level I Ecoregions in all cases except: 
# 1. Break deserts to level II ecoregions: warm deserts and cold deserts
# 2. Lump Southern Semi-Arid Highlands with Temperate Sierras = "S. Mts."

ecoregion_def <- 
  ecoregions %>%
  filter( STATE_NAME %in% sel_states ) %>% 
  st_drop_geometry() %>% 
  mutate( Ecoregion = NA_L1NAME ) %>% 
  mutate( Ecoregion = 
            ifelse( Ecoregion %in% 
                      c('TEMPERATE SIERRAS', 
                      'SOUTHERN SEMI-ARID HIGHLANDS'), 
                      'AZ/NM HIGHLANDS', 
                    Ecoregion)) %>%
  mutate( Ecoregion =  
            ifelse( Ecoregion == 'NORTH AMERICAN DESERTS', 
                    NA_L2NAME, 
                    Ecoregion))  %>%
  mutate( Ecoregion = 
            ifelse( Ecoregion == "COLD DESERTS",
                    NA_L3NAME,
                    Ecoregion)) %>% 
  mutate( Ecoregion = 
            ifelse( Ecoregion %in% 
                      c('Wyoming Basin', 
                      'Colorado Plateaus', 
                      'Arizona/New Mexico Plateau'),
                    'EASTERN COLD DESERTS', 
                    Ecoregion)) %>% 
  mutate( Ecoregion = 
            ifelse( Ecoregion %in% 
             c( 'Central Basin and Range', 
                'Northern Basin and Range', 
                'Columbia Plateau', 
                'Snake River Plain'),
             'WESTERN COLD DESERTS', 
             Ecoregion))

ecoregion_labels <- 
  data.frame( 
  ecoregion_def %>% 
    distinct(Ecoregion) %>% arrange( Ecoregion)) 

ecoregion_labels$ecogroup <- 
  c('AZ/NM Highlands', 
    'E Cold Deserts', 
    'Great Plains',
    'Coastal Forests',
    'Mediterranean California',
    'NW Forested Mts', 
    'Warm Deserts',
    'W Cold Deserts')

ecoregion_def <- 
  ecoregion_def %>% 
  left_join(ecoregion_labels)

ecoregions <- 
  ecoregions %>% 
  left_join(ecoregion_def ) %>% 
  filter( !is.na(Ecoregion )) %>% 
  group_by( ecogroup ) %>% 
  summarise( geometry = st_union( geometry )) %>% 
  st_make_valid() %>% 
  st_simplify( dTolerance = 100 ) %>%
  st_make_valid()

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)


allotment_info <- tbl( con, 'allotments')
allotment_pts <- st_read(con, 'allotment_centroids')

allotment_pts <- 
  allotment_pts %>% 
  st_transform( crs = st_crs( ecoregions ))

pts_ecoregions <- 
  allotment_pts %>%
  st_join(ecoregions, join = st_within )

ecoregions <- 
  ecoregions %>%
  st_transform( crs = 4269 )


DBI::dbRemoveTable(con, "ecogroup_shapes")

st_write( ecoregions, con, layer = 'ecogroup_shapes')

DBI::dbListFields(con, 'ecogroup_shapes')

create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE allotment_ecogroups( 
      uname INT, 
      ecogroup VARCHAR NOT NULL,
      PRIMARY KEY(uname), 
      CONSTRAINT fk_uname FOREIGN KEY(uname) REFERENCES allotments(uname));", 
                             pattern = '\n'))

DBI::dbRemoveTable(con, 'allotment_ecogroups')

RPostgres::dbSendQuery(con, create_table_query)

allotment_ecogroups <- 
  pts_ecoregions %>% 
  st_drop_geometry() %>%
  distinct(uname, ecogroup_label)

RPostgres::dbWriteTable(con, 
                        name = 'allotment_ecogroups', 
                        value = allotment_ecogroups, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

DBI::dbListFields(con, 'allotment_ecogroups')

DBI::dbDisconnect(con)

rm(list = ls() )
