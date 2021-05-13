# Find ecoregion for each allotment using spatial join  
# Find BLM Field Office IDs and District IDs using spatial join
rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)
source('code/analysis/plot_tools.R')

# input -------------------------- # 
allotment_centers <- readRDS(file = 'data/temp/BLM_allotments_sf.rds') %>%
  ungroup() %>%
  st_centroid() %>%
  select( uname ) 

state_bounds <- tigris::states(resolution = '20m')
ER <- read_sf('data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')
#--------------------------------- # 

# add ecogroup column to allotment table: 
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)
# Get ecogroup 

ER <- ER %>% 
  st_transform(crs = st_crs(allotment_centers))

# 
sel_states <- state.name[ state.abb %in% c('OR', 'WA', 'MT', 
                                           'ID', 'ND', 'SD', 
                                           'UT', 'CO', 'AZ', 
                                           'NM', 'CA', 'NV', 'WY', 'NE') ] 
state_bounds <- 
  state_bounds %>% 
  filter( NAME %in% sel_states ) %>%
  st_transform(crs = st_crs(allotment_centers)) 

# Use Level I Ecoregions in all cases except: 
# 1. Break deserts to level II ecoregions: warm deserts and cold deserts
# 2. Lump Southern Semi-Arid Highlands with Temperate Sierras = "S. Mts."
# 3. Split Cold Deserts (ERII) into West and East  
ecoregion_def <- 
  ER %>%
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

# "Ecogroup" is new field for lumping some ecoregions together and 
# splitting some ecoregions I into sub regions for analysis 
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
  left_join(ecoregion_labels , by = 'Ecoregion') 

ER <- 
  ER %>%
  st_simplify(preserveTopology = T) %>%
  st_make_valid() %>% 
  st_cast( 'MULTIPOLYGON')

ER <- ER %>% 
  distinct( L3_KEY , STATE_NAME) %>%
  filter( STATE_NAME %in% state.name[state.abb %in% WESTERN_STATES]) %>% 
  left_join(
    ecoregion_def %>% distinct( L3_KEY, Ecoregion, ecogroup ) 
  )

ER <- 
  ER %>% 
  filter( !is.na(ecogroup)) %>%
  group_by( ecogroup ) %>% 
  summarise( geometry = st_union( geometry )) %>%
  st_make_valid()

ER <- ER %>% 
  st_simplify( preserveTopology = T, dTolerance = 500 ) %>% 
  st_make_valid() 

# Get allotment ecoregions 
allotment_centers_with_ecogroup <- 
  allotment_centers %>% 
  st_join(ER, left = T, join = st_within )

missing <- allotment_centers_with_ecogroup %>% filter( is.na(ecogroup))

missing <- missing %>%
  st_join(ER %>% st_buffer( dist = 200), largest = T, left = T, join = st_within)

# fill in missing ecogroup 
allotment_centers_with_ecogroup <- 
  allotment_centers_with_ecogroup %>%
  left_join(missing %>% st_drop_geometry(), by = 'uname') %>% 
  mutate( ecogroup = ifelse( is.na(ecogroup), ecogroup.y, ecogroup )) %>% 
  select( uname, ecogroup ) 

# Reduce duplicates with ambiguous joins 
allotment_centers_with_ecogroup <- 
  allotment_centers_with_ecogroup %>% 
  group_by( uname ) %>% 
  filter( row_number() == 1)


# Check that all allotments have an Ecogroup
stopifnot(allotment_centers_with_ecogroup %>% 
            filter(is.na(ecogroup)) %>%nrow == 0)

stopifnot( allotment_centers_with_ecogroup %>% 
             group_by( uname ) %>% 
             filter(n() > 1 ) %>% nrow == 0)

allotment_ecogroups <- 
  allotment_centers_with_ecogroup %>% 
  ungroup() %>% 
  st_drop_geometry() 

# now add ecogroup information to the allotments table 
create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE ecogroup(uname INT, 
                          ecogroup VARCHAR, 
                          FOREIGN KEY (uname) REFERENCES allotments (uname));", 
                             pattern = '\n'))

RPostgres::dbSendQuery(con, create_table_query) # Create empty table 


#
alter_table_query <- "ALTER TABLE allotments ADD ecogroup VARCHAR;"
dbSendQuery(con, alter_table_query)

RPostgres::dbWriteTable(con, name = 'ecogroup', 
                        value = allotment_ecogroups, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

# Insert ecogroup 
res <- dbSendQuery(con, 'UPDATE allotments
                          SET ecogroup = ecogroup.ecogroup 
                          FROM ecogroup 
                          WHERE ecogroup.uname = allotments.uname;')

# test that the insert worked 
test1 <- tbl( con, "allotments") %>% select( uname, ecogroup ) %>% collect()
test2 <- tbl( con, "ecogroup") %>% select(uname, ecogroup) %>% collect()

stopifnot( 
  all.equal( 
  test1 %>% arrange( uname ), 
  test2 %>% arrange( uname ) )
)

# Save Ecogroup shapes 
ER %>% 
  write_rds(file = 'data/temp/simplified_ecogroup_shapefile.rds')

state_bounds %>%
  st_transform(crs = st_crs(ER)) %>% 
  filter( STUSPS %in% WESTERN_STATES) %>% 
  write_rds(file = 'data/temp/western_states_outlines_shapefile.rds')

