rm(list = ls())
library(sf)
library(tidyverse)
library(parallel)

numcores <- 4

mtbs <- st_read('~/Downloads/mtbs_perims_DD/mtbs_perims_DD.shp')

states <- 
  read_rds('data/temp/western_states_outlines_shapefile.rds')

blm <- read_rds('data/temp/BLM_allotments_sf.rds')

mtbs <- mtbs %>% 
  st_transform(crs = 'epsg:5070') %>% 
  st_simplify( dTolerance = 100) %>%
  st_make_valid()

mtbs <- mtbs %>% st_crop(states) 
blm <- blm %>% st_crop(states)

mtbs_year <- mtbs %>% 
  mutate( year = lubridate::year( Ig_Date ) ) %>% 
  filter( !is.na(Event_ID), !is.na(Ig_Date))

# Join first to find which allotments were burned each year 
blm_mtbs <- blm %>% 
  st_join(mtbs_year, join = st_intersects) %>% 
  dplyr::select( uname, year, Event_ID, Ig_Date ) %>% 
  filter( !is.na(year))

get_burned_area <- function( x, y, df1, df2 ) { 
  a <- df1 %>% filter( uname == x )
  b <- df2 %>% filter( year == y )
  
  a %>% 
    st_intersection(b) %>% 
    mutate( burned_area = st_area(SHAPE) ) %>% 
    st_drop_geometry() %>% 
    dplyr::select( uname, year, burned_area )
}

uname_year_combos <- blm_mtbs %>% 
  st_drop_geometry( ) %>% 
  group_by( year, uname) %>% 
  distinct(uname, year, n = n())

uname_year_combos$Event_ID <-  NA
uname_year_combos$area_burned <- NA

for( i in 1:nrow(uname_year_combos)){ 

  temp <- uname_year_combos[i, ]

  temp_id <- blm_mtbs %>% 
    filter( uname == temp$uname, year == temp$year) %>% 
    pull( Event_ID ) 

  temp_burns <- mtbs %>% 
    filter( Event_ID %in% temp_id) %>% 
    ungroup() %>% 
    summarise( Event_ID = list(Event_ID), geometry = st_union(geometry))

  area_burned <- blm %>%
    filter(uname == temp$uname) %>%
    mutate(area_burned = st_area(st_intersection(SHAPE,temp_burns$geometry))) 
  

  uname_year_combos$Event_ID[i] <- list( temp_id )
  uname_year_combos$area_burned[i] <- area_burned$area_burned
}


library(DBI)
library(dbplyr)
require( RPostgres  )
require( rpostgis )

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

#uname_year_combos %>% write_rds('data/temp/allotment_burns_by_year.rds')

uname_year_combos <- 
  uname_year_combos %>% 
  filter(!is.na(area_burned)) %>% 
  mutate( num_fires = n) %>% 
  select( uname, year, num_fires, area_burned) 

names(uname_year_combos)

create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE annual_burns( 
      uname INT, 
      year INT, 
      num_fires INT,
      area_burned NUMERIC,
      PRIMARY KEY(uname, year), 
      CONSTRAINT fk_uname FOREIGN KEY(uname) REFERENCES allotments(uname));", pattern = '\n'))

dbSendQuery(con, create_table_query)

RPostgres::dbWriteTable(con, 
                        name = 'annual_burns', 
                        value = uname_year_combos, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

dbDisconnect(con)
