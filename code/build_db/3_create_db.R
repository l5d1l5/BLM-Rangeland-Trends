# load files into Postgres database 
rm(list = ls() )
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )
require( rpostgis )
library(sf)

# Create Postgres db with shell 
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

climate_regions <- list(
  NW = c('OR', 'WA', 'ID'), 
  NR = c('MT', 'WY'), 
  SW = c('UT', 'CO', 'NM', 'AZ'), 
  W = c('CA', 'NV') )

climate_regions <- stack(climate_regions ) %>% 
  rename( 'climate_region' = ind, 'admin_st' = values  )

# Must remove old tables before adding 

cur_tables <- dbListTables(con)
cur_tables <- cur_tables[ -which( cur_tables %in% c('geography_columns', 'geometry_columns', 'spatial_ref_sys')) ] 
if( length(cur_tables) > 0 ) { 
  lapply( cur_tables[ - which( cur_tables == 'allotments' )], dbRemoveTable, conn = con )
  dbRemoveTable(con, 'allotments') # remove allotments last

  # dbRemoveTable(con, 'annual_data')
  # dbRemoveTable(con, 'annual_climate')
  # dbRemoveTable(con, 'elevation')
  # dbRemoveTable(con, 'npp_16')
  # dbRemoveTable(con, 'ecogroup')
  # dbRemoveTable(con, 'allotments')
}

allotment_info <- 
  readRDS(file = 'data/temp/allotment_info.rds') %>% 
  mutate( acres = as.numeric(acres ))

allotment_info <- allotment_info %>% 
  select( uname, allot_no, allot_name, 
                           last_date, area, acres, 
                           adm_unit_cd, admin_st, admu_name,
                           parent_cd, parent_name, lon, lat) %>% 
  mutate( acres = as.numeric(acres), area = as.numeric(area)) %>% 
  left_join( climate_regions)


create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE allotments(uname INTEGER PRIMARY KEY, 
                             allot_no VARCHAR, 
                             allot_name VARCHAR, 
                             last_date DATE, 
                             area NUMERIC, 
                             acres NUMERIC, 
                             adm_unit_cd CHAR(8), 
                             admin_st CHAR(2), 
                             climate_region CHAR(2),
                             admu_name VARCHAR, 
                             parent_cd CHAR(8), 
                             parent_name VARCHAR, 
                             lon NUMERIC, 
                             lat NUMERIC);", pattern = '\n'))

RPostgres::dbSendQuery(con, create_table_query) # Create empty table 

# Write allotment info to the table 
RPostgres::dbWriteTable(con, 
                        name = 'allotments', 
                        value = allotment_info, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

dbDisconnect(con)

