# load files into Postgres database 
rm(list = ls() )
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )
require( rpostgis )
library(sf)

con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = 'blm', 
    user = 'andy', 
    port = '5432'
  )


allotment_shapes <- read_rds('data/temp/BLM_allotments_sf.RDS')

allotment_info <- read_csv('data/temp/cleaned_allotment_info.csv')


allotment_centers <- 
  allotment_shapes %>% 
  ungroup() %>%
  st_centroids()


#allotment_centers <- read_rds('data/temp/allotment_centers_with_ecogroup_and_BLM_office.rds')
st_crs(allotment_shapes) == st_crs(allotment_centers)

allotment_centroids <- 
  allotment_centers %>% 
  distinct(uname)

allotment_info <-
  allotment_centers %>%
  st_drop_geometry()

stopifnot( !any( allotment_info %>% duplicated() ) ) # test for duplicates

# Join with elevation 
allotment_info <- 
  allotment_info %>% 
  left_join( read_rds('data/elevation.rds'), by = 'uname')

allotment_info <- 
  allotment_info %>% 
  select( uname:admin_st, 
          na_l3name:epa_region, 
          ecogroup, acres, elevation ) %>% 
  mutate( acres = as.numeric(acres))


# for create table query 
cbind( names(allotment_info), lapply( allotment_info, class) , lapply( allotment_info, function(x) max(str_length(x))))


create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE allotments(
                             uname INT, 
                             allot_name VARCHAR, 
                             admu_name VARCHAR, 
                             adm_unit_cd CHAR(8), 
                             parent_cd VARCHAR, 
                             parent_name VARCHAR, 
                             adm_ofc_cd VARCHAR, 
                             admin_st CHAR(2), 
                             na_l3name VARCHAR, 
                             na_l2name VARCHAR, 
                             na_l1name VARCHAR, 
                             epa_region INT, 
                             ecogroup VARCHAR, 
                             acres NUMERIC, 
                             elevation NUMERIC, 
      PRIMARY KEY(uname));", pattern = '\n'))

RPostgres::dbSendQuery(con, create_table_query) # Create empty table 

RPostgres::dbWriteTable(con, 
             name = 'allotments', 
             value = allotment_info, 
             row.names = F, 
             overwrite = F, 
             append = T)

# Allotment boundaries --------------------- # 

alltoment_shapes <- 
  allotment_shapes %>% 
  distinct(uname)

stopifnot( !any( allotment_shapes %>% duplicated() ) )

# Write BLM shapes to database 
st_write(allotment_shapes, con, 
         layer = 'allotment_shapes')

#  Load annual data 
annual_data <- read_rds('data/allotment_data_long.rds')

create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE annual_data( 
      uname INT, 
      year INT, 
      type VARCHAR, 
      value NUMERIC, 
      PRIMARY KEY(uname, year, type), 
      CONSTRAINT fk_uname FOREIGN KEY(uname) REFERENCES allotments(uname));", pattern = '\n'))

create_table_query

RPostgres::dbSendQuery(con, create_table_query)

RPostgres::dbWriteTable(con, 
                        name = 'annual_data', 
                        value = annual_data, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

rm(res, annual_data)

# 16-day production data 
#npp_16 <- read_csv('data//allotment_16_day_NPP_by_feature.csv')

# TODO Fix NPP Exports 
# 16 Day NPP exports are empty of values.  Need to fix 

create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE npp_16_day( 
      uname INT, 
      year INT, 
      doy INT, 
      afg NUMERIC, 
      pfg NUMERIC, 
      PRIMARY KEY(uname, year, doy), 
      CONSTRAINT fk_uname FOREIGN KEY(uname) REFERENCES allotments(uname));", 
                             pattern = '\n'))

# create_table_query
# dbRemoveTable(con, 'npp_16_day')
# RPostgres::dbSendQuery(con, create_table_query)
# 
# RPostgres::dbWriteTable(con, 
#                         name = 'npp_16_day', 
#                         value = npp_16_day, 
#                         row.names = F, 
#                         overwrite = F, 
#                         append = T)

rm(res, npp_16_day)
# Add allotment Centroids 
st_write( allotment_centroids, 
          con, 
  layer = 'allotment_centroids' )

#clean up 
dbDisconnect(conn = con )
rm(list = ls())

