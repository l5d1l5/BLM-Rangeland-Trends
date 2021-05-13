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

# if exists must remove other allotment tables 
#dbRemoveTable(con, 'allotment_centroids')
#dbRemoveTable(con, 'allotment_shapes')
#dbRemoveTable(con, 'annual_data')
dbRemoveTable(con, 'allotments')

allotment_info <- readRDS(file = 'data/temp/allotment_info.rds') %>%
  ungroup() 

allotment_info <- allotment_info %>% select( uname, allot_no, allot_name, 
                           last_date, area, acres, 
                           adm_unit_cd, admin_st, admu_name,
                           parent_cd, parent_name, lon, lat) %>% 
  mutate( acres = as.numeric(acres), area = as.numeric(area))

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

