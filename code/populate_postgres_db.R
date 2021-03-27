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

allotment_info <- read_rds('data/basic_allotment_info.rds')

allotment_info <- 
  allotment_info %>% 
  rowwise() %>% 
  mutate( ADMIN_ST = ifelse( !ADMIN_ST %in% state.abb , 
                             state.abb[STATE_NAME == state.name ], 
                             ADMIN_ST))  %>% 
  select( - STATE_NAME, -lat, -lon ) %>% 
  mutate( acres = as.numeric(acres )) %>% 
  ungroup() %>% 
  distinct() 

any( is.na( allotment_info$ADMIN_ST  ) )  # Check missing State Abb. 

# Creat table 
names(allotment_info)

create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE allotments( 
      uname INT, 
      admin_st CHAR(2), 
      adm_ofc_cd VARCHAR, 
      allot_name VARCHAR, 
      acres NUMERIC,
      na_l3name VARCHAR, 
      na_l2name VARCHAR, 
      na_l1name VARCHAR, 
      elevation NUMERIC, 
      PRIMARY KEY(uname));", pattern = '\n'))

create_table_query
dbRemoveTable(con, 'allotments')

RPostgres::dbSendQuery(con, create_table_query)

names( allotment_info) <- tolower( names( data.frame( allotment_info)) )

any( allotment_info %>% duplicated() )

RPostgres::dbWriteTable(con, 
             name = 'allotments', 
             value = allotment_info, 
             row.names = F, 
             overwrite = F, 
             append = T)

rm( allotment_info)

# Load Shapefile with allotment boundaries 
blm_shape <- read_rds('data/BLM_sf.rds')

blm_shape <- 
  blm_shape %>% 
  select( uid, uname, SHAPE )

any( blm_shape %>% duplicated() )

# Write BLM shapes to database 
st_write(blm_shape, con, fid_column_name = 'uid', 
         layer = 'allotment_shapes')

rm(blm_shape)

test_spatial_query <- 
'SELECT uname 
FROM allotment_shapes  
 WHERE ST_Contains( "SHAPE", 
ST_GeomFromText(\'POINT(-112.5191 45.5195)\', 4269));'

test_spatial_query <- str_remove_all(test_spatial_query, '\\n')

res <- dbGetQuery(con, test_spatial_query)

stopifnot(res$uname == 6102) # Test that the correct uname is returned

rm( res, testq )

test_join_query <- 
  'SELECT a.uname, b.uid 
  FROM allotments a, allotment_shapes b
  WHERE 
    a.uname = b.uname
  LIMIT 10;'

test_join_query <- 
  str_remove_all(test_join_query, '\\n') %>%str_squish()

res <- dbGetQuery(con, test_join_query)
res %>% View

# 

rm( res )

test_join_query <- 
  'SELECT a.uname, a.allot_name, a.acres, b.uid 
  FROM allotments a, (SELECT * FROM allotment_shapes  
  WHERE ST_Contains( "SHAPE", 
  ST_GeomFromText(\'POINT(-112.5191 45.5195)\', 4269)) ) b
  WHERE 
    a.uname = b.uname
  LIMIT 10;'

test_join_query <- 
  str_remove_all(test_join_query, '\\n') %>%str_squish()

test_join_query
res <- dbGetQuery(con, test_join_query)
res %>% View
rm(res)
   
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
npp_16_day <- read_csv('data/MT_allotment_data/MT_allotment_16_day_NPP_by_feature.csv')

npp_16_day <- 
  npp_16_day %>% 
  select(uname, year, doy, afgNPP, pfgNPP) 

names(npp_16_day) <- tolower(names( npp_16_day) )

npp_16_day <- 
  npp_16_day %>%
  rename( 'afg' = afgnpp, 'pfg' = pfgnpp)

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

create_table_query
dbRemoveTable(con, 'npp_16_day')
RPostgres::dbSendQuery(con, create_table_query)

RPostgres::dbWriteTable(con, 
                        name = 'npp_16_day', 
                        value = npp_16_day, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

rm(res, npp_16_day)
#clean up 

dbDisconnect(conn = con )
rm(list = ls())

