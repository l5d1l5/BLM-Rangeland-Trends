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


allotments <- read_rds('output/BLM_cleaned_shape_sf.RDS')

allotment_centroids <- 
  allotments %>% 
  select(uname) %>% 
  st_centroid()

allotment_info <-
  allotments %>%
  st_drop_geometry()

allotment_info <- 
  allotment_info %>% 
  select( -starts_with('US_'), 
          -c(L3_KEY:L1_KEY), -STATE_NAME)

stopifnot( !any( allotments %>% duplicated() ) )

# Join with elevation 
allotment_info <- 
  allotment_info %>% left_join( 
    read_rds('data/elevation.rds'), by = 'uname')

# Creat table 
allotment_info$acres <- as.numeric( allotment_info$acres )
res_length <- lapply( allotment_info, function(x) { range( str_length(x) )  })
res_type <- lapply( allotment_info, function(x) { class(x)  })

# for create table query 
paste( paste( tolower( names(allotment_info) ), res_type, res_length), 
       collapse = ', ')

create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE allotments(
                             uname INT, 
                             adm_unit_cd CHAR(8), 
                             admu_name VARCHAR, 
                             blm_org_type VARCHAR, 
                             parent_cd VARCHAR, 
                             parent_name VARCHAR, 
                             admin_st CHAR(2), 
                             na_l3code VARCHAR, 
                             na_l3name VARCHAR, 
                             na_l2code VARCHAR, 
                             na_l2name VARCHAR, 
                             na_l1code CHAR(2), 
                             na_l1name VARCHAR, 
                             epa_region INT, 
                             admin_st_y CHAR(2), 
                             adm_ofc_cd VARCHAR, 
                             allot_name VARCHAR, 
                             acres NUMERIC, 
                             elevation NUMERIC,
      PRIMARY KEY(uname));", pattern = '\n'))

#dbRemoveTable(con, 'allotments')

RPostgres::dbSendQuery(con, create_table_query) # Create empty table 

names( allotment_info) <- tolower( names( data.frame( allotment_info)) )
names( allotment_info)  <- str_replace_all(names(allotment_info), pattern = '\\.', replace = '_')

allotment_info <- 
  allotment_info %>%
  rename( admin_st = admin_st_x)

RPostgres::dbWriteTable(con, 
             name = 'allotments', 
             value = allotment_info, 
             row.names = F, 
             overwrite = F, 
             append = T)

# Allotment boundaries --------------------- # 
allotments <- 
  allotments %>%
  select( uname )

allotments <- allotments %>% st_cast( 'MULTIPOLYGON') %>% st_transform(4269)

stopifnot( !any( allotments %>% duplicated() ) )

# Write BLM shapes to database 
dbListTables(con)
dbRemoveTable(con, 'allotment_shapes')

st_write(allotments, con, 
         layer = 'allotment_shapes')

# Tests 
test_spatial_query <- 
'SELECT uname 
FROM allotment_shapes  
 WHERE ST_Contains( "SHAPE", 
ST_GeomFromText(\'POINT(-112.5191 45.5195)\', 4269));'

test_spatial_query <- str_remove_all(test_spatial_query, '\\n')

res <- dbGetQuery(con, test_spatial_query)
stopifnot(res$uname == 6102) # Test that the correct uname is returned

rm( res, test_spatial_query )

test_join_query <- 
  'SELECT a.uname
  FROM allotments a, allotment_shapes b
  WHERE 
    a.uname = b.uname
  LIMIT 10;'

test_join_query <- 
  str_remove_all(test_join_query, '\\n') %>%str_squish()

res <- dbGetQuery(con, test_join_query)
res %>% View
rm( res )

#
test_join_query <- 
  'SELECT a.uname, a.allot_name, a.acres 
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
# For montana only 
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
# Add allotment Centroids 

allotment_centroids <- allotment_centroids %>% st_transform(4269 )

st_write( allotment_centroids, con, layer = 'allotment_centroids' )

#clean up 
dbDisconnect(conn = con )
rm(list = ls())

