rm(list = ls())

library(DBI)
library(dbplyr)
require( RPostgres  )
library(tidyverse)
library(sf)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

lnames <- st_layers('data/BLM_National_Administrative_Units/admu.gdb/')

lnames

adm_districts <- sf::read_sf('data/BLM_National_Administrative_Units/admu.gdb/', 
                           layer = 'blm_natl_admu_dist_poly_webpub')

adm_offices <- sf::read_sf('data/BLM_National_Administrative_Units/admu.gdb/', 
                           layer = 'blm_natl_admu_field_poly_webpub')

adm_districts <- adm_districts %>% filter( ADMIN_ST != 'AK')
adm_offices <- adm_offices %>% filter( ADMIN_ST != 'AK')

adm_offices <- adm_offices %>% st_transform('epsg:4269')
adm_districts <- adm_districts %>% st_transform('epsg:4269')


#Rename columns and add to the Postgres DB
names_temp <- names(adm_offices) %>% str_to_lower() 
names( adm_offices )  <- names_temp
st_geometry(adm_offices) <- 'shape'

names_temp <- names(adm_districts) %>% str_to_lower() 
names( adm_districts )  <- names_temp
st_geometry(adm_districts) <- 'shape'

nrow(adm_districts)
length(unique(adm_districts$parent_cd) )
nrow( adm_offices)

length(unique(adm_offices$adm_unit_cd))

adm_offices[ duplicated(adm_offices$adm_unit_cd), ] 

adm_offices %>% filter( admin_st == 'OR', str_detect(admu_name, 'MEDFORD'))

adm_offices <- 
  adm_offices %>% 
  mutate( adm_ofc_cd = str_sub(adm_unit_cd, 3, 9))

st_write( adm_districts, con , layer = 'blm_districts')
st_write( adm_offices, con, layer = 'blm_field_offices' )

dbGetInfo(con, 'blm')
dbListTables(con)

dbListFields(con, 'allotment_shapes')
dbListFields(con, 'allotments')

dbListFields(con, 'blm_field_offices')
dbListFields(con, 'blm_districts')

tbl(con, 'allotments')
tbl(con, 'allotment_shapes')


test_spatial_query <- 
  str_squish(
  'SELECT a.uname, b.admin_st, b.parent_cd FROM 
  allotment_shapes AS a  
  JOIN blm_field_offices AS b 
  ON ST_Contains(b.shape, "SHAPE") 
  WHERE a.uname = 6055' )

test <- dbGetQuery(con, test_spatial_query)


#allotment_shapes ST_JOIN( )

dbListTables(con)

