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

#  Load annual data exported from Earth Engine 
AGB <- read_csv('data/RAP_EE_exports/allotment_biomass_by_year.csv') %>% 
  select( uname, year, contains('agb'))

cover <- read_csv( 'data/RAP_EE_exports/allotment_cover_by_year.csv') %>% 
  select( uname, year, AFGC, BG, LTR, PFGC, SHR, TREE)

elevation <- read_csv('data/RAP_EE_exports/allotment_elevation.csv', n_max = 10)  %>% 
  select( uname, mean )


# Join the response data together into one long data frame 
annual_data <- 
  AGB %>%
  left_join(cover, by = c('uname', 'year') ) %>%
  select(uname, year, afgAGB:pfgAGB, AFGC:TREE) %>% 
  pivot_longer(cols = c(afgAGB:TREE), names_to = 'type', values_to = 'value')  %>%
  filter( complete.cases(.))  

create_table_query <-
  str_squish( str_remove_all("
    CREATE TABLE annual_data( 
      uname INT, 
      year INT, 
      type VARCHAR, 
      value NUMERIC, 
      PRIMARY KEY(uname, year, type), 
      CONSTRAINT fk_uname FOREIGN KEY(uname) REFERENCES allotments(uname));", pattern = '\n'))

RPostgres::dbSendQuery(con, create_table_query)

RPostgres::dbWriteTable(con, 
                        name = 'annual_data', 
                        value = annual_data, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

rm(annual_data)

elevation <- elevation %>% rename( 'value' = mean )

create_table_query <- 
  str_squish( str_remove_all("
    CREATE TABLE elevation( 
      uname INT, 
      value NUMERIC, 
      PRIMARY KEY(uname), 
      CONSTRAINT fk_uname FOREIGN KEY(uname) REFERENCES allotments(uname));", pattern = '\n'))


RPostgres::dbSendQuery(con, create_table_query)

RPostgres::dbWriteTable(con, 
                        name = 'elevation', 
                        value = elevation, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)



# CLIMATE ------------------------------------- # 
climate <- read_csv('data/RAP_EE_exports/allotment_climate_by_year.csv') %>%
  select( uname, year, pdsi:tavg )

create_table_query <- 
  str_squish( str_remove_all("
    CREATE TABLE annual_climate( 
      uname INT, 
      year INT, 
      pdsi NUMERIC,
      pr NUMERIC,
      tavg NUMERIC,
      PRIMARY KEY(uname, year), 
      CONSTRAINT fk_uname FOREIGN KEY(uname) REFERENCES allotments(uname));", pattern = '\n'))


RPostgres::dbSendQuery(con, create_table_query)

RPostgres::dbWriteTable(con, 
                        name = 'annual_climate', 
                        value = climate, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

# 16-Day NPP ------------------------------------- # 
NPP_16 <- read_csv('data/RAP_EE_exports/allotment_16_day_NPP.csv') %>%  
  select(uname, year, doy, afgNPP, pfgNPP) %>% 
  rename( 'afg' = afgNPP, 'pfg' = pfgNPP )

dbRemoveTable(con, 'npp_16')

create_table_query <- 
  str_squish( str_remove_all("
    CREATE TABLE npp_16( 
      uname INT, 
      year INT, 
      doy INT, 
      afg NUMERIC,
      pfg NUMERIC,
      PRIMARY KEY(uname, year, doy), 
      CONSTRAINT fk_uname FOREIGN KEY(uname) REFERENCES allotments(uname));", pattern = '\n'))


RPostgres::dbSendQuery(con, create_table_query)

RPostgres::dbWriteTable(con, 
                        name = 'npp_16', 
                        value = NPP_16, 
                        row.names = F, 
                        overwrite = F, 
                        append = T)

dbDisconnect(con)


