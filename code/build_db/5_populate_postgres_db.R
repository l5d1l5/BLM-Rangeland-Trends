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

dbRemoveTable(con, 'allotment_centroids')
dbRemoveTable(con, 'allotment_shapes')
dbRemoveTable(con, 'annual_data')
dbRemoveTable(con, 'allotments')

#  Load annual data exported from Earth Engine 
AGB <- read_csv('data/temp/allotment_biomass_by_year.csv') %>% 
  select( uname, year, contains('agb'))

cover <- read_csv( 'data/temp/allotment_cover_by_year.csv') %>% 
  select( uname, year, AFGC, BG, LTR, PFGC, SHR, TREE)

climate <- read_csv('data/temp/allotment_climate_by_year.csv') %>%
  select( uname, year, pdsi:tavg )

# Join the response data together into one long data frame 
annual_data <- 
  AGB %>%
  left_join(cover, by = c('uname', 'year') ) %>%
  left_join(climate, by = c('uname', 'year')) %>%
  select(uname, year, afgAGB:pfgAGB, AFGC:TREE) %>% 
  pivot_longer(cols = c(afgAGB:TREE), names_to = 'type', values_to = 'value')  %>%
  filter( complete.cases(.))  

# cbind( names(annual_data), 
#        lapply( annual_data, class), 
#        lapply( annual_data, function(x) max(str_length(x))))

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

dbDisconnect(con)
