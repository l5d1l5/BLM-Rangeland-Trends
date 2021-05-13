rm(list = ls())
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

#aum <- read_rds( 'data/aum_all.rds') # where is "aum_all.rds" from? Directly from Chris?

# 
allotment_info <- 
  allotment_info %>% 
  mutate( `Bill Allot Number` = paste0( 'ADMIN_ST', 'ALLOT_NO' ))

grazing <- read_rds('data/aum_all.rds') %>% 
  left_join( 
    allotment_info %>% 
    select( `Bill Allot Number`, uname, ADMIN_ST, ADM_UNIT_CD, ALLOT_NAME, ALLOT_NO), 
  by = "Bill Allot Number") %>% 
  mutate( year = year( mdy(`Bill Begin Date`)))

grazing %>%
  select( uname, year , starts_with('Bill'), ADM_UNIT_CD, ALLOT_NAME, ALLOT_NO) %>%
  arrange( uname, year ) %>% 
  write_rds('data/temp/allotment_grazing_data.rds')

elevation %>% 
  select( uname, elevation) %>%
  mutate( elevation = as.numeric(elevation)) %>% 
  write_rds(file = 'data/temp/elevation.rds')

# Join the response data together into one long data frame 
AGB %>%
  left_join(cover, by = c('uname', 'year') ) %>%
  left_join(climate, by = c('uname', 'year')) %>%
  select(uname, year, afgAGB:pfgAGB, AFGC:TREE) %>% 
  pivot_longer(cols = c(afgAGB:TREE), names_to = 'type', values_to = 'value')  %>%
  filter( complete.cases(.)) %>% 
  write_rds( file = 'data/temp/allotment_data_long.rds')

