# AUM 
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

aum_data <- read_rds('data/aum_all.rds')
allotments <- tbl(con, 'allotments') %>% select(uname, admin_st, allot_no, allot_name, admu_name, ecogroup, acres) %>% collect()

allotments <- allotments %>% 
  mutate( `Bill Allot Number` = paste0(admin_st, allot_no))  %>% 
  filter( str_length( `Bill Allot Number`) == 7 ) 

aum_billing_allotments <- 
  allotments %>% 
  filter( allot_name != 'NONE') %>%
  group_by( `Bill Allot Number`, admin_st, allot_name, admu_name, ecogroup) %>% 
  summarise( acres = sum(acres), uname = list(uname))

complete_aum_data <- 
  aum_data %>% 
  filter( !is.na( `Bill Allot Number`)) %>%
  filter(str_length(`Bill Allot Number`) == 7) %>% 
  filter( !is.na(`Bill End Date`), !is.na(`Bill Begin Date`), !is.na(`Bill AUMs`)) %>% 
  mutate( ubill = row_number()) %>% 
  mutate( `Bill Begin Date` = lubridate::mdy(`Bill Begin Date`), `Bill End Date` = lubridate::mdy( `Bill End Date`)) %>% 
  mutate( `Bill Year` = lubridate::year(`Bill Begin Date`))

complete_aum_data %>% 
  left_join(aum_billing_allotments, by = c('Bill Allot Number', 'Bill Admin State' = 'admin_st')) %>% 
  select( `Bill Admin State`, 
          `Bill Office Name`, admu_name, 
          `Bill Allot Name`, allot_name,
          acres, uname, ecogroup, starts_with('Bill')) %>% 
  head() %>%
  view()



aum_data_match <- complete_aum_data %>% 
  left_join(allotments, by = 'Bill Allot Number') %>% 
  group_by(ubill) %>% 
  summarise( n_uname = n_distinct(uname), unames = list(unique(uname))) %>% 
  mutate( n_uname = ifelse( is.na(unames), 0, n_uname))

aum_data_match %>% View 

