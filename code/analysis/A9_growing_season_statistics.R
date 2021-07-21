rm(list = ls())
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

# Constants
lbs_per_acre_min <- 5 
sample_per_year_min <- 22 
years_of_data_min <- 29

allotments <- tbl( con, 'allotments')
npp_16 <- tbl(con, 'npp_16')

temp <- npp_16 %>% 
  select( uname, year, doy, afg, pfg) %>% 
  mutate( herb_npp = afg + pfg ) %>% 
  filter( !is.na(herb_npp))  %>% 
  group_by( uname, year ) %>% 
  mutate( npp_total = sum(herb_npp, na.rm = T)) %>%  # total for the year 
  filter( npp_total > lbs_per_acre_min, n() >= sample_per_year_min) %>%  # Greater than 5 lbs per acre and greater than 22 sample each year
  ungroup() 

temp <- temp %>% 
  filter( year > 1990 )

# Find allotments with at least 29 years of data 
year_counts <- temp %>%
  group_by( uname) %>%
  summarise( nyears = n_distinct(year))

# Filter by number of years per allotment 
temp <- year_counts %>%
  filter( nyears >= years_of_data_min) %>% 
  left_join( temp, by = 'uname')
  
# Order and calculate cumulative herbaceous production 
temp <- temp %>% 
  group_by( uname, year ) %>% 
  window_order( uname, year, doy ) %>% 
  mutate(herb_npp_cmsm = cumsum( herb_npp ) ) %>% 
  ungroup() 


critical_stats <- temp %>% 
  select( uname, year ) %>%
  ungroup() %>% 
  distinct() %>%
  mutate( crit_50 = 0.5, crit_25 = 0.25, crit_75 = 0.75 ) %>% 
  pivot_longer(starts_with('crit'), names_to = 'crit', values_to = 'crit_val') %>% 
  select( uname, year, crit_val) %>% 
  left_join( temp, by = c('uname', 'year')) %>%
  mutate( herb_npp_crit = npp_total*crit_val) %>% 
  group_by( uname, year , crit_val ) %>% 
  mutate( above = herb_npp_cmsm > herb_npp_crit ) %>% 
  group_by( uname, year, crit_val, above ) %>% 
  filter( (above & doy == min(doy, na.rm = T)) |
            (!above & doy == max(doy, na.rm = T)) ) %>% 
  mutate( above = ifelse(above, 'hi', 'lo' )) %>% 
  pivot_longer( c(doy, herb_npp_cmsm), names_to = 'type', values_to = 'value') %>% 
  mutate( type = paste( type, above, sep = '_')) %>%
  ungroup() %>%
  select( uname, year, crit_val, herb_npp_crit, type, value ) %>%
  pivot_wider(names_from = type, values_from = value ) %>%
  mutate( m = (herb_npp_cmsm_hi - herb_npp_cmsm_lo)/(doy_hi - doy_lo)) %>% 
  mutate( B = herb_npp_cmsm_hi - m*doy_hi) %>% 
  mutate( doy_crit = (herb_npp_crit - B)/m ) 
  

critical_stats %>% 
  collect() %>% 
  write_rds(file = 'data/temp/allotment_growing_season.rds')

