rm(list = ls())
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )
library(sf)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

npp_16 <- tbl(con, 'npp_16_day')

peak_production_doy <- 
  npp_16 %>% 
  select( uname, year, doy , afg, pfg) %>% 
  pivot_longer(c(afg, pfg), names_to = 'type', values_to = 'value') %>% 
  group_by( uname, year, type ) %>%
  filter(  value == max(value)) %>% 
  arrange( type, uname, year ) 

#production_window <- 
growing_days <- 
  npp_16 %>% 
  select( uname, year, doy , afg, pfg) %>% 
  pivot_longer(c(afg, pfg), names_to = 'type', values_to = 'value') %>% 
  group_by( uname, year, type ) %>%
  filter( !is.na(value)) %>%
  summarise( prod_days = count( value > 0 )) %>%
  ungroup() 

growing_days %>% ungroup() %>% summarise( max(prod_days, na.rm = T))

allotments <- tbl(con, 'allotments')

peak_production_doy %>% 
  filter( value > 4 ) %>%
  ungroup() %>% 
  ggplot( aes( x = year, y = doy, group = uname) ) + 
  geom_point() + 
  stat_smooth(geom = 'line', method = 'lm', size = 0.5, alpha = 0.2, color = 'blue') + 
  facet_wrap( ~ type )

peak_production_doy %>% 
  left_join(allotments, by = 'uname') %>%
  ggplot( aes( x = year, y = doy , group = uname)) + 
  geom_point() + 
  stat_smooth(aes( group = uname), 
              geom = 'line', 
              method = 'lm', 
              alpha = 0.1, color = 'blue') + 
  facet_grid( type ~ admu_name) 


growing_days %>%
  ungroup() %>%
  mutate( pdays = as.numeric(prod_days)) %>%
  left_join(allotments, by = 'uname') %>% 
  ggplot( aes( x = year, y = pdays)) + 
  geom_point(aes(group = uname)) + 
  facet_grid( type  ~ admu_name )

npp_16 %>% 
  pivot_longer(c(afg, pfg), names_to = 'type', values_to = 'value') %>% 
  left_join(allotments, by = 'uname') %>% 
  #mutate( year_a = as.character( (year ) )) %>% 
  ggplot( aes( x = doy, y = value , color = year )) + 
  geom_path(aes( group = uname ), alpha = 0.1) + 
  facet_grid( type ~ admu_name ) 
