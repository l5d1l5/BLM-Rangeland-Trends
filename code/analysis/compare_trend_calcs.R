rm(list = ls())
library(tidyverse)
library(dbplyr)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

new_allotments <- tbl(con, 'allotments') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, acres, area, climate_region) %>% 
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) %>% 
  filter( admin_st == 'OR') %>% 
  collect() 

#load('data/temp/cover.rda')
lin_trends1 <- read_csv('~/Downloads/afgc_linear_trends.csv')
lin_trends2 <- read_csv('~/Downloads/afgc_linear_trends (1).csv')

new_allotments <- 
  new_allotments %>% 
  left_join(lin_trends1) %>%
  left_join(lin_trends2, by = 'uname')

new_allotments %>% head

new_allotments %>%
  ggplot( aes( x = scale.x, y = scale.y )) + geom_point() 

afg_cover_model <- read_rds('output/AFG_cover_trend_model.rds')

#load('app/data/trenddata.rda')
load('app/data/mapdata.rda')
load('app/data/trenddata.rda')
afg_trends <- read_csv('output/AFG_cover_group_trends.csv')

old_value_scale <- attributes(afg_cover_model@frame$value2)[[3]]
old_year_scale <- attributes(afg_cover_model@frame$year2)[[3]]

allotments <- allotment_ctrs %>% 
  filter(State == 'OR')  %>% 
  st_drop_geometry()  %>% 
  select( uname, Name, parent_cd, acres ) 

allotments %>% 
  left_join(afg_trends, by = 'uname') %>% 
  select( uname, Name, acres, parent_cd,  full_trend) %>% 
  left_join( (all_trends %>% select( Annual_cover, uname ) )) %>%
  left_join( 
    new_allotments %>% 
      select( uname, parent_cd , acres, allot_name, offset.y, scale.y, scale.x ) %>% 
      rename( uname2 = uname, Name = allot_name )
  ) %>% 
  mutate( back_transformed_trend = full_trend*old_value_scale[1]/old_year_scale[1]) %>% 
  ggplot( aes( x = back_transformed_trend, y = scale.x )) + 
  geom_point() + 
  geom_point( aes( y = scale.y), color = 'red')  + 
  geom_abline(aes(intercept = 0, slope = 1))

