# export data for Chris: 
rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

dir.create('output/export_data')

tbl( con, 'allotments') %>% 
  data.frame() %>% 
  write_csv(file = 'output/export_data/allotment_table.csv')

tbl( con, 'annual_data') %>% 
  data.frame() %>% 
  write_csv(file = 'output/export_data/annual_veg_data.csv')

tbl( con, 'annual_climate') %>% 
  data.frame() %>% 
  write_csv(file = 'output/export_data/annual_climate_data.csv')

file.copy('data/temp/BLM_allotments_sf.rds', 'output/export_data')

file.copy('data/temp/simplified_ecogroup_shapefile.rds', 
          'output/export_data')

file.copy('data/temp/BLM_allotments_cleaned', 
          'output/export_data', overwrite = T, recursive = T )
