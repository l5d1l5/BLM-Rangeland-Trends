rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

trend_scales <- read_csv('data/temp/trend_scales.csv')

# ------ 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_model_files, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'))
names( cover_models ) <- types 

cover_att <- lapply( cover_models, function(x) attributes( x@frame$value2) )
cover_year_att <- lapply( cover_models, function(x) attributes( x@frame$year2) )

pred_grid <- lapply( types , function( x ) { 
  m <- cover_models[[x]]
  m@frame %>% 
    mutate( type = x, unit = 'Cover') %>% 
    filter( year2 == max(year2) | year2 == min(year2)) %>% 
    distinct(unit, type, year2, ecogroup, office_label) %>% 
    arrange(ecogroup, office_label, year2) %>% 
    mutate( yhat = predict( m , newdata = . , re.form = ~ (year2 | ecogroup:office_label))) %>% 
    group_by( unit, type, ecogroup, office_label ) %>% 
    summarise( rate = (yhat[which.max(year2)] - yhat[which.min(year2)])/(max(year2) - min(year2)))
})

cover_trends <- do.call(rbind, pred_grid) %>% 
  left_join(trend_scales, by = c('unit', 'type')) %>% 
  mutate( bt_trend = rate*trend_unit ) %>% 
  select(unit, type, ecogroup, office_label, rate, bt_trend ) %>% 
  select( -rate ) %>% 
  unite( c(type, unit), col = 'type') %>% 
  pivot_wider( id_cols = c('office_label', 'ecogroup'), names_from = 'type', values_from = 'bt_trend')


# Production Trends
agb_model_files <- dir(path = 'output', pattern = '.*_agb_trend_model.rds', full.names = T)
agb_models <- lapply(agb_model_files, read_rds)
types  <- c( str_extract( agb_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Total', 'Perennial'))
names( agb_models ) <- types 

agb_att <- lapply( agb_models, function(x) attributes( x@frame$value2) )
agb_year_att <- lapply( agb_models, function(x) attributes( x@frame$year2) )

pred_grid <- lapply( types , function( x ) { 
  m <- agb_models[[x]]
  m@frame %>% 
    mutate( type = x, unit = 'AGB' ) %>% 
    filter( year2 == max(year2) | year2 == min(year2)) %>% 
    distinct(unit, type, year2, ecogroup, office_label) %>% 
    arrange(ecogroup, office_label, year2) %>% 
    mutate( yhat = predict( m , newdata = . , re.form = ~ (year2 | ecogroup:office_label))) %>% 
    group_by( unit, type, ecogroup, office_label ) %>% 
    summarise( rate = (yhat[which.max(year2)] - yhat[which.min(year2)])/(max(year2) - min(year2)))
})

agb_trends <- do.call(rbind, pred_grid) %>% 
  left_join(trend_scales, by = c('unit', 'type')) %>% 
  mutate( bt_trend = rate*trend_unit ) %>% 
  select(unit, type, ecogroup, office_label, rate, bt_trend ) %>% 
  select( -rate ) %>% 
  unite( c(type, unit), col = 'type') %>% 
  pivot_wider( id_cols = c('office_label', 'ecogroup'), names_from = 'type', values_from = 'bt_trend')

office_trends <- cover_trends %>%
  left_join(agb_trends, by = c('office_label', 'ecogroup'))

read_rds('data/temp/allotment_info.rds') %>%
  distinct(admin_st,  parent_name, admu_name) %>% 
  left_join( office_trends, by = c('admu_name' = 'office_label')) %>% 
  select( admin_st, parent_name, admu_name, ecogroup, Annual_Cover:Perennial_AGB) %>%
  arrange( admin_st, parent_name, admu_name, ecogroup )   %>% 
  write_csv('output/tables/field_office_rates.csv')

all_rates <- read_csv('output/tables/field_office_rates.csv')

