rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)

source('code/analysis/plot_tools.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

allotments <- 
  tbl(con, 'allotments') %>% 
  select(uname, 
         allot_name,  
         admu_name,
         admin_st, 
         parent_cd, 
         parent_name, ecogroup, acres, elevation)

# Cover trends: 

cover <- tbl(con, 'annual_data') %>%  
  filter( type %in% c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR')) %>% 
  left_join(allotments, by = 'uname')


top_allots <- 
  cover %>% 
  filter( admin_st == 'MT'  & 
            admu_name =='Miles City Field Office') %>% 
  distinct(admu_name, allot_name, uname, acres ) %>% 
  arrange( desc(acres )) 

example_ts <- 
  cover %>% 
  filter( uname %in% c(7094, 8384, 7179)) %>% 
  collect() %>%
  as.data.frame() %>% 
  mutate( type_label = factor(type, labels = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree')))

example_ts %>% 
  mutate( type = type_label) %>% 
  plot_single_allotment_trends(my_colors = my_colors) +
  scale_y_continuous(name = 'Cover (%)') + 
  facet_wrap(~ allot_name, ncol = 1 ) + 
  ggtitle(label = 'Example Allotments, Miles City, MT')

allots <- read_sf(con, 'allotment_shapes') %>%
  filter( uname %in% c(7094, 8384, 7179)) 

allots %>% 
  left_join(example_ts %>% distinct(uname, allot_name), by = 'uname') %>% 
  ggplot() + 
  geom_sf() + 
  theme_bw()


# Production 
prod <- tbl(con, 'annual_data') %>%  
  filter( type %in% c('afgAGB', 'pfgAGB')) %>% 
  left_join(allotments, by = 'uname')

example_ts <- 
  prod %>% 
  filter( uname %in% c(7094, 8384, 7179)) %>% 
  collect() %>%
  as.data.frame() %>%
  mutate( type = factor(type, labels =c( 'Annual', 'Perennial')))


example_ts %>% 
  plot_single_allotment_trends(my_colors = my_colors) +
  #geom_smooth(data = example_ts %>% filter( year >= 2000), aes( group = type ), 
  #            method = 'lm', se = F, lty = 1, size = 0.5) + 
  scale_y_continuous(name = 'Production (lbs per acre)') + 
  facet_wrap(~ allot_name, ncol = 1 ) + 
  ggtitle(label = 'Example Allotments, Miles City, MT') + 
  ggsave(filename = 'output/figures/fig5a_AGB_example_production_trends_MilesCity.pdf', 
         height = 7, width = 10, units = 'in') 

example_ts %>% 
  plot_single_allotment_trends(my_colors = my_colors) +
  geom_smooth(data = example_ts %>% filter( year >= 2000), aes( group = type ), 
              method = 'lm', se = F, lty = 1, size = 0.5) + 
  scale_y_continuous(name = 'Production (lbs per acre)') + 
  facet_wrap(~ allot_name, ncol = 1 ) + 
  ggtitle(label = 'Example Allotments, Miles City, MT') + 
  ggsave(filename = 'output/figures/fig5b_AGB_example_production_trends_with_trendlines_MilesCity.pdf', 
         height = 7, width = 10, units = 'in') 


# 
# Biomass Trends
npp_model_files <- dir(path = 'output', pattern = '.*_npp_trend_model.rds', full.names = T)
npp_models <- lapply(npp_model_files, read_rds)
types  <- c( str_extract( npp_model_files, pattern = '[a-z]fg') )
types <- factor(types, labels = c('Annual', 'Perennial'))
trend_table <- mapply( x = npp_models, y = types, FUN = function(x,y) ecogroup_trends_as_df(x, y), SIMPLIFY = F)
trend_table <- do.call(rbind, trend_table)

trend_table %>% 
  plot_ecogroup_trend_coefficients(my_colors = my_colors) + 
  ggtitle( 'Linear Trends in Allotment NPP 1991 - 2019') +
  ggsave(filename = 'output/figures/fig6a_NPP_trend_coefficients_by_Ecoregion.pdf', 
         height = 7, width = 10, units = 'in') 

# cover trends: 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_model_files, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'))
trend_table <- mapply( x = cover_models, y = types, FUN = function(x,y) ecogroup_trends_as_df(x, y), SIMPLIFY = F)

trend_table <- do.call(rbind, trend_table)

nonWoody_trend_coefficient_plot <- 
  plot_ecogroup_trend_coefficients(trend_table %>% 
                                     filter( type %in% c('Annual', 'Perennial', 'Bare')), 
                                   my_colors = my_colors)

woody_trend_coefficient_plot <- 
  plot_ecogroup_trend_coefficients(trend_table %>% 
                                     filter( type %in% c('Shrub', 'Tree')), 
                                   my_colors = my_colors)



nonWoody_trend_coefficient_plot +
  ggtitle( 'Trends in Allotment Cover 1991 - 2019') +  
  ggsave(filename = 'output/figures/fig6b_nonWoody_cover_trend_coefficients_by_Ecoregion.pdf', 
          height = 7, width = 10, units = 'in') 

woody_trend_coefficient_plot +
  ggtitle( 'Trends in Allotment Cover 1991 - 2019') + 
  ggsave(filename = 'output/figures/fig6c_woody_cover_trend_coefficients_by_Ecoregion.pdf', 
         height = 7, width = 10, units = 'in') 

