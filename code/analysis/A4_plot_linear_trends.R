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
         parent_name, ecogroup, acres)

# Cover data ---------------- #
cover <- tbl(con, 'annual_data') %>%  
  filter( type %in% c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR')) %>% 
  left_join(allotments, by = 'uname')

# AGB data ------------------- #  
prod <- tbl(con, 'annual_data') %>%  
  filter( type %in% c('afgAGB', 'pfgAGB')) %>% 
  left_join(allotments, by = 'uname')


# Biomass Trends
agb_model_files <- dir(path = 'output', pattern = '.*_agb_trend_model.rds', full.names = T)
agb_models <- lapply(agb_model_files, read_rds)
types  <- c( str_extract( agb_model_files, pattern = '[a-z]fg') )
types <- factor(types, labels = c('Annual', 'Perennial'))
trend_table <- mapply( x = agb_models, y = types, FUN = function(x,y) ecogroup_trends_as_df(x, y), SIMPLIFY = F)
trend_table <- do.call(rbind, trend_table)

trend_table %>% 
  plot_ecogroup_trend_coefficients(my_colors = my_colors) + 
  ggtitle( 'Trends in Allotment Biomass 1991 - 2020') +
  ggsave(filename = 'output/figures/Fig_2_AGB_trend_coefficients_by_Ecoregion.png', 
         height = 5, width = 8, units = 'in', dpi = 'print') 

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
  ggtitle( 'Trends in Allotment Cover 1991 - 2020') +  
  ggsave(filename = 'output/figures/Fig_3a_nonWoody_cover_trend_coefficients_by_Ecoregion.png', 
         height = 5, width = 8, units = 'in', dpi = 'print') 

woody_trend_coefficient_plot +
  ggtitle( 'Trends in Allotment Cover 1991 - 2019') + 
  ggsave(filename = 'output/figures/Fig_3b_woody_cover_trend_coefficients_by_Ecoregion.png', 
         height = 5, width = 8, units = 'in', dpi = 'print') 


# Variance partitioning -------------------------------- 
vc <- lapply(cover_models, VarCorr)
vc <- lapply( vc, data.frame )
vc <- mapply( x = types, y = vc, function(x, y) {y$type <- x; return(y)}, SIMPLIFY = F)

cover_models[[1]] %>% 
  VarCorr %>% 
  data.frame()

AFGC <- cover_models[[1]]@frame

vc %>% bind_rows() %>% 
  filter( var1 == 'year2') %>% group_by( type) %>% 
  mutate( pvcov = vcov/sum(vcov)) %>% 
  ggplot( aes( x = type, y = pvcov,fill = grp)) + 
  geom_bar(stat = 'identity')  

agb_types  <- c( str_extract( agb_model_files, pattern = '[a-z]fg') )

vc_agb <- lapply( agb_models, VarCorr) 
vc_agb <- lapply( vc_agb, data.frame)
vc_agb <- mapply( x = agb_types, y = vc_agb, function(x,y){y$type <- x; return(y)}, SIMPLIFY = F)

trend_variance <- vc_agb %>% 
  bind_rows() %>% 
  filter( var1 == 'year2') %>% 
  mutate( type = factor(type, labels = c('Annual AGB', 'Perennial AGB'))) %>%
  bind_rows( vc %>% bind_rows() %>% filter( var1 == 'year2')) %>% 
  group_by( type ) %>%
  mutate(prop_var = vcov/sum(vcov)) %>% 
  ungroup() 

trend_variance$Group <- factor( trend_variance$grp, labels = c('District', 'Field Office', 'Allotment') )
trend_variance$type_labels <- factor( trend_variance$type, labels = c('Annual AGB', 'Perennial AGB', 'Annual Cover', 'Bare Cover', 'Perennial Cover', 'Shrub Cover', 'Tree Cover'))
trend_variance$type_labels <- factor( trend_variance$type_labels, levels = c('Annual AGB', 'Annual Cover', 'Perennial AGB', 'Perennial Cover', 'Shrub Cover', 'Tree Cover', 'Bare Cover'), ordered = T)

trend_variance %>% 
  ggplot( aes( x = type_labels, y = prop_var, fill = Group )) + 
  geom_bar(stat = 'identity') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60,hjust = 1), axis.title.x = element_blank()) + 
  ylab( 'Proportion of variance in trend coefficient') +
  ggsave(filename = 'output/figures/fig9_trend_variance.png', 
         height = 5, width = 8, units = 'in', dpi = 'print') 


# Example single allotment timeseries ---------

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

example_ts <- 
  prod %>% 
  filter( uname %in% c(7094, 8384, 7179)) %>% 
  collect() %>%
  as.data.frame() %>%
  mutate( type = factor(type, labels =c( 'Annual', 'Perennial')))


# example_ts %>% 
#   plot_single_allotment_trends(my_colors = my_colors) +
#   #geom_smooth(data = example_ts %>% filter( year >= 2000), aes( group = type ), 
#   #            method = 'lm', se = F, lty = 1, size = 0.5) + 
#   scale_y_continuous(name = 'Production (lbs per acre)') + 
#   facet_wrap(~ allot_name, ncol = 1 ) + 
#   ggtitle(label = 'Example Allotments, Miles City, MT') + 
#   ggsave(filename = 'output/figures/fig5a_AGB_example_production_trends_MilesCity.pdf', 
#          height = 7, width = 10, units = 'in') 
# 
# example_ts %>% 
#   plot_single_allotment_trends(my_colors = my_colors) +
#   geom_smooth(data = example_ts %>% filter( year >= 2000), aes( group = type ), 
#               method = 'lm', se = F, lty = 1, size = 0.5) + 
#   scale_y_continuous(name = 'Production (lbs per acre)') + 
#   facet_wrap(~ allot_name, ncol = 1 ) + 
#   ggtitle(label = 'Example Allotments, Miles City, MT') + 
#   ggsave(filename = 'output/figures/fig5b_AGB_example_production_trends_with_trendlines_MilesCity.pdf', 
#          height = 7, width = 10, units = 'in') 
# 