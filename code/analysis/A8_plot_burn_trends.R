rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(lme4)
library(emmeans)
library(merTools)

source('code/analysis/plot_tools.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

load('output/number_burned_allotments_per_blm_office.rda')
load('output/area_burned_per_blm_office.rda')

burns_per_office$n_burns_pred <- predict(n_burns_mer , type = 'response')
burns_per_office$area_pred <- predict( area_burn_mer, type = 'response' )

# ---- plot coefficients ------------------- # 
ecogroup_trends <- get_ecogroup_trends(n_burns_mer)
random_effects <- get_blm_random_effects(n_burns_mer)
trend_summary <- blm_trend_summary(n_burns_mer@frame, ecogroup_trends, random_effects)
trend_table_freq <-ecogroup_trends_as_df(trend_model = n_burns_mer, type = 'burn frequency')

trend_table_freq %>% 
  mutate( ecogroup = factor(ecogroup, labels = str_squish(ecogroup_labels))) %>% 
  plot_ecogroup_trend_coefficients(my_colors = 'black') + 
  guides(color = F) + 
  scale_y_continuous(name = 'Trend Coefficient') + 
  ggtitle("Trends in Burn Frequency 1991 - 2019") + 
  ggsave(filename = 'output/figures/Fig_4a_burn_frequency_trends_by_Ecoregion.png', 
        height = 5, width = 8, units = 'in', dpi = 'print') 

# Plot predicted vs. observed ----------------------# 
burns_per_office %>% 
  ungroup() %>%
  mutate( ecogroup = factor(ecogroup, labels = ecogroup_labels)) %>% 
  group_by( year , ecogroup ) %>% 
  ggplot(aes( x = year , y = n_burns)) + 
  stat_summary( geom = 'bar', fun = 'sum') + 
  facet_wrap( ~ ecogroup, nrow = 2, scales = 'free_y') + 
  theme_bw() + 
  scale_x_continuous(name = "Year", breaks = c(1995, 2005, 2015)) + 
  scale_y_continuous(name = 'Number of Burned Allotments') + 
  ggsave(filename = 'output/figures/burn_frequency_HORIZONTAL_by_Ecoregion.png', 
       height = 5, width = 9, units = 'in', dpi = 'print') 

dat_preds <- dat %>% 
  mutate( year = year2 + 2005) %>%
  group_by( ecogroup, year ) %>% 
  summarise( yhat_bt = sum(n_burns_pred))

dat %>%
  mutate( year = year2 + 2005 ) %>% 
  group_by( year, ecogroup ) %>%
  summarise( n_burns = sum(n_burns) ) %>% 
  ggplot( aes( x = year, y = n_burns, group = ecogroup)) + 
  #geom_line(data = dat_preds, aes(y = yhat_bt), linetype = 2, size = 0.5, alpha = 0.7) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~ecogroup, ncol = 2, scale = 'free_y') + 
  theme_bw() + 
  scale_y_continuous(name = 'Number of Burned Allotments') + 
  scale_x_continuous(name = 'Year') + 
  ggsave( filename = 'output/figures/Fig_4b_number_of_burns_per_year.png',
          dpi = 'print', height = 6, width = 4)

# Burn area -------------------------------------------- # 
ecogroup_trends <- get_ecogroup_trends(area_burn_mer)
random_effects <- get_blm_random_effects(area_burn_mer)
trend_summary <- blm_trend_summary(area_burn_mer@frame, ecogroup_trends, random_effects)
trend_table_area <-ecogroup_trends_as_df(trend_model = area_burn_mer, type = 'burn area')

trend_table_area %>% 
  mutate(ecogroup = factor(ecogroup, labels = str_squish(ecogroup_labels))) %>% 
  plot_ecogroup_trend_coefficients(my_colors = 'black') + 
  ggtitle( 'Trends in Burn Area 1991 - 2019') + 
  guides(color = F) + 
  ggsave(filename = 'output/figures/Fig_5a_burn_area_trends_by_Ecoregion.png', 
         height = 5, width = 8, units = 'in', dpi = 'print') 


trend_table <- trend_table_area %>% bind_rows(trend_table_freq ) 

trend_table %>%
  mutate(ecogroup = factor(ecogroup, labels = ecogroup_labels)) %>% 
  mutate( type = factor(type, labels = c('Burn Area', 'Burn Frequency'))) %>%
  plot_trend_coefficients(my_colors = c("#00BFC4", "#F8766D")) + 
  theme(legend.title = element_blank() ) + 
  ggsave(filename = 'output/figures/Fig_5_burn_trends_HORIZONTAL_by_Ecoregion.png', 
         height = 5, width = 9, units = 'in', dpi = 'print') 
  
  

# Predict burn area per year --------------- # 
dat <- area_burn_mer@frame

dat$yhat <- predict( area_burn_mer )
dat$yhat_bt <- exp(dat$yhat)

dat_preds <- dat %>% 
  mutate( year = year2 + 2005) %>%
  group_by( ecogroup, year ) %>% 
  summarise( yhat_bt = sum(yhat_bt), total_area = sum(exp(`(offset)`)))

dat %>%
  mutate( year = year2 + 2005 ) %>% 
  group_by( year, ecogroup ) %>%
  summarise( area_burned_km2_int = sum(area_burned_km2_int) ) %>% 
  ggplot( aes( x = year, y = area_burned_km2_int)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~ecogroup, ncol = 2, scale = 'free_y') + 
  theme_bw() + 
  scale_y_continuous(name = 'Total Allotment Area Burned') + 
  scale_x_continuous(name = 'Year')  + 
  ggsave( filename = 'output/figures/Fig_5b_burns_area_per_year.png',
          dpi = 'print', height = 6, width = 4)

burns_per_office %>% 
  ungroup() %>%
  mutate( ecogroup = factor(ecogroup, labels = ecogroup_labels)) %>% 
  group_by( year , ecogroup ) %>% 
  ggplot(aes( x = year , y = area_burned_km2 )) + 
  stat_summary( geom = 'bar', fun = 'sum') + 
  facet_wrap( ~ ecogroup, nrow = 2, scales = 'free_y') + 
  theme_bw() + 
  scale_x_continuous(name = "Year", breaks = c(1995, 2005, 2015)) + 
  scale_y_continuous(name = ~Burn~Area~km^2)  + 
  ggsave(filename = 'output/figures/burn_area_HORIZONTAL_by_Ecoregion.png', 
         height = 5, width = 9, units = 'in', dpi = 'print') 

