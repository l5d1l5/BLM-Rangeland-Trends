rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)
library(merTools)

source('code/analysis/plot_tools.R')

n_bootsims <- 2 # number of simulations for bootstrapping 

# GLMER options 
control = glmerControl(optimizer = "optimx", 
                       calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", 
                                      starttests = FALSE, 
                                      kkt = FALSE, 
                                      maxit = 10000))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

# Boot strapped predictions first for both frequency and area: 
# ------- Burn frequency --------------------------------- # 
load('output/number_burned_allotments_per_blm_office.rda')
pred_frame <- n_burns_mer@frame %>% 
  mutate( year = year2 + 2005 ) %>% 
  mutate( area_km2 = exp(`(offset)`)) %>%
  group_by( year2, year, ecogroup ) %>%
  summarise(n_burns =  sum(n_burns), area_km2 = sum(area_km2)) %>% 
  arrange(desc(year)) 

pred_frame$log_area_km2 <- log( pred_frame$area_km2 )
pred_frame$y_hat <- exp( pred_frame$log_area_km2 + predict( n_burns_mer, newdata = pred_frame, re.form = NA, type = 'link') )

pred_num_fires <- bootMer(n_burns_mer, FUN = function( x ) { predict(x, newdata = pred_frame, re.form = NA)}, 
                           nsim = n_bootsims, 
                           re.form = NA, 
                           #.progress = 'txt', 
                           ncpus = 4,
                           parallel = 'multicore')

save(pred_num_fires, file = 'output/num_fires_pred_boot.rda')

# -------Burn Area -------------------------------------- # 
load('output/area_burned_per_blm_office.rda')

pred_frame_area <- area_burn_mer@frame %>% 
  mutate( year = year2 + 2005 ) %>% 
  mutate( area_km2 = exp(`(offset)`)) %>%
  group_by( year2, year, ecogroup ) %>%
  summarise(area_burned_km2_int = sum(area_burned_km2_int), area_km2 = sum(area_km2)) %>% 
  arrange(desc(year)) 

pred_frame_area$log_area_km2 <- log( pred_frame_area$area_km2 )
pred_frame_area$y_hat <- exp( pred_frame_area$log_area_km2 + predict( area_burn_mer, newdata = pred_frame_area, re.form = NA, type = 'link') )
pred_area_burned <- bootMer(area_burn_mer, FUN = function( x ) { predict(x, newdata = pred_frame_area, re.form = NA)}, 
                          nsim = n_bootsims, 
                          re.form = NA, 
                          #.progress = 'txt', 
                          ncpus = 4,
                          parallel = 'multicore')

save(pred_area_burned, file = 'output/area_burned_pred_boot.rda')

# ---- plot coefficients ------------------- # 
ecogroup_trends <- get_ecogroup_trends(n_burns_mer)
random_effects <- get_blm_random_effects(n_burns_mer)
trend_summary <- blm_trend_summary(n_burns_mer@frame, ecogroup_trends, random_effects)
trend_table <-ecogroup_trends_as_df(trend_model = n_burns_mer, type = 'burns')

trend_table %>% 
  plot_ecogroup_trend_coefficients(my_colors = 'black') + 
  guides(color = F) + 
  ggtitle( 'Trends in Burn Frequency 1991 - 2019') +
  ggsave(filename = 'output/figures/Fig_4a_burn_frequency_trends_by_Ecoregion.png', 
         height = 5, width = 8, units = 'in', dpi = 'print') 

# Plot predicted vs. observed ----------------------# 
load('output/num_fires_pred_boot.rda')

yhats <- apply(pred_num_fires$t, MARGIN = 2, function(x) quantile(x, c(0.025, 0.5, 0.975))) %>% 
  t() %>% as.data.frame() 

pred_frame <- 
  yhats %>% 
  bind_cols(pred_frame) %>% 
  mutate(yhat = exp( log_area_km2 + `50%`), 
         ymin = exp( log_area_km2 + `2.5%`), 
         ymax = exp( log_area_km2 + `97.5%`) )

pred_frame %>%
  ggplot( aes( x = year, y = yhat, group = ecogroup)) + 
  geom_line() + 
  geom_bar( data = pred_frame, aes( y = n_burns), stat = 'identity') + 
  geom_ribbon(aes(ymin =  ymin, ymax = ymax), alpha = 0.4) + 
  facet_wrap(~ecogroup, ncol = 2) + 
  ggsave( filename = 'output/figures/Fig_4b_num_burns_per_ecogroup.png',
          dpi = 'print', height = 6, width = 4)

# Burn area -------------------------------------------- # 
load('output/area_burned_boot_pred.rda')

ecogroup_trends <- get_ecogroup_trends(area_burn_mer)
random_effects <- get_blm_random_effects(area_burn_mer)
trend_summary <- blm_trend_summary(area_burn_mer@frame, ecogroup_trends, random_effects)
trend_table <-ecogroup_trends_as_df(trend_model = area_burn_mer, type = 'burns')

trend_table %>% 
  plot_ecogroup_trend_coefficients(my_colors = 'black') + 
  ggtitle( 'Trends in Burn Area 1991 - 2019') + 
  guides(color = F) + 
  ggsave(filename = 'output/figures/Fig_5a_burn_area_trends_by_Ecoregion.png', 
         height = 5, width = 8, units = 'in', dpi = 'print') 

# Predict burn area per year --------------- # 
pred_frame2 <- area_burn_mer@frame %>% 
  mutate( year = year2 + 2005 ) %>% 
  mutate( area_km2 = exp(`(offset)`)) %>%
  group_by( year2, year, ecogroup ) %>%
  summarise(area_burned_km2_int = sum(area_burned_km2_int), 
            area_km2 = sum(area_km2)) %>% 
  arrange(desc(year)) %>% 
  ungroup() 

pred_frame2$log_area_km2 <- log( pred_frame2$area_km2 )
pred_frame2$mu_hat <- predict( area_burn_mer, 
                     newdata = pred_frame2, 
                     re.form = NA, 
                     type = 'link') 

pred_frame2 <- 
  pred_frame2 %>% 
  mutate( y_hat = exp(  log( area_km2 ) + mu_hat )) %>% 
  dplyr::select(year, ecogroup, area_burned_km2_int, y_hat) 


yhats <- apply(pred_area_burned$t, MARGIN = 2, function(x) quantile(x, c(0.025, 0.5, 0.975))) %>% 
  t() %>% as.data.frame() 

pred_frame_area <- 
  yhats %>% 
  bind_cols(pred_frame_area) %>% 
  mutate(yhat = exp( log_area_km2 + `50%`), 
         ymin = exp( log_area_km2 + `2.5%`), 
         ymax = exp( log_area_km2 + `97.5%`) )

pred_frame_area %>%
  ggplot( aes( x = year, y = yhat, group = ecogroup)) + 
  geom_line() + 
  geom_bar( data = pred_frame_area, aes( y = area_burned_km2_int), stat = 'identity') + 
  geom_ribbon(aes(ymin =  ymin, ymax = ymax), alpha = 0.4) + 
  facet_wrap(~ecogroup, ncol = 2) +
  ggsave(filename = 'output/figures/Fig_5b_burn_area_by_Ecoregion.png', 
       height = 5, width = 8, units = 'in', dpi = 'print') 

