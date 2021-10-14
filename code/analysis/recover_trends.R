
rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)

source('code/analysis/functions.R')


# cover trends:  ------------------------------------------ # 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_model_files, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Total', 'Perennial', 'Shrub', 'Tree'))
names( cover_models ) <- types 

pred_grid <- cover_models$Annual@frame
pred_grid$y_hat <- predict(cover_models$Annual)

pred_grid %>% 
  group_by( ecogroup, uname ) %>% 
  select( - office_label ) %>% 
  filter( year2 == min(year2) | year2 == max(year2)) %>%
  mutate( time = factor( year2 ,labels = c('start', 'end')) ) %>% 
  select(-year2) %>%
  ungroup() %>% 
  pivot_wider(id_cols = c(ecogroup, uname), 
              names_from = time, 
              values_from = value2 ) %>% 
  mutate( center = attributes( pred_grid$value2 )$`scaled:center`, 
          scale = attributes( pred_grid$value2 )$`scaled:scale`) %>%
  mutate( rate  = ((end*scale + center) - (start*scale + center)) /30 ) %>% 
  group_by( ecogroup ) %>% 
  summarise( mrate = mean(rate), lower = quantile(rate, 0.025 ), upper = quantile( rate, 0.975) )

attributes( pred_grid$value2 )

m1 <- read_rds('output/AFG_agb_trend_model.rds')

model_trends <- get_ecogroup_trends(m1)  

raw_trends <- trends*attributes(pred_grid$value2)$`scaled:scale`/attributes( pred_grid$year2 )$`scaled:scale`

model_trends
raw_trends
