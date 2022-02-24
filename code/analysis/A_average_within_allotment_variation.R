# Variance Plots 
rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(gridExtra )
library(ggpubr)
require(kableExtra)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

load('data/analysis_data/allotments.rda')

EE_trends <- read_csv('data/RAP_EE_exports/allotment_cover_trends.csv')

EE_trends %>% 
  select( AFGC_scale_count: uname) %>% 
  select( contains('stdDev'), 'uname') %>% 
  pivot_longer(cols = contains('stdDev'), names_to = 'type', values_to = 'TrendStdDev') %>% 
  mutate( Var = TrendStdDev^2 ) %>% 
  left_join(allotments, by = 'uname') %>% 
  filter( !is.na( ecogroup))  %>%
  group_by( ecogroup, type ) %>% 
  summarise( avgStdDev = sqrt( mean( Var, na.rm = T)) )  %>%
  ggplot( aes( x = ecogroup, y = avgStdDev)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~type) + 
  coord_flip()

