rm(list = ls() )
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn )
options( na.action = na.fail )

source('code/analysis/plot_tools.R')
load('output/cover.rda')
attach(cover)

m_afgc <- read_rds('output/AFG_cover_trend_model.rds')

ecogroup_effects <- get_ecogroup_trends( m_afgc) 
random_effects <- get_blm_random_effects( m_afgc)
AFGC_trends <- blm_trend_summary( AFGC , ecogroup_effects, random_effects) 

m_afgc_wts <- lmer( basic_form, data = AFGC, weights = area2)
cbind( ecogroup_effects, ecogroup_effects_wts )

summary( m_afgc) 
afgc_trends <- emtrends(m_afgc,  ~ ecogroup , var = "year2")
afgc_trends_wts <- emtrends(m_afgc_wts,  ~ ecogroup , var = "year2")

afgc_trends %>% data.frame() %>% mutate( type = 'original') %>%
  rbind( afgc_trends_wts %>% data.frame() %>% mutate( type = 'weighted')) %>% 
  ggplot( aes( x = ecogroup , y = year2.trend, 
               ymin = asymp.LCL, ymax = asymp.UCL, color = type)) + 
  geom_point() + 
  geom_errorbar()

summary( m_afgc_wts)
m_afgc_wts
