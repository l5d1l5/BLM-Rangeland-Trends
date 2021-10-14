rm(list = ls() )
library(tidyverse)
library(stringr)
library(lme4)
library(emmeans)

pfg_boot <- read_csv('output/julia_Bootstrap_afg_model.txt')

pfg_trend_est <- pfg_boot %>% 
  filter( is.na(group)) %>% 
  mutate( type = ifelse( str_detect(names, 'year2'), 'slope', 'intercept')) %>% 
  filter( type == 'slope') %>%
  mutate( ecogroup = str_extract(names, pattern = "(?<=\\: ).+(?= &)")) %>% 
  mutate( ecogroup = ifelse(is.na(ecogroup), 'AZ/NM Highlands', ecogroup)) %>% 
  select( iter, ecogroup, value ) %>%
  pivot_wider(names_from = ecogroup, values_from = value )  %>% 
  mutate( across( .cols = c(`E Cold Deserts`:`Warm Deserts`), .fns = ~ . + `AZ/NM Highlands`)) %>% 
  pivot_longer(cols = c(`AZ/NM Highlands`:`Warm Deserts`), names_to = 'ecogroup', values_to = "estimate")


ci <- pfg_trend_est %>% 
  group_by( ecogroup ) %>% 
  dplyr::summarise( m1 = mean(estimate ), 
             m2 = median(estimate), 
             lcl = quantile(estimate, 0.025), 
             ucl = quantile(estimate, 0.975))

pfg_trend_est %>%
  ggplot( aes( x = estimate )) + 
  geom_density() + 
  facet_wrap( ~ ecogroup, scales = 'free') + 
  geom_vline( data = ci , aes( xintercept = m2)) + 
  geom_vline( data = ci, aes( xintercept = lcl)) + 
  geom_vline(data = ci, aes( xintercept = ucl)) + 
  geom_vline(data = trends, aes( xintercept = year2.trend), col = 'red') + 
  geom_vline(data = trends, aes( xintercept = c(asymp.LCL)), col = 'red') + 
  geom_vline(data = trends, aes( xintercept = c(asymp.UCL)), col = 'red')


m <- read_rds('output/pfg_agb_trend_model.rds')

trends <- emmeans::emtrends(m, var = 'year2' , specs =  ~ ecogroup )

trends <- trends %>% as.data.frame()

trends
