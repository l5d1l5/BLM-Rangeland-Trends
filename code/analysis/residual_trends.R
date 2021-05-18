rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)

source('code/analysis/plot_tools.R')

# LMER options 
control = lmerControl(optimizer = "optimx", 
                      calc.derivs = FALSE,
                      optCtrl = list(method = "nlminb", 
                                     starttests = FALSE, 
                                     kkt = FALSE))

# Basic analysis formula for finding long-term annual trend in the data 
raw_resid_form <- formula( resid ~ year2*ecogroup + (year2|uname) + (year2|office_label) + (year2|district_label) )
abs_resid_form <- formula( abs_resid ~ year2*ecogroup + (year2|uname) + (year2|office_label) + (year2|district_label) )


con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)


allotments <- tbl(con, 'allotments') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, acres, elevation) %>% 
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) 

afg_m <- read_rds('output/afg_npp_trend_model.rds')

afg_data <- afg_m@frame 

afg_data$resid <- resid(afg_m)

afg_data<- afg_data %>% 
  mutate( year = back_transform(year2, attributes(year2), log = F)) %>%
  mutate(value = back_transform(value2, attributes_obj = attributes(value2))) %>%
  mutate( abs_resid = abs(resid)) %>% 
  mutate( decade = decade_function(year) ) %>% 
  mutate( decade = factor( decade, levels = c('90s', '00s', '10s'), ordered = T)) 
  

afg_data <- 
  afg_data %>% 
  select( decade, uname, value, value2, year2, year, resid, abs_resid ) %>% 
  left_join(allotments %>% collect() , by  = 'uname') 

afg_decadal_var <- 
  afg_data %>% 
  group_by( decade, uname,  ecogroup, district_label, office_label) %>% 
  summarise( sd_resid = sd( resid ), sd_value = sd(value) , sd_value2 = sd(value2))

afg_decadal_var %>% 
  ggplot( aes( x = decade, y = sd_resid )) + 
  geom_point() + 
  geom_boxplot() + 
  facet_wrap( ~ ecogroup )

afg_decadal_var %>% 
  ggplot( aes( x = decade, y = sd_value )) + 
  geom_point() + 
  geom_boxplot() + 
  facet_wrap( ~ ecogroup )

afg_decadal_var %>% 
  ggplot( aes( x = decade, y = sd_value2 )) + 
  geom_point() + 
  geom_boxplot() + 
  facet_wrap( ~ ecogroup )

m_raw_resid_trend <- lmer( data = afg_data, 
                           formula = raw_resid_form, 
                           control = control )

emmeans::emtrends(m_raw_resid_trend, specs = ~ ecogroup, var = 'year2' )

m_abs_resid_trend <- lmer( data = afg_data, 
                           formula = abs_resid_form, 
                           control = control )

emmeans::emtrends( m_abs_resid_trend , specs = ~ ecogroup, var = 'year2')

# 
pfg_m <- read_rds('output/pfg_npp_trend_model.rds')

pfg_data <- pfg_m@frame 

pfg_data$resid <- resid(pfg_m)

pfg_data<- pfg_data %>% 
  mutate( year = back_transform(year2, attributes(year2), log = F)) %>%
  mutate(value = back_transform(value2, attributes_obj = attributes(value2))) %>%
  mutate( abs_resid = abs(resid)) %>% 
  mutate( decade = decade_function(year) ) %>% 
  mutate( decade = factor( decade, levels = c('90s', '00s', '10s'), ordered = T)) 

pfg_data <- 
  pfg_data %>% 
  select( decade, uname, value, value2, year2, year, resid, abs_resid ) %>% 
  left_join(allotments %>% collect() , by  = 'uname') 

pfg_decadal_var <- 
  pfg_data %>% 
  group_by( decade, uname,  ecogroup, district_label, office_label) %>% 
  summarise( sd_resid = sd( resid ), sd_value = sd(value) , sd_value2 = sd(value2))

pfg_decadal_var %>% 
  ggplot( aes( x = decade, y = sd_resid )) + 
  geom_point() + 
  geom_boxplot() + 
  facet_wrap( ~ ecogroup )

pfg_decadal_var %>% 
  ggplot( aes( x = decade, y = sd_value )) + 
  geom_point() + 
  geom_boxplot() + 
  facet_wrap( ~ ecogroup )

pfg_decadal_var %>% 
  ggplot( aes( x = decade, y = sd_value2 )) + 
  geom_point() + 
  geom_boxplot() + 
  facet_wrap( ~ ecogroup )

m_raw_resid_trend <- lmer( data = pfg_data, 
                           formula = raw_resid_form, 
                           control = control )

emmeans::emtrends(m_raw_resid_trend, specs = ~ ecogroup, var = 'year2' )

m_abs_resid_trend <- lmer( data = pfg_data, 
                           formula = abs_resid_form, 
                           control = control )

emmeans::emtrends( m_abs_resid_trend , specs = ~ ecogroup, var = 'year2')


