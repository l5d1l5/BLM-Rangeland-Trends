rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)

source('code/analysis/plot_tools.R')

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

burn_data <- tbl(con, 'annual_data') %>% 
  distinct(uname, year) %>% 
  filter( year  >1990, year < 2020) %>%
  left_join(tbl(con, 'allotments'), by = 'uname') %>% 
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                        pattern = c(' Field.*$'))) %>%
  filter( ecogroup != 'Coastal Forests') %>% 
  dplyr::select( year, uname, ecogroup, area, district_label, office_label) %>% 
  left_join( tbl(con, 'annual_burns'), by = c('year', 'uname')) %>% 
  mutate( area_burned = ifelse( is.na(area_burned), 0, area_burned)) %>% 
  mutate( num_fires = ifelse( is.na(num_fires), 0, num_fires)) %>% 
  mutate( year2 = year - 2005 )  %>% 
  mutate( burned_binary = num_fires  > 0 ) %>%
  mutate( area_km2 = area/1e6) %>% 
  mutate( area_burned_km2 = area_burned/1e6 ) %>%
  mutate( log_area_km2 = log(area_km2)) %>% 
  ungroup() 

# office-level analyses: ------------------------------
burns_per_office <- 
  burn_data %>%
  ungroup() %>% 
  collect() %>% 
  mutate( burned_binary = as.integer(burned_binary)) %>% 
  group_by( year, 
            year2, 
            office_label, 
            district_label, 
            ecogroup ) %>% 
  summarise( n_burns = sum( burned_binary, na.rm = T ) , 
             n_allots = n(), 
             area_km2 = sum(area_km2), 
             area_burned_km2 = sum(area_burned_km2)) %>%
  mutate( log_area_km2 = log(area_km2) ) %>% 
  mutate( area_burned_km2_int = as.integer(round(area_burned_km2 )))  

# formulae 
num_burns_form <- formula( n_burns ~ year2*ecogroup )
num_burns_form_mer_full <- 
  formula( n_burns ~ year2*ecogroup + (year2|office_label) + (year2|district_label)) 
num_burns_form_mer_simple <- 
  formula( n_burns ~ year2*ecogroup + (1|office_label) + (year2|district_label)) 
burns_per_office_sample <- burns_per_office %>% ungroup() %>% filter(row_number() <= 2000 )

any(is.na(burns_per_office$n_burns))
any(is.infinite(burns_per_office$n_burns))

n_burns_poisson <- 
  glm( num_burns_form, 
       data = burns_per_office, 
       family = 'poisson')

n_burns_mer <- 
  glmer(num_burns_form_mer_full, 
        data = burns_per_office, 
        offset = log_area_km2, 
        family = 'poisson', 
        control = control)

n_burns_mer2 <- 
  glmer(num_burns_form_mer_simple, 
        data = burns_per_office, 
        offset = log_area_km2, 
        family = 'poisson', 
        control = control)

anova(n_burns_mer, n_burns_mer2)

save(list = c('n_burns_poisson', 'n_burns_mer', 'n_burns_mer2'), 
     file = 'output/number_burned_allotments_per_blm_office.rda')

rm(n_burns_mer, n_burns_mer2, n_burns_poisson)

# burned area per office 
burn_area_form <- 
  formula( area_burned_km2_int ~ year2*ecogroup)

burn_area_form_mer_full <- 
  formula( area_burned_km2_int ~ year2*ecogroup + 
             (year2|office_label) + 
             (year2|district_label))

burn_area_form_mer_simple <- 
  formula( area_burned_km2_int ~ year2*ecogroup + 
             (1|office_label) + 
             (year2|district_label))

area_burn_glm <- glm( burn_area_form , 
                      data= burns_per_office, 
                      offset = log_area_km2, 
                      family = 'poisson')

area_burn_mer <- glmer( burn_area_form_mer_full, 
                        data = burns_per_office, 
                        offset = log_area_km2, 
                        family = 'poisson', 
                        control = control)

area_burn_mer2 <- glmer( burn_area_form_mer_simple, 
                        data = burns_per_office, 
                        offset = log_area_km2, 
                        family = 'poisson', 
                        control = control)

save(area_burn_glm, area_burn_mer, area_burn_mer2, 
     file = 'output/area_burned_per_blm_office.rda')

rm(area_burn_glm, area_burn_mer, area_burn_mer2)

# allotment-level analyses ------------------ # 

burn_data_sample <- 
  burn_data %>%
  filter( ecogroup %in% c('W Cold Deserts', 'E Cold Deserts'))

burn_p_formula <- 
  formula( burned_binary ~ 
             year2*ecogroup)

burn_p_form_mer_full <- 
  formula( burned_binary ~ 
             year2*ecogroup + (year2|uname) + (year2|office_label) + (year2|district_label))

burn_p_form_mer_simple <- 
  formula( burned_binary ~ 
             year2*ecogroup + (1|uname) + (year2|office_label) + (year2|district_label))

burn_p_form_mer_simple2 <- 
  formula( burned_binary ~ 
             year2*ecogroup + (1|uname) + (1|office_label) + (year2|district_label))

burn_data_sample <- burn_data_sample %>% 
  mutate( area_1e5_km2 =  area_km2/1e5 )

burn_p_m <- glm( burn_p_formula, 
                 data = burn_data_sample, 
                 offset = area_1e5_km2,  
                 family = 'binomial')

summary(burn_p_m)

burn_p_mer <- glmer( 
  burn_p_form_mer_full, 
  data = burn_data_sample, 
  offset = area_1e5_km2,
  family = 'binomial', 
  control = control)

summary(burn_p_mer)

burn_p_mer2 <- glmer( 
  burn_p_form_mer_simple, 
  data = burn_data_sample, 
  offset = area_1e5_km2,
  family = 'binomial', 
  control = control)

burn_p_mer3 <- glmer( 
  burn_p_form_mer_simple2, 
  data = burn_data_sample, 
  offset = area_1e5_km2,
  family = 'binomial', 
  control = control)

# num burns per allotment --------------------- # 
num_burns_per_allotment <- 
  formula( num_fires ~ year2*ecogroup + (year2|district_label) )

mod_num <- glmer(data = burn_data_sample, 
                 num_burns_per_allotment, 
                 offset = log_area_km2, 
                 family = 'poisson', 
                 control = control)

summary(mod_num)
