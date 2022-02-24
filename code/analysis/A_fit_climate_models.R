rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(optimx)
library(dfoptim)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

allotments <- read_csv('data/temp/allotment_info.csv') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, hectares, area, climate_region, 
          Other, Private, BLM, elevation.x ) %>%
  rename( elevation = elevation.x ) %>%
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) %>% 
  filter( ecogroup != 'Marine West Coast Forest')

# ---------------------------- # 

climate <- read_csv('data/RAP_EE_exports/allotment_climate_by_year.csv') %>% 
  filter( year > 1990 , year < 2021 ) %>% 
  select( uname, year, pdsi, pr, tavg) %>% 
  pivot_longer(pdsi:tavg ) %>% 
  arrange( uname, name, year ) %>% 
  left_join(allotments, by = 'uname') 

climate <- 
  climate %>% 
  group_by( name ) %>% 
  mutate( value2 = scale(value), year2 = scale(year )) %>% 
  ungroup() 

# ------------------------------------------ # 
control_lmer$optCtrl$eval.max <- 1e8
control_lmer$optCtrl$iter.max <- 1e8

trend_formula <- formula(value2 ~ year2 * ecogroup +
                           (year2 | ecogroup:office_label ) + 
                           (year2 | uname) 
)

trend_formula2 <- update(trend_formula, . ~ . - year2:ecogroup)
trend_formula3 <- update(trend_formula2, . ~ . - ecogroup)
trend_formula4 <- update(trend_formula2, . ~ . - year2)

# PDSI 
m_pdsi1 <- lmer(data = climate %>% 
                 filter( name == 'pdsi'), 
               formula = trend_formula, 
               control = control_lmer, REML = F)

m_pdsi2 <- lmer(data = climate %>% 
                 filter( name == 'pdsi'), 
               formula = trend_formula2, 
               control = control_lmer, REML = F)

m_pdsi3 <- lmer(data = climate %>% 
                  filter( name == 'pdsi'), 
                formula = trend_formula3, 
                control = control_lmer, REML = F)

m_pdsi4 <- lmer(data = climate %>% 
                  filter( name == 'pdsi'), 
                formula = trend_formula4, 
                control = control_lmer, REML = F)

table <- MuMIn::model.sel(m_pdsi1, m_pdsi2, m_pdsi3, m_pdsi4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')

kableExtra::save_kable(table, file = 'output/tables/pdsi_model_comparison_table.md')

m_pdsi <- update( m_pdsi1, REML = T)

# 
m_tavg1 <- lmer(data = climate %>% 
                  filter( name == 'tavg'), 
                formula = trend_formula, 
                control = control_lmer, REML = F)

m_tavg2 <- lmer(data = climate %>% 
                  filter( name == 'tavg'), 
                formula = trend_formula2, 
                control = control_lmer, REML = F)

m_tavg3 <- lmer(data = climate %>% 
                  filter( name == 'tavg'), 
                formula = trend_formula3, 
                control = control_lmer, REML = F)

m_tavg4 <- lmer(data = climate %>% 
                  filter( name == 'tavg'), 
                formula = trend_formula4, 
                control = control_lmer, REML = F)

table <- MuMIn::model.sel(m_tavg1, m_tavg2, m_tavg3, m_tavg4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')

table 

kableExtra::save_kable(table, file = 'output/tables/tavg_model_comparison_table.md')
m_tavg <- update( m_tavg1, REML = T)

# --------------- # 
m_pr1 <- lmer(data = climate %>% 
                  filter( name == 'pr'), 
                formula = trend_formula, 
                control = control_lmer, REML = F)

m_pr2 <- lmer(data = climate %>% 
                  filter( name == 'pr'), 
                formula = trend_formula2, 
                control = control_lmer, REML = F)

m_pr3 <- lmer(data = climate %>% 
                  filter( name == 'pr'), 
                formula = trend_formula3, 
                control = control_lmer, REML = F)

m_pr4 <- lmer(data = climate %>% 
                  filter( name == 'pr'), 
                formula = trend_formula4, 
                control = control_lmer, REML = F)

table <- MuMIn::model.sel(m_pr1, m_pr2, m_pr3, m_pr4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')


table 

kableExtra::save_kable(table, file = 'output/tables/pr_model_comparison_table.md')
m_pr <- update( m_pr1, REML = T)

# Get ecogroup level trends 
ecogroup_effects <- get_ecogroup_trends( m_pdsi  ) 
random_effects <- get_blm_random_effects( m_pdsi)
pdsi_trends <- blm_trend_summary( climate %>% filter( name == 'pdsi'), ecogroup_effects , random_effects)

ecogroup_effects <- get_ecogroup_trends( m_pr  ) 
random_effects <- get_blm_random_effects( m_pr)
pr_trends <- blm_trend_summary( climate %>% filter( name == 'pr'), ecogroup_effects , random_effects)

ecogroup_effects <- get_ecogroup_trends( m_tavg  ) 
random_effects <- get_blm_random_effects( m_tavg)
tavg_trends <- blm_trend_summary( climate %>% filter( name == 'tavg'), ecogroup_effects , random_effects)

#-------------------- # 
saveRDS(m_pdsi, file = 'output/pdsi_trend_model.rds')
saveRDS(m_pr, file = 'output/pr_trend_model.rds')
saveRDS(m_tavg, file = 'output/tavg_trend_model.rds')


write_csv(pdsi_trends, file = 'output/pdsi_group_trends.csv')
write_csv(pr_trends, file = 'output/pr_group_trends.csv')
write_csv(tavg_trends, file = 'output/tavg_group_trends.csv')

save(climate, file = 'data/analysis_data/climate_data.rda')

