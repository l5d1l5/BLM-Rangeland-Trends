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
basic_form <- formula( value2 ~ year2*ecogroup + (year2|uname) + (year2|office_label) + (year2|district_label) )

# Functions for summarizing trends at the Ecogroup and BLM Admin Level
get_ecogroup_trends <- function( model ){ 
  
  fixeffects <- emtrends(model, ~ ecogroup, 'year2') %>% 
    as.data.frame()
  
  ecogroup_effect <- fixeffects$year2.trend
  names(ecogroup_effect) <-  c( str_trim( fixeffects$ecogroup ) )
  
  return( ecogroup_effect ) 
} 


get_blm_random_effects <- function( model ) { 
  
  dist_effects <- ranef(model)$district_label 
  office_effects <- ranef(model)$office_label
  allot_effects <- ranef(model)$uname
  
  out <- list( dist_effects, office_effects, allot_effects ) 
  names( out ) <- c("dist_effects", "office_effects", "allot_effects")
  
  return( out )
} 


blm_trend_summary <- function( my_data , ecogroup_effects,  random_effects ){ 
  
  trend_summary <- my_data %>% 
    distinct(ecogroup, uname, office_label, district_label) %>% 
    mutate( ecogroup_trend = ecogroup_effects[ ecogroup] ) %>% 
    mutate( office_trend = random_effects$office_effects[office_label, ]$year2, 
            district_trend = random_effects$dist_effects[district_label, ]$year2, 
            allotment_trend = random_effects$allot_effects[uname, ]$year2) %>% 
    rowwise() %>% 
    mutate( full_trend = ecogroup_trend + 
              district_trend + 
              office_trend + allotment_trend) %>% 
    select( full_trend, office_trend, district_trend, allotment_trend, ecogroup_trend, ecogroup,  district_label, office_label, uname )
  
  return(trend_summary) 
}


con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)


allotments <- tbl(con, 'allotments') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, acres) %>% 
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) 

annual_data <- 
  tbl(con, 'annual_data') %>% 
  filter( year  > 1990 ) %>%
  filter( value > 0 ) %>% 
  left_join(allotments, by = 'uname') %>% 
  filter( ecogroup != "Coastal Forests")

afg <- 
  annual_data %>% 
  filter( type == 'afgAGB') %>%
  group_by( type, uname ) %>%
  filter( min(value) > 0.5 ) %>% 
  ungroup() %>% 
  select( uname, year,value, admin_st, district_label, office_label, ecogroup) %>% 
  collect() %>% 
  as.data.frame() %>% 
  mutate( value2 = scale(log(value))) %>%
  mutate( year2 = scale(year, scale = F)) %>% 
  mutate( value1 = log(value))

pfg <- 
  annual_data %>% 
  filter( type == 'pfgAGB') %>%
  group_by( type, uname ) %>%
  filter( min(value) > 0.5 ) %>% 
  ungroup() %>% 
  select( uname, year,value, admin_st, district_label, office_label, ecogroup) %>% 
  collect() %>% 
  as.data.frame() %>% 
  mutate( value2 = scale(log(value))) %>%
  mutate( year2 = scale(year, scale = F))  %>% 
  mutate( value1 = log(value)) 

afg_attributes <- attributes( afg$value2)
pfg_attributes <- attributes( pfg$value2)

# test backtransformation 
stopifnot( 
  all.equal( back_transform(afg$value2[1:10], afg_attributes), 
             afg$value[1:10] ) ) 

m_afg_agb <- lmer(data = afg,           
           basic_form, 
          control = control)

afg_fixed <- get_ecogroup_trends(m_afg_agb)
afg_random <- get_blm_random_effects(m_afg_agb)
afg_trends <- blm_trend_summary( afg, afg_fixed, afg_random)

saveRDS(m_afg_agb, file = 'output/afg_agb_trend_model.rds')
write_csv(afg_trends, file = 'output/afg_group_agb_trends.csv')

# PFG Trends 
m_pfg_agb <- lmer(data = pfg, basic_form, control = control)
summary(m_pfg_agb)

pfg_fixed <- get_ecogroup_trends(m_pfg_agb)
pfg_random <- get_blm_random_effects(m_pfg_agb)
pfg_trends <- blm_trend_summary( pfg, pfg_fixed, pfg_random)

saveRDS(m_pfg_agb, file = 'output/pfg_agb_trend_model.rds')
write_csv(pfg_trends, file = 'output/pfg_group_agb_trends.csv')

rm( afg, pfg, m_afg_agb, m_pfg_agb)

# Cover trends: 
cover <- 
  annual_data %>%  
  filter( type %in% c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR')) %>% 
  group_by( type, uname ) %>% 
  filter( min( value)  > 0.5 ) %>% 
  ungroup()


AFGC <- cover %>% 
  filter( type == 'AFGC') %>%
  select( uname, year,value, admin_st, district_label, office_label, ecogroup) %>% 
  collect() %>% 
  as.data.frame() %>% 
  mutate( value2 = scale(log(value))) %>%
  mutate( year2 = scale(year, scale = F)) 

PFGC <- cover %>% 
  filter( type == 'PFGC') %>%
  select( uname, year,value, admin_st, district_label, office_label, ecogroup) %>% 
  collect() %>% 
  as.data.frame() %>% 
  mutate( value2 = scale(log(value))) %>%
  mutate( year2 = scale(year, scale = F)) 

BG <- cover %>% 
  filter( type == 'BG') %>%
  select( uname, year,value, admin_st, district_label, office_label, ecogroup) %>% 
  collect() %>% 
  as.data.frame() %>% 
  mutate( value2 = scale(log(value))) %>%
  mutate( year2 = scale(year, scale = F)) 

TREE <- cover %>% 
  filter( type == 'TREE') %>%
  select( uname, year,value, admin_st, district_label, office_label, ecogroup) %>% 
  collect() %>% 
  as.data.frame() %>% 
  mutate( value2 = scale(log(value))) %>%
  mutate( year2 = scale(year, scale = F)) 


SHR <- cover %>% 
  filter( type == 'SHR') %>%
  select( uname, year,value, admin_st, district_label, office_label, ecogroup) %>% 
  collect() %>% 
  as.data.frame() %>% 
  mutate( value2 = scale(log(value))) %>%
  mutate( year2 = scale(year, scale = F)) 

# Fit annual cover data 
m_afgc <- lmer(data = AFGC, basic_form, control = control)
ecogroup_effects <- get_ecogroup_trends( m_afgc) 
random_effects <- get_blm_random_effects( m_afgc)
AFGC_trends <- blm_trend_summary( AFGC , ecogroup_effects, random_effects) 

# Fit perennial cover data 
m_pfgc <- lmer(data = PFGC, basic_form, control = control)
ecogroup_effects <- get_ecogroup_trends(m_pfgc ) 
random_effects <- get_blm_random_effects( m_pfgc)
PFGC_trends <- blm_trend_summary( PFGC, ecogroup_effects , random_effects)

# Fit Bare ground cover --------
m_bare <- lmer(data = BG, basic_form, control = control)
ecogroup_effects <- get_ecogroup_trends(m_bare ) 
random_effects <- get_blm_random_effects( m_bare)
bare_trends <- blm_trend_summary( BG, ecogroup_effects , random_effects)

# Fit Bare ground cover --------
m_tree <- lmer(data = TREE, basic_form, control = control)
ecogroup_effects <- get_ecogroup_trends(m_tree ) 
random_effects <- get_blm_random_effects( m_tree)
tree_trends <- blm_trend_summary( TREE, ecogroup_effects , random_effects)

# Fit Woody ground cover --------
m_shrub <- lmer(data = SHR, basic_form, control = control)
ecogroup_effects <- get_ecogroup_trends(m_shrub ) 
random_effects <- get_blm_random_effects( m_shrub)
shrub_trends <- blm_trend_summary( SHR, ecogroup_effects , random_effects)

# ---------- Output 

saveRDS(m_afgc, file = 'output/AFG_cover_trend_model.rds')
saveRDS(m_pfgc, file = 'output/PFG_cover_trend_model.rds')
saveRDS(m_bare, file = 'output/BG_cover_trend_model.rds')
saveRDS(m_tree, file = 'output/TREE_cover_trend_model.rds')
saveRDS(m_shrub, file = 'output/SHR_cover_trend_model.rds')

write_csv(AFGC_trends, file = 'output/AFG_cover_group_trends.csv')
write_csv(PFGC_trends, file = 'output/PFG_cover_group_trends.csv')
write_csv(bare_trends, file = 'output/BG_cover_group_trends.csv')
write_csv(tree_trends, file = 'output/TREE_cover_group_trends.csv')
write_csv(shrub_trends, file = 'output/SHR_cover_group_trends.csv')

