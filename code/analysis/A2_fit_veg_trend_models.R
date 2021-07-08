rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)
library(optimx)
library(dfoptim)
library(parallel)

source('code/analysis/plot_tools.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)


allotments <- tbl(con, 'allotments') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, acres, area, climate_region) %>% 
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

agb <- annual_data %>% 
  filter( type %in% c('pfgAGB', 'afgAGB')) %>% 
  pivot_wider(names_from = type, values_from = value ) %>%
  mutate( herb_agb = pfgAGB + afgAGB ) %>%
  pivot_longer(c('pfgAGB', 'afgAGB', 'herb_agb'), 
               names_to = 'type', values_to = 'value' ) %>% 
  group_by( type, uname) %>% 
  filter( min(value, na.rm = T) > 0.5 ) %>%
  select( type, uname, year,value, admin_st, district_label, office_label, ecogroup, climate_region, area) %>% 
  collect() %>% 
  as.data.frame() %>% 
  group_by( type, uname) %>% 
  filter( !any(is.na(value))) %>% 
  ungroup() %>% 
  split(f = .$type ) 

agb <- 
  agb %>% 
  lapply( function(x){ 
    x %>% 
      mutate( value2 = scale(log(value)), 
              value1 = log(value)) %>%
      mutate( year2 = scale(year), 
              area2 = scale(area, center = F ))
    }) 


afg_attributes <- attributes( agb$afgAGB$value2)
pfg_attributes <- attributes( agb$pfgAGB$value2)
herb_attributes <- attributes(agb$herb_agb$value2)

# test back-transformation 
stopifnot( 
  all.equal( back_transform(agb$afg$value2[1:10], afg_attributes), 
             agb$afg$value[1:10] ) ) 


# AFG TRENDS
# Basic analysis formula for finding long-term annual trend in the data 
attach(agb)
write_csv(agb$pfgAGB, file = 'data/temp/pfg_agb.csv')
basic_form <- formula( value2 ~ year2*ecogroup +  
                         (year2|office_label) + (year2|uname) + (1|year:climate_region))

# sample_uname <- pfgAGB %>% 
#   select( ecogroup, office_label, uname ) %>% 
#   arrange( ecogroup,  office_label, uname) %>%
#   distinct() %>%
#   group_by(ecogroup, office_label) %>% 
#   filter( row_number() < 10 ) %>% 
#   pull(uname)
# 
# # 
# sample_dat <- pfgAGB %>% filter( uname %in% sample_uname )
# 
# m_pfg_agb <- lmer(formula = basic_form, 
#                   data = sample_dat)
# 
# # check singularity 
# tt <- getME(m_pfg_agb,"theta")
# ll <- getME(m_pfg_agb,"lower")
# min(tt[ll==0])
# 
# m_pfg_agb@optinfo
# summary(m_pfg_agb)
# 
# meth_tab <- data.frame( allFit(show.meth.tab=TRUE)  ) %>% 
#   bind_rows( data.frame( optimizer = 'optimx', method = c('nlminb' , 'bobyqa', 'BFGS')))
# 
# ncores <- parallel::detectCores()
# 
# diff_optims <- allFit(m_pfg_agb, 
#                       meth.tab = meth_tab, 
#                       maxfun = 1e5, 
#                       parallel = 'multicore', ncpus = ncores)
# 
# summary(diff_optims)
# Try with full dataset 
m_pfg_agb <- lmer(formula = basic_form, 
                  data = pfgAGB, 
                  control = control_lmer)

saveRDS(m_pfg_agb, file = 'output/pfg_agb_trend_model.rds')

m_afg_agb <- lmer(formula = basic_form, 
                  data = afgAGB, 
                  control = control_lmer)

summary(m_afg_agb)

saveRDS(m_afg_agb, file = 'output/afg_agb_trend_model.rds')

m_herb_agb <- lmer(data = herb_agb, 
                   basic_form, 
                   control = lmerControl(optimizer = 'optimx', 
                                         optCtrl = list( method = 'nlminb', 
                                                         eval.max = 1e3, 
                                                         iter.max = 1e3)))

saveRDS(m_herb_agb, file = 'output/herb_agb_trend_model.rds')

# Check convergence with different methods, could take a while ---------------------------- # 
# diff_optims <- allFit(m_pfg_agb, 
#                       meth.tab = meth_tab, 
#                       maxfun = 1e6, 
#                       parallel = 'multicore', 
#                       ncpus = ncores)

# ------------- # 

afg_fixed <- get_ecogroup_trends(m_afg_agb)
afg_random <- get_blm_random_effects(m_afg_agb)
afg_trends <- blm_trend_summary( afgAGB, afg_fixed, afg_random)

stopifnot( all(complete.cases(afg_trends)))

# PFG TRENDS
pfg_fixed <- get_ecogroup_trends(m_pfg_agb)
pfg_random <- get_blm_random_effects(m_pfg_agb)
pfg_trends <- blm_trend_summary( pfgAGB, pfg_fixed, pfg_random)

# Herbaceous AGB Trends AFG + PFG = Total Herbaceous 
herb_fixed <- get_ecogroup_trends(m_herb_agb)
herb_random <- get_blm_random_effects(m_herb_agb)
herb_trends <- blm_trend_summary(herb_agb, herb_fixed, herb_random)

# SAVE AGB models and output 
write_csv(afg_trends, file = 'output/afg_group_agb_trends.csv')
write_csv(pfg_trends, file = 'output/pfg_group_agb_trends.csv')
write_csv(herb_trends, file = 'output/herb_group_agb_trends.csv')

detach(agb)
rm( agb, m_afg_agb, m_pfg_agb, m_herb_agb)

# Cover trends: 
cover <- 
  annual_data %>%  
  filter( type %in% c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR')) %>% 
  pivot_wider(names_from = type, values_from = value ) %>%
  mutate( HERB = AFGC + PFGC ) %>%
  pivot_longer( c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR', 'HERB'), 
               names_to = 'type', values_to = 'value' )  %>% 
  group_by( type, uname) %>% 
  filter( min(value, na.rm = T) > 0.5 ) %>%
  select( type, uname, year,value, admin_st, district_label, office_label, ecogroup, area, climate_region) %>% 
  collect() %>% 
  as.data.frame() %>% 
  group_by( type, uname) %>% 
  filter( !any(is.na(value))) %>% 
  ungroup() %>% 
  split(f = .$type ) 

cover <- cover %>% lapply( 
  function(x) { 
    x %>% mutate( value2 = scale(log(value))) %>%
      mutate( year2 = scale(year), 
              area2 = scale(area, center = F)) }) 

save(cover, file = 'output/cover.rda')
attach( cover )

# Fit annual cover data 
m_afgc <- lmer(data = AFGC, basic_form, control = my_control)
tt <- getME(m_afgc,"theta")
ll <- getME(m_afgc,"lower")
min(tt[ll==0])

ecogroup_effects <- get_ecogroup_trends( m_afgc) 
random_effects <- get_blm_random_effects( m_afgc)
AFGC_trends <- blm_trend_summary( AFGC , ecogroup_effects, random_effects) 

# Fit perennial cover data 
m_pfgc <- lmer(data = PFGC, basic_form, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_pfgc ) 
random_effects <- get_blm_random_effects( m_pfgc)
PFGC_trends <- blm_trend_summary( PFGC, ecogroup_effects , random_effects)

# Fit Bare ground cover --------
m_bare <- lmer(data = BG, basic_form, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_bare ) 
random_effects <- get_blm_random_effects( m_bare)
bare_trends <- blm_trend_summary( BG, ecogroup_effects , random_effects)

# Fit Bare ground cover --------
m_tree <- lmer(data = TREE, basic_form, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_tree ) 
random_effects <- get_blm_random_effects( m_tree)
tree_trends <- blm_trend_summary( TREE, ecogroup_effects , random_effects)

# Fit Woody ground cover --------
m_shrub <- lmer(data = SHR, basic_form, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_shrub ) 
random_effects <- get_blm_random_effects( m_shrub)
shrub_trends <- blm_trend_summary( SHR, ecogroup_effects , random_effects)

# Fit total herbaceous ---------- 

m_herb <- lmer(data = HERB, basic_form, control = control_lmer)
# note total herbaceous cover is NOT log transformed 
ecogroup_effects <- get_ecogroup_trends(m_herb) 
random_effects <- get_blm_random_effects( m_herb)
herb_trends <- blm_trend_summary(HERB, ecogroup_effects , random_effects)

# ---------- Output 
saveRDS(m_afgc, file = 'output/AFG_cover_trend_model.rds')
saveRDS(m_pfgc, file = 'output/PFG_cover_trend_model.rds')
saveRDS(m_bare, file = 'output/BG_cover_trend_model.rds')
saveRDS(m_tree, file = 'output/TREE_cover_trend_model.rds')
saveRDS(m_shrub, file = 'output/SHR_cover_trend_model.rds')
saveRDS(m_herb, file = 'output/HERB_cover_trend_model.rds')

write_csv(AFGC_trends, file = 'output/AFG_cover_group_trends.csv')
write_csv(PFGC_trends, file = 'output/PFG_cover_group_trends.csv')
write_csv(bare_trends, file = 'output/BG_cover_group_trends.csv')
write_csv(tree_trends, file = 'output/TREE_cover_group_trends.csv')
write_csv(shrub_trends, file = 'output/SHR_cover_group_trends.csv')
write_csv(herb_trends, file = 'output/HERB_cover_group_trends.csv')

detach(cover )
rm( cover ) 