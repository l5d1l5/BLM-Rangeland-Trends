rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)
library(lmerTest)

source('code/analysis/plot_tools.R')
load( 'output/phenology_models.rda')

control = lmerControl(optimizer = "optimx", 
                      calc.derivs = FALSE,
                      optCtrl = list(method = "nlminb", 
                                     starttests = FALSE, 
                                     kkt = FALSE))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

allotments <- 
  tbl(con, 'allotments') %>% 
  select(uname, 
         allot_name,  
         admu_name,
         admin_st, 
         parent_cd, 
         parent_name, ecogroup, acres) %>% 
  left_join( tbl(con, 'elevation'), by = 'uname') %>%
  filter( ecogroup != 'Coastal Forest') 


pheno_df <- IQR_mer@frame 
pheno_ranef <- get_blm_random_effects(IQR_mer)
pheno_ecogroup <- get_ecogroup_trends(IQR_mer)
trend_summary <- blm_trend_summary(pheno_df, pheno_ecogroup, pheno_ranef)

any(is.na(trend_summary$allot_trend)) 
stopifnot( all(complete.cases(trend_summary)) )



