# Analyze phenology trends 

rm(list = ls())
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lmerTest)

source('code/analysis/plot_tools.R')

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

# Constants
lbs_per_acre_min <- 5 
sample_per_year_min <- 22 
years_of_data_min <- 29

# 
allotments <- tbl( con, 'allotments') %>% 
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) 

elevation <- tbl(con, 'elevation') 
phenology <- read_rds('data/temp/allotment_growing_season.rds')
#
phenology <- phenology %>%
  select( uname, year, crit_val, doy_crit) %>% 
  mutate( label = 'crit_val') %>%
  unite('label', c('label', 'crit_val'), sep = '_') %>% 
  pivot_wider(names_from = label, values_from = doy_crit) %>%
  mutate( IQR = crit_val_0.75 - crit_val_0.25) 

phenology <- allotments %>%
  filter( ecogroup != 'Coastal Forests') %>%
  #left_join(elevation, by = 'uname') %>% 
  collect() %>% 
  left_join( phenology , by = 'uname')  %>% 
  filter( year > 1990 , year < 2020, !is.na(IQR))

phenology <- phenology %>% 
  select( uname, year, ecogroup, district_label, office_label, 
          IQR, crit_val_0.25, crit_val_0.5, crit_val_0.75)  %>%
  mutate( IQR2 = scale(IQR), cv_0.25_2 = scale(crit_val_0.25), 
          cv_0.5_2 = scale(crit_val_0.5), 
          cv_0.75_2 = scale(crit_val_0.75)) %>% 
  mutate( year2 = scale(year, scale = F)) %>% 
  arrange( uname, year ) 

# Formula(s)
pheno_form_IQR <- formula( IQR2 ~ year2*ecogroup + (year2|uname) + 
                         (year2|office_label) + (year2|district_label))
pheno_form_cv25 <- formula(cv_0.25_2 ~  year2*ecogroup + (year2|uname) + 
                              (year2|office_label) + (year2|district_label))
pheno_form_cv50 <- formula(cv_0.5_2 ~ year2*ecogroup + (year2|uname) + 
                              (year2|office_label) + (year2|district_label))
pheno_form_cv75 <- formula(cv_0.75_2 ~ year2*ecogroup + (year2|uname) + 
                              (year2|office_label) + (year2|district_label))

#TODO add elevation? 

IQR_mer <- lmer( pheno_form_IQR, data = phenology, control = control ) 

cv25_mer <- lmer(pheno_form_cv25, data = phenology, control = control)
cv50_mer <- lmer(pheno_form_cv50, data = phenology, control = control)
cv75_mer <- lmer(pheno_form_cv75, data = phenology, control = control)


save(IQR_mer, 
     cv25_mer, 
     cv50_mer,  
     cv75_mer, 
     file = 'output/phenology_models.rda')
