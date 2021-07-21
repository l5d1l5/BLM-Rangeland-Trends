# Analyze phenology trends 

rm(list =ls())
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )
library(lmerTest)

source('code/analysis/plot_tools.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

# 
allotments <- tbl( con, 'allotments') %>% 
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) 

phenology <- read_rds('data/temp/allotment_growing_season.rds')

phenology <- phenology %>%
  select( uname, year, crit_val, doy_crit ) %>% 
  mutate( label = 'crit_val') %>%
  unite('label', c('label', 'crit_val'), sep = '_') %>% 
  pivot_wider(names_from = label, values_from = doy_crit) %>%
  mutate( GSL = crit_val_0.75 - crit_val_0.25) %>% 
  rename( SOS = crit_val_0.25, EOS = crit_val_0.75, MS = crit_val_0.5)

phenology <- allotments %>%
  filter( ecogroup != 'Coastal Forests') %>%
  #left_join(elevation, by = 'uname') %>% 
  collect() %>% 
  left_join( phenology , by = 'uname')  %>% 
  filter( year > 1990 , year < 2020, !is.na(GSL))

phenology <- phenology %>%
  group_by( uname ) %>% 
  filter( n_distinct(year) >= 29 )

#
phenology %>% 
  group_by( climate_region, ecogroup ) %>%
  summarise( n_distinct(year), n_distinct(uname), n_distinct(office_label) )

# ----------------- # 
phenology <- phenology %>% 
  data.frame() %>% 
  dplyr::select( uname, year, climate_region, ecogroup, district_label, office_label, 
          GSL, SOS, MS, EOS)  %>%
  mutate( GSL2 = scale(GSL), 
          SOS2 = scale(SOS), 
          MS2 = scale(MS), 
          EOS2 = scale(EOS)) %>% 
  mutate( year2 = scale(year)) %>% 
  arrange( uname, year ) 

# Formula(s)
pheno_form_GSL <- formula( GSL2 ~ year2*ecogroup + (year2|uname) + 
                             (year2|office_label) + (1|year:climate_region))


pheno_form_SOS <- formula(SOS2 ~  year2*ecogroup  + (year2|uname) + 
                             (year2|office_label) + (1|year:climate_region))

pheno_form_MS <- formula(MS2 ~ year2*ecogroup  +  (year2|uname) + 
                             (year2|office_label) + (1|year:climate_region))

pheno_form_EOS <- formula(EOS2 ~ year2*ecogroup  +  (year2|uname) + 
                             (year2|office_label) + (1|year:climate_region))



GSL_mer <- lmer( pheno_form_GSL, data = phenology, control = control_lmer) 

SOS_mer <- lmer(pheno_form_SOS, data = phenology, control = control_lmer)
EOS_mer <- lmer(pheno_form_EOS, data = phenology, control = control_lmer)
MS_mer <- lmer(pheno_form_MS, data = phenology, control = control_lmer)

save( GSL_mer, 
     SOS_mer, 
     MS_mer,  
     EOS_mer, 
     file = 'output/phenology_models.rda')
