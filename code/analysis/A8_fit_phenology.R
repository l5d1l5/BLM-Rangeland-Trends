# Analyze phenology trends 

rm(list =ls())
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )
library(lmerTest)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

phenology <- read_rds('data/temp/allotment_phenology.rds')

phenology <- phenology %>%
  select( climate_region, ecogroup, office_label, uname, year, crit_val, doy_crit ) %>% 
  mutate( label = 'crit_val') %>%
  unite('label', c('label', 'crit_val'), sep = '_') %>% 
  pivot_wider(names_from = label, values_from = doy_crit) %>%
  mutate( GSL = crit_val_0.75 - crit_val_0.25) %>% 
  rename( SOS = crit_val_0.25, EOS = crit_val_0.75, MS = crit_val_0.5)

# ----------------- # 
phenology <- 
  phenology %>% 
  filter(!is.na(GSL)) %>% 
  data.frame() %>% 
  dplyr::select( uname, year, climate_region, ecogroup, office_label, 
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


# fit models 
GSL_mer <- lmer( pheno_form_GSL, data = phenology, control = control_lmer) 
SOS_mer <- lmer(pheno_form_SOS, data = phenology, control = control_lmer)
EOS_mer <- lmer(pheno_form_EOS, data = phenology, control = control_lmer)
MS_mer <- lmer(pheno_form_MS, data = phenology, control = control_lmer)

save( GSL_mer, 
     SOS_mer, 
     MS_mer,  
     EOS_mer, 
     file = 'output/phenology_models.rda')
