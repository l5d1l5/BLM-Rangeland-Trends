rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)

source('code/analysis/plot_tools.R')

# LMER options 
# Basic analysis formula for finding long-term annual trend in the data 
basic_form <- formula( has_burned ~ year*ecogroup + (year|uname) + (year|office_label) + (year|district_label) )

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


burn_data <- 
  tbl(con, 'annual_data') %>% 
  filter( type == 'burned')  %>%
  mutate( has_burned = value > 1e-6 ) %>% 
  left_join(allotments) %>% 
  filter( ecogroup != 'Coastal Forests') %>%
  collect() %>% 
  mutate( year2 = scale( year) ) %>%
  mutate( has_burned = as.numeric(has_burned)) %>% 
  mutate( million_acres = acres/1e+6)

control = glmerControl(optimizer = "optimx", 
                       calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", 
                                      starttests = FALSE, 
                                      kkt = FALSE, 
                                      maxit = 10000))

# basic_form <- formula( has_burned ~ year2*ecogroup + (year2|uname) + (year2|office_label) + (year2|district_label))
# 
# burn_binom_model <-
#   glmer(data = burn_data,
#         formula = basic_form, 
#         offset = million_acres, 
#         family = binomial, 
#         control = control)   
# 
# summary( burn_binom_model  )
# 
# ecogroup_effects <- get_ecogroup_trends(burn_binom_model)
# random_effects <- get_blm_random_effects(burn_binom_model)
# 
# trend_summary <- blm_trend_summary( burn_data, ecogroup_effects , random_effects)
# 
# saveRDS(burn_binom_model, file = 'output/burn_binom_trend_model.rds')
# write_csv(trend_summary, file = 'output/burn_trends_by_allotment.csv')


# Poisson model for acres burned per office 

burned_acres <- 
  burn_data %>% 
  mutate( acres_burned = value*acres ) %>% 
  group_by( year2, district_label, office_label, ecogroup ) %>% 
  summarise( acres_burned = sum(acres_burned), total_acres = sum(acres, na.rm = T) )  %>% 
  mutate( frac_burned = acres_burned/total_acres ) %>% 
  mutate( log_total_acres = log(total_acres))

burned_acres %>% 
  ggplot( aes( x = year2, y = acres_burned )) + 
  geom_bar(stat ='identity') + 
  facet_wrap(~ecogroup) 

burn_data %>%
  filter( value > 0 ) %>% 
  ggplot( aes( x = value )) + geom_histogram()


area_burned_form <- formula( sq_km_burned ~ year2*ecogroup + (year2|office_label) + (year2|district_label))

burned_acres <- burned_acres  %>%
  mutate( sq_km_burned = acres_burned*0.00404686 , 
          total_sq_km = total_acres*0.00404686 , 
          log_total_sq_km = log(total_sq_km))

hist( burned_acres$sq_km_burned )

burned_acres$sq_km_burned = floor( burned_acres$sq_km_burned  )

table( burned_acres$sq_km_burned )

hist( burned_acres$sq_km_burned[ burned_acres$sq_km_burned > 1000] )

m_area <- glmer( data = burned_acres, 
       formula = area_burned_form, 
       family = 'poisson', 
       offset = log_total_sq_km)

ecogroup_effects <- get_ecogroup_trends(m_area)

random_effects <- get_blm_random_effects(m_area)


trend_summary <- burned_acres %>% 
  distinct(ecogroup, office_label, district_label) %>% 
  mutate( ecogroup_trend = ecogroup_effects[ ecogroup] ) %>% 
  mutate( office_trend = random_effects$office_effects[office_label, ]$year2, 
          district_trend = random_effects$dist_effects[district_label, ]$year2) %>% 
  rowwise() %>% 
  mutate( full_trend = ecogroup_trend + 
            district_trend + 
            office_trend) %>% 
  select( full_trend, office_trend, district_trend, ecogroup_trend, ecogroup,  district_label, office_label )


saveRDS(m_area, file = 'output/burn_area_poisson_model.rds')
write_csv(trend_summary, file = 'output/burn_trends_by_allotment.csv')
