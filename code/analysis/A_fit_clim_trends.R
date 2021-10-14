rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(optimx)
library(dfoptim)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

clim <- read_csv('data/RAP_EE_exports/allotment_climate_by_year.csv')
load('data/analysis_data/cover.rda')

clim <- cover$AFGC %>% 
  distinct(uname, year, ecogroup, office_label ) %>%
  left_join( clim, by = c('year', 'uname')) %>% 
  select( uname, ecogroup, office_label, year, pdsi, pr, tavg )

control_lmer$optCtrl$eval.max <- 1e8
control_lmer$optCtrl$iter.max <- 1e8

trend_formula <- formula(value2 ~ year2 * ecogroup +
                           (year2 | ecogroup:office_label ) + 
                           (year2 | uname) 
)

trend_formula2 <- formula(value2 ~ year2 + 
                            (year2 | ecogroup ) +
                            (year2 | ecogroup:office_label ) + 
                            (year2 | uname) 
)


clim2 <- clim %>%
  pivot_longer(cols = c(tavg,pr,pdsi)) %>% 
  split( f = .$name ) %>% 
  lapply( . , FUN = function( x ) { x %>%  
            mutate( value2 = scale(value), year2 = scale(year)) } )

tavg_mer <- lmer(data = clim2$tavg, 
                 formula = trend_formula, 
                 control = control_lmer)

summary( tavg_mer )
tavg_ecogroup_trends <- emmeans::emtrends(tavg_mer, specs = ~ ecogroup, var = 'year2')

tavg_center <- attr(clim2$tavg$value2, 'scaled:center')
tavg_scale <- attr(clim2$tavg$value2, 'scaled:scale')
year_scale <- attr(clim2$tavg$year2, 'scaled:scale')

as.data.frame(tavg_ecogroup_trends) %>% 
  mutate( tavg_scale = year2.trend*tavg_scale/year_scale)

clim2$tavg %>% 
  ggplot( aes( x = year, y = value ))  + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap( ~ ecogroup  ) 

# PDSI  ---------------------------------- # 
pdsi_center <- attr( clim2$pdsi$value2, 'scaled:center')
pdsi_scale <- attr( clim2$pdsi$value2, 'scaled:scale')
year_scale <- attr(clim2$pdsi$year2, 'scaled:scale')

clim2$pdsi %>% 
  ggplot( aes( x = year, y = value )) + 
  geom_point() + 
  geom_smooth(method= 'lm') + 
  facet_wrap( ~ ecogroup ) 


pdsi_mer <- lmer(data = clim2$pdsi, 
                 formula = trend_formula, 
                 control = control_lmer)

summary( pdsi_mer )
pdsi_ecogroup_trends <- emmeans::emtrends(pdsi_mer, specs = ~ ecogroup, var = 'year2')

as.data.frame(pdsi_ecogroup_trends) %>% 
  mutate(pdsi_scale = year2.trend*pdsi_scale/year_scale)

# LOG PR  ---------------------------------- # 
clim2$pr <- clim2$pr %>% 
  mutate( value2 = scale( log(value)))

pr_center <- attr( clim2$pr$value2, 'scaled:center')
pr_scale <- attr( clim2$pr$value2, 'scaled:scale')
year_scale <- attr(clim2$pr$year2, 'scaled:scale')

clim2$pr %>% 
  ggplot( aes( x = year, y = value2 )) + 
  geom_point() + 
  geom_smooth(method= 'lm') + 
  facet_wrap( ~ ecogroup ) 


pr_mer <- lmer(data = clim2$pr, 
                 formula = trend_formula, 
                 control = control_lmer)

summary( pr_mer )
pr_ecogroup_trends <- emmeans::emtrends(pr_mer, specs = ~ ecogroup, var = 'year2')

as.data.frame(pr_ecogroup_trends) %>% 
  mutate( pr_scale = year2.trend*pr_scale/year_scale)

