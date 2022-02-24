rm(list = ls())
library(tidyverse)
library(sf)
library(lme4)
library(emmeans)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

# ------ 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_model_files, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree', 'Woody'))
names( cover_models ) <- types 

cover_att <- lapply( cover_models, function(x) attributes( x@frame$value2) )
cover_year_att <- lapply( cover_models, function(x) attributes( x@frame$year2) )

# Now back transform sd into units of log cover/log production per year
# trend_bt = trend_scaled*log_y_sd/year_sd 
trend_scales <- 
  unlist( cover_att, recursive = F ) %>% 
  data.frame() %>% 
  head( 1 ) %>%
  pivot_longer( Annual.dim:Tree.scaled.scale , 
                names_to = 'type', values_to = 'value') %>%
  separate( type, into = c('type', 'stat', 'par'))  %>% 
  filter( par == 'scale' ) %>%
  select( type, par, value ) %>% 
  mutate( unit = 'Cover', par2 = 'log_y_sd') %>% 
  bind_rows(
    unlist( cover_year_att, recursive = F ) %>% 
      data.frame() %>% 
      head(1) %>%
      pivot_longer( Annual.dim:Tree.scaled.scale , 
                    names_to = 'type', values_to = 'value') %>%
      separate( type, into = c('type', 'stat', 'par'))  %>% 
      filter( par == 'scale' ) %>% 
      select( type, par, value ) %>% 
      mutate( unit = 'Cover', par2 = 'year_sd')
  ) 

trend_scales <- 
  trend_scales %>% 
  pivot_wider( names_from = par2 , values_from = value) %>%
  mutate( trend_unit = log_y_sd/year_sd)

cover_trend_scales <- trend_scales

# Predict and get rates 
pred_grid <- lapply( types , function( x ) { 
  m <- cover_models[[x]]
  m@frame %>% 
    mutate( type = x ) %>% 
    filter( year2 == max(year2) | year2 == min(year2)) %>% 
    arrange(uname, year2) %>% 
    mutate( yhat = predict( m , newdata = . )) %>% 
    group_by( type, uname ) %>% 
    dplyr::summarise( rate = (yhat[which.max(year2)] - yhat[which.min(year2)])/(max(year2) - min(year2)))
  })


trends <- do.call(rbind, pred_grid) %>% 
  ungroup() %>%
  left_join(trend_scales, by = 'type') %>% 
  mutate( bt_trend = rate*trend_unit ) %>% 
  select(type, uname, rate, bt_trend )

# Sanity check on back transformation 
# rf <- ranef( cover_models$Annual )
# rf$uname$uname <- as.numeric( row.names( rf$uname) )
# rf$`ecogroup:office_label`$`ecogroup:office_label` <- row.names( rf$`ecogroup:office_label` )
# rf$`ecogroup:office_label` <- rf$`ecogroup:office_label` %>%
#   separate( `ecogroup:office_label`, sep = ':', into = c('ecogroup', 'office_label'))
# 
# ff <- emtrends(cover_models$Annual, ~ ecogroup, 'year2') %>% as.data.frame()
# 
# df <- cover_models$Annual@frame
# 
# test_df <- df %>%
#   distinct( ecogroup, office_label, uname ) %>%
#   left_join( rf$uname, by = 'uname') %>%
#   left_join( rf$`ecogroup:office_label`, by = c('ecogroup', 'office_label')) %>%
#   left_join(ff , by = 'ecogroup') %>%
#   select(ecogroup, uname, office_label, starts_with('(Inter'), starts_with('year2')) %>%
#   mutate( full_trend = year2.x  + year2.y + year2.trend) %>%
#   mutate( type = 'Annual', unit = 'Cover') %>%
#   left_join(trend_scales, by = c('type', 'unit')) %>%
#   mutate( trend_unit = trend_scales$trend_unit[trend_scales$type == 'Annual']) %>%
#   mutate( bt_trend = full_trend*trend_unit)
# 
# test_equiv <- trends %>%
#   left_join(test_df, by = c('type', 'uname')) %>%
#   select( type, uname, rate, full_trend, bt_trend.x, bt_trend.y)
# 
# plot( test_equiv$bt_trend.x , test_equiv$bt_trend.y )
# abline(0,1)

#  test against true values
# load('data/analysis_data/cover.rda')
# load('data/analysis_data/allotments.rda')
# 
# cover_att$Annual
# cover_year_att$Annual
# 
# cover_models$Annual@frame %>%
#   distinct( ecogroup, uname) %>%
#   group_by( ecogroup) %>%
#   sample_n(2 ) %>%
#   select( uname ) %>%
#   left_join( cover$AFGC ) %>%
#   ungroup() %>% 
#   mutate(yhat2 = predict( cover_models$Annual, newdata = . )) %>%
#   select( ecogroup, uname, year, year2, value, value2, yhat2 ) %>%
#   mutate( yhat = exp(yhat2*cover_att$Annual$`scaled:scale` + cover_att$Annual$`scaled:center`) ) %>%
#   left_join(trends %>% filter( type == 'Annual'))  %>% 
#   left_join( test_equiv %>% select(type, uname, bt_trend.x, bt_trend.y), by = c('uname', 'type')) %>% 
#   group_by(ecogroup,  uname) %>%
#   mutate( yhat_test = yhat[which.min(year)]*exp(bt_trend*(year-min(year))) - 1 ) %>%
#   mutate( yhat_test.x = yhat[which.min(year)]*exp(bt_trend.x*(year-min(year)))) %>%
#   mutate( yhat_test.y = yhat[which.min(year)]*exp(bt_trend.y*(year-min(year)))) %>%
#   ggplot( aes( x = year, y = value, group = uname) ) +
#   geom_line() +
#   geom_line( aes( y = yhat, group = uname), color = 'black') +
#   geom_line( aes( y = yhat_test, group = uname), color = 'blue', linetype = 2) +
#   #geom_line( aes( y = yhat_test.x, group = uname), color = 'orange', linetype = 2) +
#   geom_line( aes( y = yhat_test.y, group = uname), color = 'red', linetype = 1) +
#   facet_wrap( ~ ecogroup )

# 

allotment_shp <- sf::read_sf('data/temp/BLM_allotments_cleaned/allotments.shp')

allotment_rates <- allotment_shp %>% 
  left_join(
  trends %>%
    ungroup() %>%
    select( - rate ) %>%
    pivot_wider( names_from = type, values_from = bt_trend )
) 


dir.create('data/temp/cover_rates')

allotment_rates %>% 
  st_write(dsn = 'data/temp/cover_rates/allotments_with_rates.shp', 
            layer = 'allotments', 
            append = F)

# Do the same for production trends
# ------------------------------------------------ # 
agb_model_files <- dir(path = 'output', pattern = '.*_agb_trend_model.rds', full.names = T)
agb_models <- lapply(agb_model_files, read_rds)
types  <- c( str_extract( agb_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Herbaceous', 'Perennial'))
names( agb_models ) <- types 

agb_att <- lapply( agb_models, function(x) attributes( x@frame$value2) )
agb_year_att <- lapply( agb_models, function(x) attributes( x@frame$year2) )

# Now back transform sd into units of log agb/log production per year
# trend_bt = trend_scaled*log_y_sd/year_sd 

trend_scales <- 
  unlist( agb_att, recursive = F ) %>% 
  data.frame() %>% 
  head( 1 ) %>%
  pivot_longer( Annual.dim:Perennial.scaled.scale , 
                names_to = 'type', values_to = 'value') %>% 
  separate( type, into = c('type', 'stat', 'par'))  %>% 
  filter( par == 'scale' ) %>%
  select( type, par, value ) %>% 
  mutate( unit = 'AGB', par2 = 'log_y_sd') %>% 
  bind_rows(
    unlist( agb_year_att, recursive = F ) %>% 
      data.frame() %>% 
      head(1) %>%
      pivot_longer( Annual.dim:Perennial.scaled.scale , 
                    names_to = 'type', values_to = 'value') %>%
      separate( type, into = c('type', 'stat', 'par'))  %>% 
      filter( par == 'scale' ) %>% 
      select( type, par, value ) %>% 
      mutate( unit = 'AGB', par2 = 'year_sd')
  ) 

trend_scales <- 
  trend_scales %>% 
  pivot_wider( names_from = par2 , values_from = value) %>%
  mutate( trend_unit = log_y_sd/year_sd)

cover_trend_scales %>% 
  bind_rows(trend_scales) %>% 
  write_csv('data/temp/trend_scales.csv')

# Predict and get rates 

pred_grid <- lapply( types , function( x ) { 
  m <- agb_models[[x]]
  m@frame %>% 
    mutate( type = x ) %>% 
    filter( year2 == max(year2) | year2 == min(year2)) %>% 
    arrange(uname, year2) %>% 
    mutate( yhat = predict( m , newdata = . )) %>% 
    group_by( type, uname ) %>% 
    dplyr::summarise( rate = (yhat[which.max(year2)] - yhat[which.min(year2)])/(max(year2) - min(year2)))
})


trends <- do.call(rbind, pred_grid) %>% 
  left_join(trend_scales, by = 'type') %>% 
  mutate( bt_trend = rate*trend_unit ) %>% 
  select( uname, rate, bt_trend )

allotment_shp <- sf::read_sf('data/temp/BLM_allotments_cleaned/allotments.shp')

allotment_rates <- allotment_shp %>% 
  left_join(
    trends %>%
      ungroup() %>%
      select( - rate ) %>%
      mutate( type = factor(type)) %>% 
      mutate( type = factor( type, labels = c('AFG', "HERB", 'PFG'))) %>% 
      pivot_wider( names_from = type, values_from = bt_trend )
  ) 

dir.create('data/temp/agb_rates')

allotment_rates %>% 
  st_write(dsn = 'data/temp/agb_rates/allotments_with_rates.shp', 
           layer = 'allotments', append = F)
 

# Sanity Check on AGB Rates -------------- # 
rf <- ranef( agb_models$Annual )
rf$uname$uname <- as.numeric( row.names( rf$uname) )
rf$`ecogroup:office_label`$`ecogroup:office_label` <- row.names( rf$`ecogroup:office_label` )
rf$`ecogroup:office_label` <- rf$`ecogroup:office_label` %>%
  separate( `ecogroup:office_label`, sep = ':', into = c('ecogroup', 'office_label'))

ff <- emtrends(agb_models$Annual, ~ ecogroup, 'year2') %>% as.data.frame()

df <- agb_models$Annual@frame

test_df <- df %>%
  distinct( ecogroup, office_label, uname ) %>%
  left_join( rf$uname, by = 'uname') %>%
  left_join( rf$`ecogroup:office_label`, by = c('ecogroup', 'office_label')) %>%
  left_join(ff , by = 'ecogroup') %>%
  select(ecogroup, uname, office_label, starts_with('(Inter'), starts_with('year2')) %>%
  mutate( full_trend = year2.x  + year2.y + year2.trend) %>%
  mutate( type = 'Annual', unit = 'AGB') %>%
  left_join(trend_scales, by = c('type', 'unit')) %>%
  mutate( trend_unit = trend_scales$trend_unit[trend_scales$type == 'Annual']) %>%
  mutate( bt_trend = full_trend*trend_unit)

test_equiv <- trends %>%
  left_join(test_df, by = c('type', 'uname')) %>%
  select( type, uname, rate, full_trend, bt_trend.x, bt_trend.y)

plot( test_equiv$bt_trend.x , test_equiv$bt_trend.y )
abline(0,1)

#  test against true values ------ 
# load('data/analysis_data/agb.rda')
# load('data/analysis_data/allotments.rda')
# 
# agb_att$Annual
# agb_year_att$Annual
# 
# agb_models$Annual@frame %>%
#   distinct( ecogroup, uname) %>%
#   group_by( ecogroup) %>%
#   sample_n(2 ) %>%
#   select( uname ) %>%
#   left_join( agb$afgAGB ) %>%
#   ungroup() %>%
#   mutate(yhat2 = predict( agb_models$Annual, newdata = . )) %>%
#   select( ecogroup, uname, year, year2, value, value2, yhat2 ) %>%
#   mutate( yhat = exp(yhat2*agb_att$Annual$`scaled:scale` + agb_att$Annual$`scaled:center`) ) %>%
#   left_join(trends %>% filter( type == 'Annual')) %>%
#   left_join( test_equiv %>% select( uname, bt_trend.x, bt_trend.y), by = c('uname', 'type')) %>%
#   group_by(ecogroup,  uname) %>%
#   mutate( yhat_test = yhat[which.min(year)]*exp(bt_trend*(year-min(year))) -5 ) %>%
#   mutate( yhat_test.x = yhat[which.min(year)]*exp(bt_trend.x*(year-min(year)))) %>%
#   mutate( yhat_test.y = yhat[which.min(year)]*exp(bt_trend.y*(year-min(year)))) %>%
#   ggplot( aes( x = year, y = value, group = uname) ) +
#   geom_line() +
#   geom_line( aes( y = yhat, group = uname), color = 'black') +
#   #geom_line( aes( y = yhat_test, group = uname), color = 'blue', linetype = 2) +
#   geom_line( aes( y = yhat_test.x, group = uname), color = 'orange', linetype = 2) +
#   #geom_line( aes( y = yhat_test.y, group = uname), color = 'red', linetype = 1) +
#   facet_wrap( ~ ecogroup , scales = 'free_y') 
# 
# 
# 

