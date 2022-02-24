# Variance Plots 
rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(gridExtra )
library(ggpubr)
require(kableExtra)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

load('data/analysis_data/allotments.rda')

# Pixel level variance and means per allotment 

pixel_cover_trends <- read_csv('data/RAP_EE_exports/allotment_cover_trends.csv')
pixel_prod_trends <- read_csv('data/RAP_EE_exports/allotment_production_trends.csv')


pixel_stdDev <- pixel_cover_trends %>% 
  select(contains('std'), 'uname') %>% 
  pivot_longer(cols = contains('std'), names_to = 'type', values_to = 'stdDev') %>% 
  mutate(var = stdDev^2) %>% 
  group_by( type ) %>% 
  summarise( var = mean(var, na.rm = T)) %>% 
  mutate( stdDev = sqrt(var)) %>% 
  bind_rows(
    pixel_prod_trends %>% 
      select(contains('std'), 'uname') %>% 
      pivot_longer(cols = contains('std'), names_to = 'type', values_to = 'stdDev') %>% 
      mutate(var = stdDev^2) %>% 
      group_by( type ) %>% 
      summarise( var = mean(var, na.rm = T)) %>% 
      mutate( stdDev = sqrt(var))
  ) %>% 
  mutate( type2 = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree', 'Annual', 'Perennial')) %>% 
  mutate( type = factor(type2)) %>%
  mutate( unit = c('Cover', 'Cover', 'Cover', 'Cover', 'Cover', 'Production', 'Production')) %>% 
  mutate( grp = 'Pixel') %>% 
  select( type, grp, unit, var, stdDev) 

# Variance partitioning -------------------------------- 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_model_files <- cover_model_files[ !str_detect(cover_model_files, "WOODY") ] 

cover_models <- lapply(cover_model_files, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = names(my_colors))
names( cover_models ) <- types 

cover_att <- lapply( cover_models, function(x) attributes( x@frame$value2) )
cover_year_att <- lapply( cover_models, function(x) attributes( x@frame$year2) )

vc <- lapply(cover_models, VarCorr)
vc <- lapply(vc, data.frame )

vc <- mapply(x = types, y = vc, 
             function(x, y) {y$type <- x; return(y)}, SIMPLIFY = F)

cover_variance <- do.call( bind_rows, vc )
cover_variance$unit <- 'Cover'

# Production Variance 
agb_model_files <- dir(path = 'output', pattern = '.*_agb_trend_model.rds', full.names = T)
agb_models <- lapply(agb_model_files, read_rds)
types  <- c( str_extract( basename( agb_model_files), pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Perennial'))
names( agb_models ) <- types 
vc_agb <- lapply(agb_models, VarCorr )
vc_agb <- lapply( vc_agb, data.frame)

agb_att <- lapply( agb_models, function(x) attributes( x@frame$value2) )
agb_year_att <- lapply( agb_models, function(x) attributes( x@frame$year2) )

vc <- mapply(x = types, y = vc_agb, 
             function(x, y) {y$type <- x; return(y)}, SIMPLIFY = F)

agb_variance <- do.call( bind_rows, vc )
agb_variance$unit <- 'Production'

# Now back transform sd into units of log cover/log production per year
# trend_bt = trend_scaled*log_y_sd/year_sd 
trend_scales <- read_csv('data/temp/trend_scales.csv') # Output from script A8. 

trend_scales <- trend_scales %>% 
  mutate( unit = ifelse(unit == 'AGB', 'Production', unit ))

all_random_effects <- bind_rows(
  cover_variance, 
  agb_variance ) %>% 
  left_join(trend_scales ) %>% 
  filter( is.na(var2)) %>% 
  mutate( var1 = replace_na(var1, replace = '(Intercept)'))  %>% 
  mutate( variance = ifelse( var1 == 'year2', vcov*(trend_unit^2), vcov*(log_y_sd^2 ))) %>% 
  mutate( sd = ifelse( var1 == 'year2', sdcor*trend_unit, sdcor*log_y_sd )) %>% 
  mutate( variance2 = sd^2, 
          sd2 = sqrt( variance ))


# Bind pixel-level variance ------------------------------------------ # 
var_cols <- scales::hue_pal()(3)


final_variance_table <- 
  all_random_effects %>% 
  filter( var1 == 'year2') %>% 
  select( type, unit, grp, var1, par, variance, sd ) %>% 
  mutate( grp2 = factor(grp)) %>%
  mutate( grp2 = factor(grp2, labels = c('Office', 'Allotment'))) %>% 
  mutate( grp = grp2 ) %>% 
  rename( var = variance, stdDev = sd) %>% 
  select( type, grp, unit, var, stdDev ) %>% 
  bind_rows(
    pixel_stdDev
  ) %>% 
  mutate( type_unit = paste( type, unit, sep = ' ')) %>% 
  mutate( isPix = ifelse(grp == 'Pixel', "A) Within Allotment/Pixel-scale", "B) Between Allotment (Random Effects)")) %>% 
  mutate( isPix = factor( isPix , levels = c('A) Within Allotment/Pixel-scale', 'B) Between Allotment (Random Effects)'), ordered = T))


final_variance_table %>%
  ggplot( aes( x = type_unit, y = var, fill = grp )) + 
  geom_bar(stat = 'identity')   + 
  facet_wrap( ~ isPix, ncol  = 2 ) + 
  ylab( "Trend Variance") + 
  theme_bw() + 
  scale_fill_manual(values = var_cols, breaks = c('Allotment', 'Office'), name = 'Scale') + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text( angle = -50, hjust = 0), 
        strip.text = element_text(hjust = 0)) + 
  ggsave( filename = 'output/figures/Fig_S4_trend_variance.png',
          width = 6.5, height = 4, units = 'in', dpi = 600)


final_variance_table %>% 
  group_by(type_unit) %>% 
  arrange( var ) %>% 
  mutate( order = row_number()) %>%
  arrange( type_unit, order ) %>% 
  select( type_unit, grp, order , var ) %>% View 



