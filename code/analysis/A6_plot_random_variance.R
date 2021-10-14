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

pixel_cover_trends <- read_csv('data/RAP_EE_exports/afg_resid_samples.csv') %>% 
  bind_cols( read_csv('data/RAP_EE_exports/bg_resid_samples.csv')) %>% 
  bind_cols( read_csv('data/RAP_EE_exports/pfg_resid_samples.csv')) %>% 
  bind_cols( read_csv('data/RAP_EE_exports/shr_resid_samples.csv')) %>%
  bind_cols( read_csv('data/RAP_EE_exports/tree_resid_samples.csv'))  %>% 
  select( AFG, BG, PFG, SHR, TREE)

pixel_prod_trends <- read_csv('data/RAP_EE_exports/afg_prod_resid_samples.csv') %>% 
  bind_cols( read_csv('data/RAP_EE_exports/pfg_prod_resid_samples.csv')) %>% 
  select( AFG, PFG) 

pixel_cover_trends %>% 
  mutate( grp = 'pixel', var1 = 'year2', unit  = 'Cover') %>% 
  pivot_longer(AFG:TREE, names_to = 'type', values_to = 'resid') %>% 
  bind_rows( pixel_prod_trends %>% 
               mutate( grp = 'pixel', var1 = 'year2', unit = 'Production') %>% 
               pivot_longer(AFG:PFG, names_to = 'type', values_to = 'resid')) %>% 
  write_rds('data/temp/pixel_residual_trends.rds')

pixel_cover_trends <- 
  pixel_cover_trends %>% 
  summarise_all( .funs = list( sd)) %>% 
  pivot_longer(names_to = 'type', values_to = 'sd', AFG:TREE) %>%
  mutate( grp = "pixel",
          var1 = "year2", 
          unit = "Cover")

pixel_prod_trends <- 
  pixel_prod_trends %>% 
  summarise_all( .funs = list( sd)) %>% 
  pivot_longer(names_to = 'type', values_to = 'sd', AFG:PFG) %>%
  mutate( grp = "pixel",
          var1 = "year2", 
          unit = "Production")

# Variance partitioning -------------------------------- 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_model_files, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Total', 'Perennial', 'Shrub', 'Tree'))
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
types <- factor(types, labels = c('Annual', 'Total', 'Perennial'))
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
pixel_level_trends <- 
  pixel_cover_trends %>% 
    mutate( type = factor( type, labels = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'))) %>% 
  bind_rows(
    pixel_prod_trends %>% 
              mutate( type = factor(type, labels = c('Annual', 'Perennial')))
    )

all_random_effects <- 
  all_random_effects %>% 
  select( grp, var1, type, unit, sd, variance ) %>% 
  bind_rows(
    pixel_level_trends %>% 
      ungroup() %>% 
      select( grp, var1, type, unit, sd)
  ) %>% 
  mutate( Scale = factor( grp, levels = c('ecogroup:office_label', 'uname', 'pixel'), ordered = T)) %>% 
  mutate( Scale = factor( Scale, labels = c('Office', 'Allotment', 'Pixel')))



all_random_effects %>% 
  filter( type != 'Total', var1 == 'year2' ) %>% 
  mutate( unit = ifelse( unit == 'Production', 'Prod.', unit )) %>% 
  unite( 'type', c(type, unit ) , sep = '\n')  %>% 
  ggplot( aes( fill = Scale, x = type, y = sd )) + 
  geom_bar( stat = 'identity', position = position_dodge()) + 
  theme_bw() + 
  scale_y_continuous(name = 'Std. Dev. of Residual Trends') + 
  scale_fill_manual(values = error_scale_colors) +
  theme(axis.title.x = element_blank()) + 
  ggsave( filename = 'output/figures/Fig_5_trend_sd.png',
          width = 6, height = 4, units = 'in', dpi = 300)


all_random_effects %>% 
  filter( type != 'Total', var1 == 'year2' ) %>% 
  mutate( unit = ifelse( unit == 'Production', 'Prod.', unit ))  %>% 
  select(  Scale, type, unit, sd) %>% 
  pivot_wider(names_from = Scale, values_from = sd ) %>% 
  mutate( pixel_factor = Pixel / Allotment )

# Sanity check on transformation 
test_rf <- ranef(cover_models$Annual)
var( test_rf$uname$year2 )
var( test_rf$`ecogroup:office_label`$year2 )

uname_ranef <- VarCorr(cover_models$Annual)  %>% data.frame() %>% 
  filter( grp == 'uname', var1 == 'year2') %>% 
  pull( vcov)

trend_unit <- trend_scales$trend_unit[ trend_scales$type == 'Annual' & trend_scales$unit == 'Cover' ] 
pixel_level <- read_csv('data/RAP_EE_exports/afg_resid_samples.csv')
sd(pixel_level$AFG)

sqrt( var( test_rf$uname$year2*trend_unit) )
sqrt( var(test_rf$uname$year2)*(trend_unit)^2 )
sqrt( uname_ranef*(trend_unit^2))


all_random_effects %>% 
  filter( type != 'Total', var1 == 'year2' ) %>% 
  mutate( unit = ifelse( unit == 'Production', 'Prod.', unit )) %>% 
  unite( 'type', c(type, unit ) , sep = '\n')  %>% 
  arrange( Scale, type ) %>% 
  filter( type == 'Annual\nCover')


# Compare sd of ecogroup trends 
lapply( cover_models, get_ecogroup_trends) %>% 
  do.call(rbind , . ) %>% 
  data.frame() %>%
  mutate( type = row.names(.)) %>% 
  pivot_longer(`AZ.NM.Highlands`:`Warm.Deserts`, 'ecogroup', 'value') %>%
  mutate( unit = 'Cover') %>%
  left_join(trend_scales, by  = c('type', 'unit')) %>% 
  mutate( trend = value*trend_unit ) %>%
  group_by( type, unit ) %>%
  summarise( sd( trend))
  

