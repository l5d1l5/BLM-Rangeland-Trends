# Compare trends trends calculated from allotment-level average cover vs. 
# trends calculated at the pixel-scale and then averaged across the allotment. 
# trends1 = trends calculated from allotment-level avgs. 
# trends2 = trends calculated from pixels to allotment-level. 

rm(list = ls())
library(tidyverse)


# Trends1: ---------------------------------------------------------- # 
trend_scales <- read_csv('data/temp/trend_scales.csv')
alias <- c('AFG_' = 'Annual_', 'AFGC_' = 'Annual_', 'afgNPP' = 'Annual',
           'BG' = 'Bare', 
           'PFG_' = 'Perennial_', 'PFGC_' = 'Perennial_', 'pfgNPP' = 'Perennial', 
           'SHR' = 'Shrub', 'TREE' = 'Tree')

trend_files <- dir( path = 'output', pattern = '*_group_trends.csv', full.names = T)
types <- str_extract(string =  basename( trend_files ), pattern = '.*(?=_group_trends)') # look ahead 
trends1 <- lapply( trend_files, read_csv  )
trends1 <- mapply( types, trends1 , FUN = function(x, y) { y$type <- x ; return( y )}, SIMPLIFY = F) %>% 
  do.call(rbind, . ) %>% 
  select( uname, full_trend, type) %>% 
  pivot_wider(id_cols = 'uname', names_from = type, values_from = full_trend)

names( trends1 )[-1] <- paste(names(trends1)[-1], 'trend1', sep = '_')
names( trends1 ) <- str_replace_all( names( trends1) , c( 'agb' = 'AGB', 'cover' = 'Cover'))

trends1 <- trends1 %>% 
  pivot_longer( 'AFG_AGB_trend1':'TREE_Cover_trend1') %>% 
  mutate( name = str_replace_all( name, alias) ) %>% 
  separate( name , c('type', 'unit', 'trend'))  %>% 
  left_join(trend_scales) %>% 
  mutate( trend_rescaled = value*trend_unit) %>%
  unite('type', type:trend) %>% 
  pivot_wider(id_cols = uname, names_from = type, values_from = trend_rescaled)
  
# Trends2: ---------------------------------------------------------- # 
allotments <- read_csv( 'data/temp/allotment_info.csv')
cover_trends2 <- read_csv( 'data/RAP_EE_exports/allotment_cover_trends.csv') 
prod_trends2  <- read_csv( 'data/RAP_EE_exports/allotment_production_trends.csv')

names( cover_trends2) <- str_replace(names(cover_trends2), 'scale_', 'Cover_')
names( cover_trends2) <- str_replace(names(cover_trends2), '_mean', '_trend2')

names( prod_trends2)  <- str_replace(names(prod_trends2), 'scale_', 'AGB_')
names( prod_trends2)  <- str_replace(names(prod_trends2), '_mean', '_trend2')

names( cover_trends2) <- str_replace_all(names(cover_trends2), alias)
names( prod_trends2) <- str_replace_all( names( prod_trends2), alias)

# ------------------------------------------------------------ # 
trends <- allotments %>% 
  filter( ecogroup != 'Marine West Coast Forest') %>% 
  left_join(cover_trends2, by = 'uname') %>% 
  left_join(prod_trends2, by = 'uname') %>% 
  left_join(trends1, by= 'uname') 

trends %>% 
  ggplot( aes( x  = Annual_Cover_trend2, y = Annual_Cover_trend1 )) + 
  geom_smooth(method = 'lm', se = F) + 
  geom_point(alpha = 0.1) + 
  facet_wrap( ~ ecogroup )

trends %>% 
  ggplot( aes( x  = Perennial_Cover_trend2, y = Perennial_Cover_trend1 )) + 
  geom_point( alpha = 0.1) + 
  geom_smooth(method = 'lm', se = F)

trends %>% 
  ggplot( aes( x  = Bare_Cover_trend2, y = Bare_Cover_trend1 )) + 
  geom_point( alpha = 0.1) + 
  geom_smooth(method = 'lm', se = F) + 
  geom_abline( aes( intercept = 0, slope = 1 ))

trends %>% 
  ggplot( aes( x  = Shrub_Cover_trend2, y = Shrub_Cover_trend1 )) + 
  geom_point( alpha = 0.1) + 
  geom_smooth(method = 'lm', se = F) + 
  geom_abline( aes( intercept = 0, slope = 1 ))

trends %>% 
  ggplot( aes( x  = Tree_Cover_trend2, y = Tree_Cover_trend1 )) + 
  geom_point( alpha = 0.1) + 
  geom_smooth(method = 'lm', se = F) + 
  geom_abline( aes( intercept = 0, slope = 1 ))

trends %>% 
  ggplot( aes( x  = Annual_AGB_trend2, y = Annual_AGB_trend1 )) + 
  geom_point( alpha = 0.1) + 
  geom_smooth(method = 'lm', se = F) + 
  geom_abline( aes( intercept = 0, slope = 1 )) + 
  facet_wrap( ~ ecogroup )

trends %>% 
  ggplot( aes( x  = Perennial_AGB_trend2, y = Perennial_AGB_trend1 )) + 
  geom_point( alpha = 0.1) + 
  geom_smooth(method = 'lm', se = F) + 
  geom_abline( aes( intercept = 0, slope = 1 )) + 
  facet_wrap( ~ ecogroup )

# plot allotment trend variation by ecoregion 
trend_stats <- 
  trends %>% 
  pivot_longer(cols = contains( c("Cover", "AGB"))) %>% 
  group_by( ecogroup, name ) %>% 
  summarise( avg = mean( value, na.rm = T)) %>% 
  separate( name, into = c('type', 'unit', 'stat'))

trend_stats %>%  
  filter( stat == 'stdDev') %>% 
  ggplot( aes( x = ecogroup, y = avg )) + 
  geom_bar(stat = 'identity')  + 
  facet_grid( type ~ unit ) + 
  coord_flip()

trend_stats %>%  
  filter( stat == 'count') %>% 
  ggplot( aes( x = ecogroup, y = avg )) + 
  geom_bar(stat = 'identity')  + 
  facet_grid( type ~ unit ) + 
  coord_flip()

trend_stats %>% 
  filter( stat %in% c('trend1', 'trend2')) %>% 
  ggplot( aes( x = ecogroup, y = avg, fill = stat )) + 
  geom_bar(stat = 'identity', position = position_dodge())  + 
  facet_grid( type ~ unit )

# Compare random effect sddev 
source('code/analysis/parameters.R')
library(lme4)
fls <- dir(path = 'output', pattern = 'trend_model.rds', full.names = T)
types <- str_extract( basename( fls), '[A-Z]+')
units <- str_extract( basename(fls), 'cover|agb')

random_effects <- mapply(x =  fls, y = types, z = units,
                         FUN =  function( x, y , z ){ 
                              VarCorr( read_rds( x ) ) %>% 
                              data.frame() %>% 
                              mutate( type = y, unit = z)} , SIMPLIFY = F)

random_effects <- do.call( rbind, random_effects) %>% 
  filter( var1 == 'year2') %>% 
  mutate( type = str_replace_all(type, c('AFG'= 'Annual', 
                                         'BG' = 'Bare', 
                                         'PFG' = 'Perennial', 
                                         'SHR' = 'Shrub', 
                                         'TREE'  = 'Tree'))) %>% 
  mutate( unit = str_replace_all( unit, c('agb' = 'AGB', 'cover' = 'Cover'))) %>% 
  left_join(trend_scales) %>% 
  mutate( sd = sdcor*trend_unit )


trend_sd <- cover_trends2 %>% 
  select( contains('stdDev')) %>% 
  pivot_longer(cols = Annual_Cover_stdDev:Tree_Cover_stdDev) %>% 
  separate( name, c("type","unit", "stat")) %>%
  bind_rows(
    prod_trends2 %>% 
    select( contains('stdDev')) %>% 
    pivot_longer(cols = Annual_AGB_stdDev:Perennial_AGB_stdDev) %>% 
    separate( name, c("type","unit", "stat"))
    ) %>% 
  group_by( type, unit, stat ) %>% 
  summarise( sd = mean( value , na.rm = T)) %>% 
  mutate( grp = 'Pixel')  %>% 
  bind_rows(
    random_effects %>% select( grp, type, unit, sd) ) 

trend_sd %>% 
  ungroup() %>% 
  mutate( grp2 = factor(grp, labels = c('Office', 'Pixel', 'Allotment'))) %>% 
  mutate( grp2 = factor(grp2, levels = c('Office', 'Allotment', 'Pixel'), ordered = T)) %>%
  mutate( unit = str_replace(unit, 'AGB', 'Prod.')) %>% 
  filter( !is.na(type), !is.na(unit)) %>% 
  mutate( type2 = paste( type, unit, sep = '\n')) %>% 
  ggplot( aes( x = type2, y = sd , fill = grp2)) +
  geom_bar( stat = 'identity', position  = position_dodge()) +
  scale_fill_manual( name = 'Scale', values = error_scale_colors)  + 
  theme_bw() + 
  ylab( 'Trend Standard Deviation') + 
  xlab( 'Vegetation Trend') + 
  ggsave( filename = 'output/figures/Fig_5_trend_sd.png',
        width = 6, height = 4, units = 'in', dpi = 300)



