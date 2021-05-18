rm(list = ls())

library(tidyverse)
library(dbplyr)
library(ggridges)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

allotment_info <- tbl( con, 'allotments')

allotment_info <- tbl(con, 'allotments') %>% 
  left_join( 
    allotment_info %>% 
      group_by( na_l3name) %>% 
      summarise(num_allots = n_distinct(uname))
  ) %>% 
  mutate( Region3_Label = paste( na_l3name, 'n =',  num_allots )) %>% 
  select( - num_allots ) %>% 
  left_join( 
    allotment_info %>% 
      group_by( na_l2name) %>% 
      summarise(num_allots = n_distinct(uname))
  )  %>%
  mutate( Region2_Label = paste( na_l2name, 'n =',  num_allots )) %>% 
  select( - num_allots ) %>% 
  left_join( 
    allotment_info %>% 
      group_by( na_l1name) %>% 
      summarise(num_allots = n_distinct(uname))
  )  %>%
  mutate( Region1_Label = paste( na_l1name, 'n =',  num_allots )) 
  
annual_data  <- tbl(con, 'annual_data')

decadal_stats <- 
  annual_data %>% 
  mutate( decade = as.integer( floor( year/10)*10 )) %>%
  group_by( uname, type, decade ) %>%
  summarise( avg = mean(value, na.rm = T ), 
             nyears = count(!is.na(value)) ) %>% 
  mutate( decgroup = paste0( as.character( decade), 's' )) 
  
cover_dist_by_decade <- 
  decadal_stats %>%
  left_join(allotment_info) %>% 
  filter( type %in% c('AFGC', 'BG', 'PFGC', 'LTR', 'TREE', 'SHR')) %>% 
  filter( nyears > 0 )

cover_dist_by_decade %>% 
  ggplot( aes( y = decgroup, fill = decgroup, x = avg, color = decgroup)) + 
  geom_density_ridges(alpha = 0.5, scale = 3) + 
  facet_grid( Region1_Label ~ type, scales = 'free')

allotment_info %>% head %>% View

#
decadal_stats %>%
  left_join(allotment_info) %>% 
  filter( type == 'burned') %>% 
  ggplot( aes( x = na_l1name, y = avg , group = decgroup , fill = decgroup )) + 
  geom_boxplot( position = position_dodge() )

decadal_stats %>% 
  left_join(allotment_info) %>% 
  filter( type %in% c('AFGC', 'BG', 'PFGC', 'LTR', 'TREE', 'SHR')) %>% 
  ggplot( aes( x = type , y = avg, fill = decgroup) ) + 
  stat_summary(fun = 'median', geom = 'bar', position = position_dodge()) + 
  facet_wrap( ~ parent_name  ) 

annual_data %>% 
  filter( type %in% c('AFGC', 'PFGC', 'BG')) %>% 
  left_join(allotment_info) %>% 
  arrange( adm_ofc_cd, type,  uname, year) %>% 
  ggplot( aes( x = year, y = value) ) + 
  geom_line(alpha = 0.05, aes( group = uname )) + 
  stat_summary(fun = 'mean', geom = 'line', color = 'lightblue', alpha = 0.9) + 
  stat_summary(fun = function(x) quantile(x, 0.75), 
               color = 'red', 
               geom = 'line', alpha = 0.5) + 
  stat_summary(fun = function(x) quantile(x, 0.25), 
               color = 'purple', geom = 'line', alpha = 0.5) + 
  #geom_smooth(aes( group = 1), method = 'lm', se = F, alpha = 0.5, size = 0.5) + 
  facet_grid(type ~ Region2_Label, scales = 'free_y' )

annual_data %>% 
  filter( type %in% c('AFGC', 'PFGC', 'SHR', 'TREE', 'BG')) %>% 
  left_join(allotment_info) %>% 
  arrange( adm_ofc_cd, type,  uname, year) %>%
  #mutate( value = ifelse( type == 'TREE', log(value), value )) %>% 
  #filter( !is.infinite(value)) %>% 
  ggplot( aes( x = year, y = value) ) + 
  #geom_line(alpha = 0.05, aes( group = uname )) + 
  stat_summary(fun.max = function(x) quantile(x, 0.75),
               fun.min = function(x) quantile(x, 0.25), 
               fill = 'blue', 
               geom = 'ribbon', alpha = 0.2) + 
  #stat_summary(fun = function(x) quantile(x, 0.25), 
  #             color = 'b', geom = 'line', alpha = 0.5) + 
  stat_summary(fun = 'mean', geom = 'line', color = 'darkblue', alpha = 0.5) + 
  geom_smooth(aes( group = 1), method = 'lm', 
              se = F, alpha = 0.2, size = 0.5, color = 'black') + 
  facet_grid(type ~ Region3_Label, scales = 'free_y' ) 

# NPP 
production_dist_by_decade <- 
  decadal_stats %>%
  left_join(allotment_info) %>% 
  filter( type %in% c('afgAGB', 'pfgAGB')) %>% 
  ggplot( aes( y = decgroup, fill = decgroup, x = avg, color = decgroup)) + 
  geom_density_ridges(alpha = 0.5, scale = 3) + 
  facet_grid( Region3_Label ~ type, scales = 'free')

production_dist_by_decade

annual_data %>% 
  left_join(allotment_info) %>% 
  filter( type %in% c('afgAGB', 'pfgAGB')) %>% 
  arrange( adm_ofc_cd, type,  uname, year) %>%
  #mutate( value = ifelse( type == 'TREE', log(value), value )) %>% 
  #filter( !is.infinite(value)) %>% 
  ggplot( aes( x = year, y = value) ) + 
  #geom_line(alpha = 0.05, aes( group = uname )) + 
  stat_summary(fun.max = function(x) quantile(x, 0.75),
               fun.min = function(x) quantile(x, 0.25), 
               fill = 'blue', 
               geom = 'ribbon', alpha = 0.2) + 
  #stat_summary(fun = function(x) quantile(x, 0.25), 
  #             color = 'b', geom = 'line', alpha = 0.5) + 
  stat_summary(fun = 'mean', geom = 'line', color = 'darkblue', alpha = 0.5) + 
  geom_smooth(aes( group = 1), method = 'lm', 
              se = F, alpha = 0.2, size = 0.5, color = 'black') + 
  facet_grid(type ~ Region3_Label, scales = 'free_y' ) + 
  scale_y_log10()


burn_info <- 
  annual_data %>% 
  filter( type %in% c('burned')) %>% 
  left_join(allotment_info) %>% 
  mutate( burned_acres = as.numeric( value*acres )) %>% 
  mutate( burned = value > 0)

burn_summary <- 
  burn_info %>% 
  group_by( Region3_Label, year ) %>% 
  ungroup() %>%
  group_by( Region3_Label,  na_l3name, year ) %>%
  mutate( burned = as.integer( burned )) %>% 
  summarise( frac_with_burn = mean(burned), 
             avg_prop_burn = mean(value ), 
             total_acre_burned = sum(burned_acres), 
             prop_acres_burned = as.numeric( sum(burned_acres)/sum(acres) )) 


burn_summary %>% 
  filter( na_l3name %in% c('Middle Rockies', 'Northwestern Glaciated Plains', 
                           'Northwestern Great Plains')) %>%
  ggplot( aes( x = year, y = prop_acres_burned) )+ 
  geom_bar(stat = 'identity') + 
  facet_wrap( ~ Region3_Label,  nrow = 1) + 
  scale_y_continuous(name = 'Proportion burned (acres burned/total acres)')


burn_summary %>% 
  filter( na_l3name %in% c('Middle Rockies', 'Northwestern Glaciated Plains', 
                           'Northwestern Great Plains')) %>%
  ggplot( aes( x = year, y = frac_with_burn)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap( ~ Region3_Label,  nrow = 1) + 
  scale_y_continuous(name = 'Fraction of allotments burned')


# decadal_stats %>%
#   left_join(basic_allotment_info ) %>% 
#   filter( type %in% c('burned')) %>% View
#   filter( !nyears == 0) %>% head
#   ggplot( aes( y = decade, fill = decade, x = avg, color = decade)) + 
#   geom_density_ridges(alpha = 0.5, scale = 3) + 
#   facet_grid( NA_L3NAME , scales = 'free')


