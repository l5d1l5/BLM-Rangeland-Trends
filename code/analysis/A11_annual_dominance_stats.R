# Quantify major changes 
rm(list = ls() )
library(tidyverse)

source('code/analysis/parameters.R')

annual_data <- read_rds('data/temp/annual_data.rds')
allotments <- read_rds('data/temp/allotment_info.rds')

annual_data <- annual_data %>% 
  left_join(allotments, by = 'uname')

decades <- 
  data.frame( year = 1980:2030 ) %>% 
  mutate( decade = cut( year , c(1980, 1990, 2000, 2010, 2020, 2030))) %>% 
  mutate( decade = factor(decade, labels = c('1980s', '1990s', '2000s', '2010s', '2020s'))) 

# Annual Production ------------------------------------------- # 

annual_prod_dominance <- annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'production', type != 'herbAGB') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecogroup) %>% 
  summarise(avg = mean(value, na.rm = T))  %>% 
  pivot_wider(names_from = type, values_from = avg) %>% 
  mutate( annual_dom = afgAGB > pfgAGB ) %>%
  group_by( decade, ecogroup, annual_dom ) %>%
  summarise( n = n() )  %>% 
  group_by( decade, ecogroup ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecogroup, decade ) %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecogroup, Dominance , n ) %>% 
  pivot_wider( names_from = Dominance, values_from = n , values_fill = 0) %>%
  arrange( ecogroup, decade ) %>%
  mutate( n_total = `more perennials` + `more annuals`) %>% 
  mutate( frac_more_perennials = `more perennials`/n_total ) %>% 
  mutate( frac_more_annuals = `more annuals`/n_total )

annual_prod_dominance %>% 
  write_csv('output/tables/annual_prod_dominance.csv')

annual_prod_dominance %>% 
  filter( ecogroup != 'Marine West Coast Forest') %>% 
  ggplot( aes( x = decade, y = frac_more_annuals )) + 
  facet_wrap( ~ ecogroup ) + 
  geom_bar(stat = 'identity') + 
  geom_text( aes( label = paste0( `more annuals`,'(', round( frac_more_annuals, 2)*100, '%)')), nudge_y = 0.08, size = 3) + 
  ylab( 'Fraction of Allotments') + 
  xlab( 'Decade') + 
  theme_bw() + 
  ggtitle('Annual Production > Perennial Production') + 
  ggsave( 'output/figures/Fig_Supp_annual_gt_perennial_by_decade.png', 
          height = 6, width = 8, units = 'in', dpi = 600)

# Annual Cover ---------------------------------------- 
annual_cover_dominance <- annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecogroup) %>% 
  summarise(avg = mean(value, na.rm = T))  %>% 
  pivot_wider(names_from = type, values_from = avg) %>%
  mutate( annual_dom = AFGC > PFGC ) %>%
  group_by( decade, ecogroup, annual_dom ) %>%
  summarise( n = n() )  %>% 
  group_by( decade, ecogroup ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecogroup, decade ) %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecogroup, Dominance , n ) %>% 
  pivot_wider( names_from = Dominance, values_from = n , values_fill = 0) %>%
  arrange( ecogroup, decade ) %>%
  mutate( n_total = `more perennials` + `more annuals`) %>% 
  mutate( frac_more_perennials = `more perennials`/n_total ) %>% 
  mutate( frac_more_annuals = `more annuals`/n_total )

annual_cover_dominance %>% 
  write_csv('output/tables/annual_cover_dominance.csv')

annual_cover_dominance %>% 
  filter( ecogroup != 'Marine West Coast Forest') %>% 
  ggplot( aes( x = decade, y = frac_more_annuals )) + 
  facet_wrap( ~ ecogroup ) + 
  geom_bar(stat = 'identity') + 
  geom_text( aes( label = paste0( `more annuals`,'(', round( frac_more_annuals, 2)*100, '%)')), nudge_y = 0.08, size = 3) + 
  ylab( 'Fraction of Allotments') + 
  xlab( 'Decade') + 
  theme_bw() + 
  ggtitle('Annual Cover > Perennial Cover')  + 
  ggsave( 'output/figures/Fig_Supp_annual_cover_gt_perennial_by_decade.png', 
          height = 6, width = 8, units = 'in', dpi = 600)

# Trees ---------------------------------------------------
tree_bins <-
  annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover', type == 'TREE') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecogroup) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate( tree_bins = cut( avg, c(0, 1, 2, 5, 10, 20, 100))) %>% 
  mutate( tree_bins = factor(tree_bins, labels = c('0-1', '1-2', '2-5', '5-10', '10-20', '>20'))) %>% 
  group_by( decade, ecogroup, tree_bins ) %>%
  summarise( n = n() )  %>% 
  group_by( decade, ecogroup ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecogroup, decade , tree_bins) 

tree_bins %>% 
  arrange( ecogroup, tree_bins, decade )  %>% 
  write_csv('output/tables/tree_cover_by_decade.csv')

tree_bins %>% 
  filter( ecogroup != 'Marine West Coast Forest') %>% 
  filter( !is.na(tree_bins )) %>% 
  ggplot( aes( x = decade, y = frac, fill = tree_bins )) + 
  geom_bar(stat = 'identity') + 
  facet_wrap( ~ ecogroup )   + 
  scale_fill_brewer(type = 'seq',palette = 2, name = 'Tree Cover Class (%)')  + 
  ylab( 'Fraction of Allotments') + 
  xlab( 'Decade') + 
  theme_bw() + 
  ggtitle('Average Tree Cover by Decade') + 
  ggsave( 'output/figures/Fig_Supp_tree_cover_by_decade.png', 
          height = 6, width = 8, units = 'in', dpi = 600)

