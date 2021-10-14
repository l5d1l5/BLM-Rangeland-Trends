# 
rm(list = ls())

library(tidyverse)

load('data/analysis_data/allotments.rda')
load('data/analysis_data/cover.rda')

source('code/analysis/parameters.R')
source('code/analysis/functions.R')

treeMod <- read_rds('output/TREE_cover_trend_model.rds')

TREE <- cover$TREE
treeMod2 <- update(treeMod , . ~ . - (year2|admin_st) - (year2|office_label), 
                   data = TREE, 
                   control = control_lmer)

treeMod3 <- update( treeMod2, . ~ . + (year2|ecogroup:office_label))
treeMod4 <- update( treeMod3, . ~ . + (year2|ecogroup:admin_st))

VarCorr(treeMod4)
VarCorr(treeMod3)
VarCorr(treeMod2)
VarCorr(treeMod)

emtrends(treeMod, ~ ecogroup, 'year2')
emtrends(treeMod2, ~ ecogroup, 'year2')
emtrends(treeMod3, ~ ecogroup, 'year2')
emtrends(treeMod4, ~ ecogroup, 'year2')

anova(treeMod4, treeMod)

rtest <- ranef(treeMod3)

change <- do.call(bind_rows,  cover )  %>% 
  filter( type != "LTR") %>%
  mutate( decade = decade_function(year)) %>% 
  filter( decade %in% c('90s', '10s')) %>%
  group_by( ecogroup, type, uname, decade ) %>% 
  summarise( avg = mean(value)) %>% 
  group_by( ecogroup, type, uname ) %>% 
  summarise( change = log(avg[decade == '10s']) - log(avg[decade == '90s']))  

change %>% 
  filter( ecogroup == 'Mediterranean California', type == 'TREE')  

cover$TREE  %>% 
  filter( ecogroup == 'Mediterranean California' ) %>% 
  mutate( decade = decade_function(year-1)) %>% 
  select( uname, year, value, value2, decade ) %>% 
  group_by( uname, decade ) %>%
  summarise( avg = mean(value2)) %>% 
  mutate( dec_label = factor(decade, levels = c('90s', '00s', '10s', '20s'))) %>%
  filter( dec_label != '00s') %>% 
  ggplot( aes( x = dec_label , y = avg, group = uname )) + 
  geom_point() + 
  geom_line()

treeMod@frame  %>% 
  filter( ecogroup == 'Mediterranean California' ) %>% 
  mutate( year = back_transform(year2,attributes(year2), log = F)) %>% 
  mutate( decade = decade_function(year-1)) %>% 
  select( uname, year, value2, decade ) %>% 
  group_by( uname, decade ) %>%
  summarise( avg = mean(value2)) %>% 
  mutate( dec_label = factor(decade, levels = c('90s', '00s', '10s', '20s'))) %>%
  filter( dec_label != '00s') %>% 
  ggplot( aes( x = dec_label , y = avg, group = uname )) + 
  geom_point() + 
  geom_line()

treeCover <- cover$TREE

treeCover %>% 
  filter( ecogroup == 'Mediterranean California') %>%
  mutate( yhat = predict( treeMod4, newdata = . , re.form= ~ (1|uname) + (1|ecogroup:office_label) + (1|ecogroup:admin_st))) %>% 
  ggplot( aes( x = year, y = value2 , group = uname)) + 
  geom_point() + 
  geom_line( aes( y = yhat, group = uname )) + 
  facet_wrap( ~ office_label)



change %>% 
  ungroup() %>% 
  mutate( cuts = cut(change, c(-10, 0, 20 ))) %>% 
  group_by( type) %>% 
  mutate( total = n()) %>% 
  group_by( type, cuts ) %>%  
  summarise( p = n()/unique(total)) %>% 
  pivot_wider(names_from = type, values_from = p )

change %>% 
  ungroup() %>% 
  mutate( dir = ifelse(change > 0, "increase", "decrease")) %>% 
  group_by( ecogroup, type) %>% 
  mutate( total = n() ) %>%
  group_by( ecogroup, type, dir) %>% 
  summarise( p = n()/unique(total)) %>%
  pivot_wider( names_from = dir, values_from = p) %>% 
  View()
  ggplot( aes( x = ecogroup, y = p, fill = dir )) + 
  geom_bar(stat='identity') + 
  facet_wrap( ~ type) + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0))
  pivot_wider(names_from = type, values_from = p )


cover_ranks <- do.call( bind_rows, cover )  %>% 
  filter( type != "LTR") %>% 
  mutate( decade = decade_function(year)) %>% 
  filter( decade %in% c('90s', '10s')) %>%
  group_by( ecogroup, type, uname, decade ) %>% 
  summarise( avg = mean(value)) %>% 
  select( ecogroup, uname, decade, type, avg) %>% 
  ungroup() %>%
  group_by( ecogroup, uname, decade) %>%
  mutate( rank = 6 - rank(avg)) %>% 
  arrange(ecogroup, uname, decade, type)
  
dominance_change <- cover_ranks %>% 
  ungroup() %>% 
  group_by( ecogroup, uname, decade ) %>%
  summarise( top = type[rank==1]) %>% 
  pivot_wider( names_from = decade, values_from = top ) 
  



lt_change %>% 
  ungroup() %>% 
  mutate( cuts = cut(change, c(-10, 0.6931, 20 ))) %>% 
  group_by( ecogroup) %>% 
  mutate( total = n() ) %>%
  group_by( ecogroup, cuts) %>% 
  summarise( n()/unique(total))
  
summarise( quantile( change , c(0.1, 0.5, 0.9 )))
  ggplot(aes( x = change )) + 
  geom_density()  
  facet_wrap( ~ ecogroup, scales = 'free_y')
