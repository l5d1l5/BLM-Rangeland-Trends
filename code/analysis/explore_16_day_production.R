rm(list = ls())
library(tidyverse)

info <- read_rds('data/basic_allotment_info.rds')
annualAGB <- read_rds('data/allotment_data_long.rds') %>% 
  filter( type %in% c('herbaceousAGB')) %>%
  filter( uname == 5892 ) 

NPP_sixteen <- read_csv('data/MT_allotment_data/MT_allotment_16_day_NPP_by_feature.csv') %>% 
  select( - `system:index`, -.geo) %>%
  filter( uname == 5892 )

NPP_sixteen <- 
  NPP_sixteen %>% 
  rowwise() %>% 
  mutate( totalNPP = afgNPP + pfgNPP ) %>% 
  pivot_longer(cols = c(afgNPP, pfgNPP, totalNPP), names_to = "type") %>%
  mutate( value = replace_na( value , 0) ) %>%
  group_by( type, uname, year ) %>% 
  arrange( type, uname, year, doy ) %>% 
  mutate( cmltvNPP = cumsum(value))

# plot total 16 production by total annual production 
NPP_sixteen %>% 
  filter( type == 'totalNPP') %>%
  group_by( year, uname, type ) %>% 
  summarise( annual_total = sum( value )) %>% 
  left_join(annualAGB, by = c('year', 'uname'))  %>% 
  ggplot( aes( x = annual_total, y = value )) + 
  geom_point() 

NPP_sixteen %>%
  filter( year %in% c(1990, 2000, 2010, 2019)) %>% 
  ggplot(aes( color = factor( year), group = year, x = doy, y  = value )) + 
  geom_point() + 
  geom_path() + 
  facet_wrap( ~ type )
