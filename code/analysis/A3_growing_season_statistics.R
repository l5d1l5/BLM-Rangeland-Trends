rm(list = ls())
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )
require( rpostgis )
library(sf)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

# Constants
lbs_per_acre_min <- 5 
sample_per_year_min <- 22 
years_of_data_min <- 29

allotments <- tbl( con, 'allotments')
npp_16 <- tbl(con, 'npp_16')

temp <- npp_16 %>% 
  select( uname, year, doy, afg, pfg) %>% 
  mutate( herb_npp = afg + pfg ) %>% 
  filter( !is.na(herb_npp))  %>% 
  group_by( uname, year ) %>% 
  mutate( npp_total = sum(herb_npp, na.rm = T)) %>%  # total for the year 
  filter( npp_total > lbs_per_acre_min, n() >= sample_per_year_min) %>%  # Greater than 5 lbs per acre and greater than 22 sample each year
  ungroup() 

select_allots <- 
  temp %>% 
  group_by( uname, year) %>% 
  distinct( uname, year ) %>%
  group_by( uname ) %>%
  summarise(n_years = n()) %>% 
  filter( n_years >= years_of_data_min ) # only allotments with at least X years of data 

temp <- select_allots %>% 
  select( uname ) %>%
  left_join( temp ) %>% 
  group_by( uname, year ) %>% 
  window_order( uname, year, doy ) %>% 
  mutate(herb_npp_cmsm = cumsum( herb_npp ) ) %>% 
  ungroup() 

critical_stats <- temp %>% 
  select( uname, year ) %>%
  ungroup() %>% 
  distinct() %>%
  mutate( crit_50 = 0.5, crit_25 = 0.25, crit_75 = 0.75 ) %>% 
  pivot_longer(starts_with('crit'), names_to = 'crit', values_to = 'crit_val') %>% 
  select( uname, year, crit_val) %>% 
  left_join( temp, by = c('uname', 'year')) %>%
  mutate( herb_npp_crit = npp_total*crit_val) %>% 
  group_by( uname, year , crit_val ) %>% 
  mutate( above = herb_npp_cmsm > herb_npp_crit ) %>% 
  group_by( uname, year, crit_val, above ) %>% 
  filter( (above & doy == min(doy)) |
            (!above & doy == max(doy)) ) %>% 
  mutate( above = ifelse(above, 'hi', 'lo' )) %>% 
  pivot_longer( c(doy, herb_npp_cmsm), names_to = 'type', values_to = 'value') %>% 
  mutate( type = paste( type, above, sep = '_')) %>%
  ungroup() %>%
  select( uname, year, crit_val, herb_npp_crit, type, value ) %>%
  pivot_wider(names_from = type, values_from = value ) %>%
  mutate( m = (herb_npp_cmsm_hi - herb_npp_cmsm_lo)/(doy_hi - doy_lo)) %>% 
  mutate( B = herb_npp_cmsm_hi - m*doy_hi) %>% 
  mutate( doy_crit = (herb_npp_crit - B)/m ) 
  

critical_stats %>% 
  collect() %>% 
  write_rds(file = 'data/temp/allotment_growing_season.rds')


# Make example plot --------------------- # 

plotting_example <- 
  critical_stats %>%
  filter( uname %in% c(1)) %>% 
  collect() %>% 
  mutate( label2 = paste0( crit_val*100 , '%')) %>%
  mutate( crit_val = factor(crit_val)) %>% 
  mutate( label = round(doy_crit)) 

example_ts <- tbl(con, 'npp_16') %>% 
  mutate( herb_npp = afg + pfg ) %>%
  select( uname, year, doy, herb_npp ) %>% 
  filter( uname %in% c(1)) %>% 
  group_by( uname, year ) %>% 
  window_order( uname, year, doy ) %>% 
  filter( !is.na(herb_npp )) %>%
  mutate( herb_npp_cmsm = cumsum( herb_npp)) %>% 
  collect() 

p1 <- example_ts %>% 
  filter( year > 2014, year < 2020) %>% 
  ggplot( aes( x = doy, y = herb_npp_cmsm, group = uname )) + 
  geom_line() + 
  geom_point(data = plotting_example %>% 
               filter( year > 2014, year < 2020), 
             aes( x= doy_crit, y = herb_npp_crit, color = crit_val)) + 
  geom_hline(aes(yintercept = 0), color = 'darkgray') + 
  scale_x_continuous(name = 'Day of Year', breaks = c(0, 300)) + 
  scale_y_continuous(name = 'Cumulative NPP', limits = c(-20,NA)) + 
  scale_color_discrete(guide = F) + 
  theme_bw() +
  coord_cartesian(clip = 'off') + 
  geom_segment(data = plotting_example %>% filter( year %in% c(2017, 2018)), 
               aes( x = doy_crit, xend = doy_crit, y = herb_npp_crit, yend = 0), linetype =2, size = 0.5 , alpha = 0.5) + 
  geom_segment(data = plotting_example %>% filter( year %in% c(2017, 2018)), 
               aes( x = 0, xend = doy_crit, y = herb_npp_crit, yend = herb_npp_crit), linetype = 2, size = 0.5, alpha = 0.5) + 
  geom_text( data = plotting_example %>% 
               filter( year %in% c(2017, 2018)), 
             aes( x = 0, y = herb_npp_crit, label = label2), alpha = 0.7, size = 3, nudge_y = 20, hjust = 0) +
  geom_text( data = plotting_example %>% 
               filter( year %in% c(2017, 2018)), 
             aes( x = doy_crit, y = 0, label = label), alpha = 0.7, size = 3, angle = -60, nudge_y = -5, hjust = 0) +
  facet_grid( ~ year ) 

p2 <- plotting_example %>% 
  filter( year > 2014, year < 2020) %>%
  ggplot( aes( x = year, y = doy_crit )) + 
  geom_point(aes(color =label2)) + 
  geom_line(aes( group = label2, color = label2)) + 
  geom_text( data = plotting_example %>% filter( year %in% c(2017, 2018)), 
             aes( x = year, y = doy_crit, label = label ), alpha = 0.7, size = 3, hjust =0, nudge_y = 10) + 
  scale_x_continuous(name = 'Year', breaks = c(2014:2019)) + 
  scale_y_continuous(name = 'Day of Year') +
  scale_color_discrete(name = 'Thresh.') + 
  theme_bw() + 
  theme(legend.position = c(1.1,1.15), legend.box.background = element_rect(fill = NA, colour = 1))

layout<-matrix( c(1,3,2,3), byrow = T, nrow = 2, ncol = 2)

gridExtra::grid.arrange(p1, p2, layout_matrix = layout, widths = c(1,0.2 )) %>% 
  ggsave(filename =  'output/figures/growing_season_example.png', 
          height = 8, width = 8, units = 'in', dpi = 600)
