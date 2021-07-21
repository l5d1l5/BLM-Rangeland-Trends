rm(list = ls())
library(tidyverse)
library(DBI)
library(dbplyr)
require( RPostgres  )

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

critical_stats <- read_rds(file = 'data/temp/allotment_growing_season.rds')

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

plotting_example$label3 <- factor( plotting_example$label2, labels = c('SOS (25%)', 
                                               'MS (50%)', 
                                               'EOS (75%)'))

p1 <- example_ts %>% 
  filter( year > 2014, year < 2020) %>% 
  ggplot( aes( x = doy, y = herb_npp_cmsm, group = uname )) + 
  geom_line(size = 0.5, alpha = 0.5) + 
  geom_point(alpha = 0.5, size = 0.8) +
  geom_point(data = plotting_example %>% 
               filter( year > 2014, year < 2020), 
             aes( x= doy_crit, y = herb_npp_crit, color = crit_val), size = 3, alpha = 0.7) + 
  geom_hline(aes(yintercept = 0), color = 'darkgray') + 
  scale_x_continuous(name = 'Day of Year', breaks = c(0, 300)) + 
  scale_y_continuous(name = 'Cumulative NPP', limits = c(-50,NA)) + 
  scale_color_manual(guide = F, name = 'Phenology', values = c('forestgreen', 'darkgray', 'blue')) + 
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
  geom_point(aes(color =label3)) + 
  geom_line(aes( group = label3, color = label3)) + 
  geom_text( data = plotting_example %>% filter( year %in% c(2017, 2018)), 
             aes( x = year, y = doy_crit, label = label ), alpha = 0.7, size = 3, hjust =0, nudge_y = 10) + 
  scale_x_continuous(name = 'Year', breaks = c(2014:2019)) + 
  scale_y_continuous(name = 'Day of Year') +
  scale_color_manual(name = 'Phenological Period', values = c('forestgreen', 'darkgray', 'blue')) + 
  theme_bw() + 
  theme(legend.position = c(1.15, 0.5), 
        legend.box.background = element_rect(fill = NA, colour = 1))

example_duration_df <- 
  plotting_example %>% 
  dplyr::select(uname, year, label3, doy_crit ) %>% 
  pivot_wider(id_cols = c(uname, year), names_from = label3, values_from = doy_crit) %>% 
  mutate( duration = `EOS (75%)` - `SOS (25%)`)  %>%
  mutate( label = paste( round(duration), ' days'))

p3 <- example_duration_df %>% 
  filter( year > 2014, year < 2020) %>%
  ggplot( aes( x = year, y = duration )) + 
  geom_point() + 
  geom_line() + 
  geom_text( data = example_duration_df %>% filter( year %in% c(2017, 2018)), 
             aes( x = year, y = duration, label = label ), alpha = 0.7, size = 3, hjust = 0.5, 
             nudge_y = 2) + 
  scale_x_continuous(name = 'Year', breaks = c(2014:2019)) + 
  scale_y_continuous(name = 'Growing Season Duration (days)') +
  theme_bw()

layout<-matrix( c(1, 4, 2,4,3, 4), byrow = T, nrow = 3, ncol = 2)

gridExtra::grid.arrange(p1 + ggtitle('a)'), 
                        p2 + ggtitle('b)'), 
                        p3 + ggtitle('c)'), 
                        layout_matrix = layout , 
                        widths = c(1, 0.3), heights = c(1.2, 0.9, 0.9)) %>%
  ggsave(filename =  'output/figures/growing_season_example.png', 
         height = 8, width = 8, units = 'in', dpi = 600)
