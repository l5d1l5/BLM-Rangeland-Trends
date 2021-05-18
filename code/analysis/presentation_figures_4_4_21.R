rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)


back_transform <- function(x, attributes_obj, log = T ){ 
  
  center <- attributes_obj$`scaled:center`
  sd <- attributes_obj$`scaled:scale`
  
  if( log ){ 
    exp( (sd*x) + center) 
  }else{ 
    sd*x - center 
  }
}

plot_single_allotment_trends <- function( dataset, my_colors ){ 
  dataset %>% 
    ggplot( aes( x = year , y= value, fill = type, color = type)) + 
    geom_line() + 
    scale_color_manual(values = my_colors) + 
    theme_bw() + 
    theme( axis.title.x =  element_blank() , legend.title = element_blank() ) 
}



con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

my_colors <- c('Annual' = 'sienna1', 
               'Bare'  = 'darkgray', 
               'Perennial' = 'steelblue', 
               'Tree' = 'forestgreen', 
               'Shrub' = 'lavender', 
               'Litter' = 'red')




allotments <- 
  tbl(con, 'allotments') %>% 
  select(uname, 
         allot_name,  
         admu_name,
         admin_st, 
         admin_st_y, 
         parent_cd, 
         parent_name, 
         na_l1name, na_l2name, na_l3name, acres, elevation) %>% 
  mutate( admin_st = ifelse(is.na(admin_st), admin_st_y, admin_st)) %>% 
  mutate( parent_name = str_to_title(parent_name)) %>%
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim ( str_to_title(admu_name))), 
                                  pattern = ' Field.*$')
  ) %>% 
  mutate( er1label = str_squish(
    str_trim(
      str_remove_all(
        str_to_title(na_l1name), '[\\?\\*\\(\\)]')
    )
  )
  ) %>% 
  mutate( er2label = str_squish(
    str_trim(
      str_remove_all(
        str_to_title(na_l2name), '[\\?\\*\\(\\)]')
    )
  )
  ) %>% 
  mutate( er3label = str_squish(
    str_trim(
      str_remove_all(
        str_to_title(na_l3name), '[\\?\\*\\(\\)]')
    )
  )
  ) 

# Cover trends: 

cover <- tbl(con, 'annual_data') %>%  
  filter( type %in% c('AFGC', 'PFGC', 'BG')) %>% 
  left_join(allotments, by = 'uname')


top_allots <- 
  cover %>% 
  filter( admin_st == 'MT'  & 
            office_label =='Miles City') %>% 
  distinct(office_label, allot_name, uname, acres ) %>% 
  arrange( desc(acres )) 

top_allots

example_ts <- 
  cover %>% 
  filter( uname %in% c(7094, 8384, 7179)) %>% 
  mutate( type = ifelse( type == 'AFGC', 'Annual', type )) %>%
  mutate( type = ifelse( type == 'PFGC', 'Perennial', type )) %>%
  mutate( type = ifelse( type == 'BG', 'Bare', type))  %>%
  collect() %>%
  as.data.frame()


example_ts %>% 
  plot_single_allotment_trends(my_colors = my_colors) +
  scale_y_continuous(name = 'Cover (%)') + 
  facet_wrap(~ allot_name, ncol = 1 ) + 
  ggtitle(label = 'Example Allotments, Miles City, MT')




allots <- read_sf(con, 'allotment_shapes') %>%
  filter( uname %in% c(7094, 8384, 7179)) 

allots %>% 
  left_join(example_ts %>% distinct(uname, allot_name), by = 'uname') %>% 
  ggplot() + 
  geom_sf() + 
  theme_bw()


# Production 
prod <- tbl(con, 'annual_data') %>%  
  filter( type %in% c('afgAGB', 'pfgAGB')) %>% 
  left_join(allotments, by = 'uname')

example_ts <- 
  prod %>% 
  filter( uname %in% c(7094, 8384, 7179)) %>% 
  mutate( type = ifelse( type == 'afgAGB', 'Annual', type )) %>%
  mutate( type = ifelse( type == 'pfgAGB', 'Perennial', type )) %>%
  collect() %>%
  as.data.frame()


example_ts %>% 
  plot_single_allotment_trends(my_colors = my_colors) +
  #geom_smooth(data = example_ts %>% filter( year >= 2000), aes( group = type ), 
  #            method = 'lm', se = F, lty = 1, size = 0.5) + 
  scale_y_continuous(name = 'Production (lbs per acre)') + 
  facet_wrap(~ allot_name, ncol = 1 ) + 
  ggtitle(label = 'Example Allotments, Miles City, MT') + 
  ggsave(filename = 'output/figures/fig5a_AGB_example_production_trends_MilesCity.pdf', 
         height = 7, width = 10, units = 'in') 

example_ts %>% 
  plot_single_allotment_trends(my_colors = my_colors) +
  geom_smooth(data = example_ts %>% filter( year >= 2000), aes( group = type ), 
              method = 'lm', se = F, lty = 1, size = 0.5) + 
  scale_y_continuous(name = 'Production (lbs per acre)') + 
  facet_wrap(~ allot_name, ncol = 1 ) + 
  ggtitle(label = 'Example Allotments, Miles City, MT') + 
  ggsave(filename = 'output/figures/fig5b_AGB_example_production_trends_with_trendlines_MilesCity.pdf', 
         height = 7, width = 10, units = 'in') 


# 
# Biomass Trends
afg_model <- read_rds( file = 'output/afg_npp_trend_model.rds')
pfg_model <- read_rds( file = 'output/pfg_npp_trend_model.rds')

get_ecogroup_trends_as_df <- function(trend_model, type){ 

  trends <- emmeans::emtrends(trend_model, ~ ecogroup, var = 'year2') %>% 
    data.frame(type = type)
}

plot_ecogroup_trend_coefficients <- function( trend_summary, my_colors ){ 
  
  trend_summary %>%
    ggplot(aes( x = ecogroup, color = type, 
                y = year2.trend, ymin = asymp.LCL, ymax = asymp.UCL )) + 
      geom_point(position = position_dodge(width = 1)) + 
      geom_errorbar(position = position_dodge( width = 1)) + 
      geom_hline(aes( yintercept = 0), lty = 2, color = 'darkgray') + 
      scale_color_manual(values = my_colors, name = 'Trend') + 
      theme_bw() + 
      theme(axis.text.x =  element_text(angle = 45 , hjust = 1), axis.title.x = element_blank()) + 
      ylab( 'Annual Coefficient')    
    
}

afg_trends <- get_ecogroup_trends_as_df(afg_model, type = 'Annual')
pfg_trends <- get_ecogroup_trends_as_df(pfg_model, type = 'Perennial')


npp_trends <- rbind( afg_trends, pfg_trends )
npp_trends
trend_plot <- 
  npp_trends %>%
  plot_ecogroup_trend_coefficients()

trend_plot

#  ggtitle( 'Trends in Aboveground Biomass') + 
#  ggsave(filename = 'output/figures/fig6a_AGB_linear_by_Ecoregion.pdf', 
#         height = 7, width = 10, units = 'in') 


# cover trends: 
AFGC_model <- read_rds( file = 'output/AFGC_cover_trends.rds')

cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_trend_models, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
trend_table <- mapply( x = cover_models, y = types, FUN = function(x,y) get_ecogroup_trends_as_df(x, y), SIMPLIFY = F)

trend_table <- do.call(rbind, trend_table)

trend_table %>% distinct(type)

test <- plot_ecogroup_trend_coefficients(trend_table)
test

AFGC_trends <- get_ecogroup_trends_as_df(AFGC_model, type = 'Annual')
PFGC_trends <- get_ecogroup_trends_as_df(AFGC_model, type = 'Perennial')


annual_trends <- emtrends(annual_model, ~ er1label, 'year2') %>%
  as.data.frame() %>% 
  mutate( type = 'Annual')

read_csv()

perennial_model <- read_rds( file = 'output/PFGC_cover_trends.rds')

perenial_trends <- emtrends(perennial_model, ~ er1label, 'year2') %>%
  as.data.frame() %>% 
  mutate( type = 'Perennial')

bare_model <- read_rds( file = 'output/BG_cover_trends.rds')

bare_trends <- emtrends(bare_model, ~ er1label, 'year2') %>%
  as.data.frame() %>% 
  mutate( type = 'Bare')

rbind( annual_trends, perenial_trends, bare_trends ) %>% 
  ggplot(aes( x = er1label, color = type, 
              y = year2.trend, ymin = year2.trend + SE, ymax = year2.trend - SE )) + 
  geom_point(position = position_dodge(width = 1)) + 
  geom_errorbar(position = position_dodge( width = 1)) + 
  geom_hline(aes( yintercept = 0), lty = 2, color = 'darkgray') + 
  scale_color_manual(values = my_colors, name = 'Trend') + 
  theme_bw() + 
  theme(axis.text.x =  element_text(angle = 45 , hjust = 1), axis.title.x = element_blank()) + 
  ylab( 'Annual Coefficient') + 
  ggtitle( 'Trends in Allotment Cover') + 
  ggsave(filename = 'output/figures/fig6b_Cover_linear_Trends_by_Ecoregion.pdf', 
          height = 7, width = 10, units = 'in') 

