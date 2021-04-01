rm(list = ls())

library(tidyverse)
library(dbplyr)
library(ggridges)

my_colors <- c('Annual' = 'sienna1', 
               'Bare'  = 'darkgray', 
               'Perennial' = 'steelblue', 
               'Tree' = 'forestgreen', 
               'Shrub' = 'lavender', 
               'Litter' = 'red')

plot_trends <- function( dataset, my_colors ){ 
  dataset %>% 
    ggplot( aes( x = year , y= value, fill = type, color = type)) + 
    stat_summary(fun.max = function(x) quantile(x, 0.75),
                 fun.min = function(x) quantile(x, 0.25), 
                 geom = 'ribbon', alpha = 0.3, color = NA) + 
    stat_summary(fun = function( x ) median(x), geom = 'line') + 
    scale_fill_manual(values = my_colors) +   
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

allotment_info <- tbl( con, 'allotments')

allotment_regions <- 
  allotment_info %>% 
  select(uname, 
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

allotment_summary <- 
  allotment_regions %>% 
  filter( !is.na(parent_cd)) %>%
  group_by(district_label, admin_st ) %>% 
  summarise( num_allotments = as.numeric( count(uname) ), 
             total_acres = ( as.numeric( sum(acres))), 
             mean_acres = mean(acres), 
             min_acres = min(acres), 
             max_acres = max(acres), 
             mean_elevation = mean(elevation))

fig1_num_allotments <- 
  allotment_summary %>%  
  ggplot(aes( x = district_label, y = num_allotments)) + 
  geom_bar(stat = 'identity', position = position_dodge()) + 
  geom_text( aes( label = num_allotments ), nudge_y = 110) + 
  facet_wrap( ~ admin_st, scales = 'free_x', nrow = 2) + 
  scale_y_continuous(name = 'Total Number of Allotments') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(margin = margin(1,10,1,1)))  + 
  ggsave(filename = 'output/figures/fig1_num_allotments.pdf', 
       width = 10, height = 7, units = 'in')


fig2_acres_allotments <- 
  allotment_summary %>%  
  ggplot(aes( x = district_label, y = total_acres/1e6)) + 
  geom_bar(stat = 'identity', position = position_dodge()) + 
  geom_text( aes( label = round( total_acres/1e6, 1) ), nudge_y = 1) + 
  facet_wrap( ~ admin_st, scales = 'free_x', nrow = 2) + 
  scale_y_continuous(name = 'Acres (Millions)') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(margin = margin(1,10,1,1))) +
  ggsave(filename = 'output/figures/fig2_acres_allotments.pdf', 
         width = 10, height = 7, units = 'in')

# Summarize counts and acres per EcoregionII

allotment_summary <- 
  allotment_info %>% 
  mutate( ecoregion_label = str_to_title(na_l2name)) %>% 
  mutate( ecoregion_label = str_squish( str_trim( str_remove_all( ecoregion_label, '[\\?\\*\\(\\)]')))) %>% 
  group_by(ecoregion_label) %>% 
  summarise( num_allotments = as.numeric( count(uname) ), 
             total_acres = ( as.numeric( sum(acres))), 
             mean_acres = mean(acres), 
             min_acres = min(acres), 
             max_acres = max(acres), 
             mean_elevation = mean(elevation)) 

allotment_summary %>% 
  filter( total_acres > 1e6) %>%
  ggplot( aes( x = ecoregion_label, y = num_allotments )) + 
  geom_bar(stat = 'identity') + 
  geom_text( aes( label = num_allotments ), nudge_y = 150) + 
  #facet_wrap( ~admin_st , scales = 'free_x') +
  theme_bw() + 
  scale_y_continuous(name = 'Total Number of Allotments') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(margin = margin(1,10,1,1)))  + 
  ggsave(filename = 'output/figures/fig1a_num_allotments_per_ecoregion2.pdf', 
          width = 10, height = 7, units = 'in' )


allotment_summary %>% 
  filter( total_acres > 1e6) %>% 
  ggplot( aes( x = ecoregion_label, y = total_acres/1e6 )) + 
  geom_bar(stat = 'identity') + 
  geom_text( aes( label = round( total_acres/1e6, 1) ), nudge_y = 3) + 
  scale_y_continuous(name = 'Acres (Millions)') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(margin = margin(1,10,1,1)))  + 
  ggsave(filename = 'output/figures/fig2a_acres_allotments_ecoregion2.pdf', 
         width = 10, height = 7, units = 'in' )


# Allotment Cover per year ----------------------- # 

cover <- 
  tbl(con, 'annual_data') %>%
  filter( type %in% c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR' )) %>% 
  mutate( decade = as.integer( floor( year/10)*10 )) %>%
  mutate( decade = paste0( as.character( decade), 's' ))  %>% 
  mutate( type = ifelse( type == 'AFGC', 'Annual', type )) %>%
  mutate( type = ifelse( type == 'PFGC', 'Perennial', type )) %>%
  mutate( type = ifelse( type == 'BG', 'Bare', type))

# cover %>% 
#   left_join(allotment_regions, by = 'uname') %>% 
#   group_by( uname, type, decade, er1label ) %>% 
#   summarise( avg = median(value, na.rm = T) ) %>% 
#   ggplot( aes( y = decade, fill = decade, x = avg, color = decade)) + 
#   geom_density_ridges(alpha = 0.5, scale = 3) + 
#   facet_grid( type ~ er1label, scales = 'free')

cover %>% 
  left_join(allotment_regions, by = 'uname') %>% 
  filter( type %in% c('Annual', 'Perennial', 'Bare')) %>% 
  plot_trends(my_colors = my_colors) + 
  facet_wrap( ~ er1label ) + 
  scale_y_continuous(name = 'Percent Cover (+/- Interquartile Range)') + 
  ggsave(filename = 'output/figures/fig3a_cover_trend_by_level_I_ecoregion.pdf', 
         height = 7, width = 10, units = 'in') 

cover %>% 
  left_join(allotment_regions, by = 'uname') %>% 
  filter( type %in% c('Annual', 'Perennial', 'Bare')) %>% 
  plot_trends(my_colors = my_colors) + 
  facet_wrap( ~ er2label ) + 
  scale_y_continuous(name = 'Percent Cover (+/- Interquartile Range)') + 
  ggsave(filename = 'output/figures/fig3b_cover_trend_by_level_II_ecoregion.pdf', 
       height = 7, width = 10, units = 'in') 

cover %>% 
  left_join(allotment_regions, by = 'uname') %>% 
  filter( type %in% c('Annual', 'Perennial', 'Bare')) %>% 
  plot_trends(my_colors = my_colors) + 
  facet_wrap( ~ admin_st ) + 
  scale_y_continuous(name = 'Percent Cover (+/- Interquartile Range)') + 
  ggsave(filename = 'output/figures/fig3c_cover_trend_by_state.pdf', 
       height = 7, width = 10, units = 'in') 


cover %>% 
  left_join(allotment_regions, by = 'uname') %>% 
  filter( type %in% c('Annual', 'Perennial', 'Bare')) %>% 
  filter( admin_st %in% c('MT') ) %>% 
  plot_trends(my_colors = my_colors) + 
  facet_wrap(district_label ~ office_label) + 
  scale_y_continuous(name = 'Percent Cover (+/- Interquartile Range)') + 
  ggsave( filename = 'output/figures/fig3d_MT_cover_trends_by_field_office.pdf', 
          height = 7, width = 10, units = 'in')


# Allotment Aboveground Biomass
AGB <- 
  tbl(con, 'annual_data') %>%
  filter( type %in% c('afgAGB','pfgAGB')) %>% 
  mutate( decade = as.integer( floor( year/10)*10 )) %>%
  mutate( decade = paste0( as.character( decade), 's' ))   %>% 
  mutate( type = ifelse( type == 'afgAGB', 'Annual', type )) %>%
  mutate( type = ifelse( type == 'pfgAGB', 'Perennial', type))


AGB %>% 
  left_join(allotment_regions, by = 'uname') %>% 
  plot_trends( my_colors = my_colors) + 
  facet_wrap( ~ er1label) +
  scale_y_continuous(name = 'Above Ground Biomass (lbs. per acre)') + 
  ggsave(filename = 'output/figures/fig4a_AGB_trend_by_level_I_ecoregion.pdf', 
       height = 7, width = 10, units = 'in') 

AGB %>% 
  left_join(allotment_regions, by = 'uname') %>% 
  plot_trends( my_colors = my_colors) + 
  facet_wrap( ~ er2label) +
  scale_y_continuous(name = 'Above Ground Biomass (lbs. per acre)') + 
  ggsave(filename = 'output/figures/fig4b_AGB_trend_by_level_II_ecoregion.pdf', 
         height = 7, width = 10, units = 'in') 


AGB %>% 
  left_join(allotment_regions, by = 'uname') %>% 
  plot_trends( my_colors = my_colors) + 
  facet_wrap(~ admin_st ) +
  scale_y_continuous(name = 'Above Ground Biomass (lbs. per acre)') + 
  ggsave(filename = 'output/figures/fig4c_AGB_trend_by_state.pdf', 
         height = 7, width = 10, units = 'in') 


AGB %>% 
  left_join(allotment_regions, by = 'uname') %>% 
  filter( admin_st == 'MT' ) %>%
  plot_trends( my_colors = my_colors) + 
  facet_wrap( district_label ~ office_label) +
  scale_y_continuous(name = 'Above Ground Biomass (lbs. per acre)') + 
  ggsave(filename = 'output/figures/fig4d_MT_AGB_trend_by_field_office.pdf', 
         height = 7, width = 10, units = 'in') 


#  Quick secondary confirmation of acreage 

# test <- st_read(con, 'allotment_shapes') %>% 
#   mutate( area = st_area(SHAPE))  %>%
#   select( uname, area ) %>%
#   st_drop_geometry() 
# 
# 
# mt <- tbl( con, 'allotments') %>% filter( admin_st == 'MT') %>% collect() 
# 
# mt %>% 
#   group_by( parent_name ) %>% 
#   summarise( n_distinct(uname) )
# 
# mt %>%
#   left_join(test , by = 'uname') %>%
#   group_by( admin_st, parent_name) %>% 
#   summarise( n_distinct(uname) , sum(area), sum(area)/4046.86)
