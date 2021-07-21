rm(list = ls())

library(tidyverse)
library(dbplyr)

source('code/analysis/plot_tools.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

allotment_info <- tbl( con, 'allotments')
dbListTables(con)

annual_data <- tbl(con, 'annual_data') %>% 
  pivot_wider(id_cols = c(uname, year),names_from =  type,values_from =  value) 

ecogroup_averages <- tbl(con, 'annual_climate')  %>% 
  left_join( annual_data )  %>% 
  left_join(allotment_info) %>% 
  left_join(tbl(con, 'elevation')) %>% 
  filter( year >= 1991, year <= 2019, ecogroup != 'Coastal Forests')  %>% 
  group_by( ecogroup ) %>% 
  summarise_at(.vars = c('pr', 'tavg', 
                         'herbaceousAGB', 
                         'TREE', 
                         'AFGC', 'PFGC', 
                         'BG', 'area', 
                         'elevation'), .funs = c('mean') )
  

ecogroup_totals <- annual_data  %>% 
  left_join(allotment_info) %>% 
  filter( year >= 1991, year <= 2019, ecogroup != 'Coastal Forests')  %>% 
  group_by( ecogroup, uname ) %>% 
  distinct(ecogroup, uname ) %>% 
  left_join(allotment_info) %>% 
  group_by( ecogroup ) %>% 
  summarise( area = sum(area), 
             allotments = n_distinct(uname), 
             districts = n_distinct(parent_name), 
             field_offices = n_distinct(admu_name)) %>% 
  select( ecogroup, districts, field_offices, allotments, area ) %>% 
  mutate( area_Million_hectare = (area/10e3)/1e6 ) %>%
  select( - area )

# For Table 1 in the Text 
allotment_summary <- ecogroup_totals %>% 
  left_join(ecogroup_averages, by = 'ecogroup') %>% 
  select( ecogroup, districts, field_offices, allotments, area_Million_hectare, pr, tavg, elevation) %>% 
  collect() 



allotment_summary %>% write_csv('output/allotment_summary_table.csv')

unique_groups <- annual_data  %>% 
  left_join(allotment_info) %>% 
  filter( year >= 1991, year <= 2019, ecogroup != 'Coastal Forests')  %>% 
  distinct(ecogroup, parent_name, admu_name, uname, area )  %>% 
  ungroup() %>%
  summarise( n_distinct(ecogroup), 
             n_distinct(parent_name), n_distinct(admu_name), 
             n_distinct(uname), sum(area))

# using the data in the models only 
test1 <- read_rds('output/AFG_cover_trend_model.rds')
test2 <- read_rds('output/PFG_cover_trend_model.rds')
test3 <- read_rds('output/pfg_agb_trend_model.rds')
test4 <- read_rds('output/afg_agb_trend_model.rds')
test <- rbind(test1@frame, test2@frame, test3@frame, test4@frame )

test %>%
  dplyr::select( ecogroup , office_label, district_label, uname) %>%
  group_by( ecogroup ) %>%
  summarise( noffices = n_distinct(office_label), 
             ndistricts = n_distinct(district_label), 
             nallotments = n_distinct(uname))


test %>%
  dplyr::select( ecogroup, office_label, district_label, uname) %>%
  summarise( necogroup = n_distinct(ecogroup), 
             noffices = n_distinct(office_label), 
             ndistricts = n_distinct(district_label), 
             nallotments = n_distinct(uname))

tbl(con,)
allotment_info %>% 
  group_by( ecogroup ) %>% 

  summarise( )

allotment_summary <- 
  allotment_info %>% 
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

allotment_summary %>%
  ungroup( ) %>%
  summarise( sum(num_allotments))

allotment_summary %>% 
  ungroup() %>% 
  summarise( n_distinct(admin_st)) 

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

# Summarize counts and acres per my Ecogroup Definitions 

allotment_summary <- 
  allotment_info %>% 
  group_by(ecogroup) %>% 
  summarise( num_allotments = as.numeric( count(uname) ), 
             total_acres = ( as.numeric( sum(acres))), 
             mean_acres = mean(acres), 
             median_acres = median(acres), 
             min_acres = min(acres), 
             max_acres = max(acres), 
             mean_elevation = mean(elevation)) 

allotment_summary %>% collect() %>% view

allotment_summary %>% 
  ggplot( aes( x = ecogroup, y = num_allotments )) + 
  geom_bar(stat = 'identity') + 
  geom_text( aes( label = num_allotments ), nudge_y = 150) + 
  #facet_wrap( ~admin_st , scales = 'free_x') +
  theme_bw() + 
  scale_y_continuous(name = 'Total Number of Allotments') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(margin = margin(1,10,1,1)))  + 
  ggsave(filename = 'output/figures/fig1a_num_allotments_per_ecogroup.pdf', 
          width = 10, height = 7, units = 'in' )


allotment_summary %>% 
  ggplot( aes( x = ecogroup, y = total_acres/1e6 )) + 
  geom_bar(stat = 'identity') + 
  geom_text( aes( label = round( total_acres/1e6, 1) ), nudge_y = 3) + 
  scale_y_continuous(name = 'Acres (Millions)') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(margin = margin(1,10,1,1)))  + 
  ggsave(filename = 'output/figures/fig2a_acres_allotments_ecogroups.pdf', 
         width = 10, height = 7, units = 'in' )


# Allotment Cover per year ----------------------- # 

cover <- 
  tbl(con, 'annual_data') %>%
  filter( type %in% c('AFGC', 'PFGC', 'BG', 'TREE', 'SHR' )) %>% 
  mutate( decade = as.integer( floor( year/10)*10 )) %>%
  mutate( decade = paste0( as.character( decade), 's' ))  %>% 
  mutate( type = ifelse( type == 'AFGC', 'Annual', type )) %>%
  mutate( type = ifelse( type == 'PFGC', 'Perennial', type )) %>%
  mutate( type = ifelse( type == 'BG', 'Bare', type)) %>% 
  mutate( type = ifelse( type == 'TREE', 'Tree', type)) %>%
  mutate( type = ifelse( type == 'SHR', 'Shrub', type)) 

# cover %>% 
#   left_join(allotment_regions, by = 'uname') %>% 
#   group_by( uname, type, decade, er1label ) %>% 
#   summarise( avg = median(value, na.rm = T) ) %>% 
#   ggplot( aes( y = decade, fill = decade, x = avg, color = decade)) + 
#   geom_density_ridges(alpha = 0.5, scale = 3) + 
#   facet_grid( type ~ er1label, scales = 'free')



# cover %>% 
#   ungroup() %>%
#   filter( type %in% c('Annual', 'Bare', 'Perennial', 'Tree', 'Shrub')) %>%
#   mutate( class = ifelse( type %in% c('Tree', 'Shrub'), 'Woody', 'non-Woody')) %>% 
#   group_by( year, class, type ) %>% 
#   summarise( v = mean( value ), vmin = quantile(value, 0.25), vmax = quantile( value, 0.75)) %>%
#   ggplot( aes( x = year, y = v, color = type, ymin = vmin, ymax = vmax, fill = type )) + 
#   geom_line(aes( linetype = type)) + 
#   geom_ribbon(alpha = 0.4, color = NA) + 
#   scale_color_manual(values = colors2) + 
#   scale_fill_manual(values = colors2) + 
#   facet_grid( ~ class )  + 
#   theme_bw()
cover_for_plotting <- 
  cover %>% 
  left_join(allotment_info, by = 'uname') %>% 
  mutate( ecogroup = ifelse( ecogroup == 'Mediterranean California', 'Mediterranean\nCalifornia', ecogroup)) %>%
  filter( type %in% c('Annual', 'Perennial', 'Bare', 'Tree', 'Shrub')) %>% 
  mutate( class = ifelse( type %in% c('Tree', 'Shrub'), 'Woody', 'Non-Woody'))  

cover_for_plotting %>% 
  select( uname, ecogroup, year, class, type, value, decade) %>% 
  plot_trends(my_colors = my_colors) + 
  facet_grid(ecogroup ~ class , scales = 'free_y')  +
  scale_y_continuous(name = 'Percent Cover (+/- Interquartile Range)') + 
  theme( strip.text.y = element_text(angle = 0), 
         axis.title.x = element_text()) + 
  ggsave(filename = 'output/figures/fig3a_cover_trend_by_level_I_ecoregion.pdf', 
         height = 10, width = 7, units = 'in') 

cover_for_plotting %>%  
  plot_trends(my_colors = my_colors) + 
  facet_grid(er2label ~ class, scales = 'free_y') + 
  scale_y_continuous(name = 'Percent Cover (+/- Interquartile Range)') + 
  theme( strip.text.y = element_text(angle = 0), 
         axis.title.x = element_text()) + 
  ggsave(filename = 'output/figures/fig3b_cover_trend_by_level_II_ecoregion.pdf', 
       height = 10, width = 7, units = 'in') 

cover_for_plotting %>%  
  plot_trends(my_colors = my_colors) + 
  facet_grid(admin_st ~ class, scales = 'free_y') + 
  scale_y_continuous(name = 'Percent Cover (+/- Interquartile Range)') + 
  theme( strip.text.y = element_text(angle = 0), 
         axis.title.x = element_text()) + 
  ggsave(filename = 'output/figures/fig3c_cover_trend_by_state.pdf', 
         height = 10, width = 7, units = 'in') 

cover_for_plotting %>% 
  filter( admin_st %in% c('MT') , 
          type %in% c('Annual', 'Bare', 'Perennial')) %>% 
  plot_trends(my_colors = my_colors) + 
  facet_wrap( ~ office_label) + 
  scale_y_continuous(name = 'Percent Cover (+/- Interquartile Range)') + 
  theme( axis.title.x = element_text()) + 
  ggtitle('Average Allotment Cover by MT Field Office') + 
  ggsave( filename = 'output/figures/fig3d_MT_cover_trends_by_field_office.pdf', 
          height = 7, width = 10, units = 'in')


# Allotment Aboveground Biomass
AGB_for_plotting <- 
  tbl(con, 'annual_data') %>%
  filter( type %in% c('afgAGB','pfgAGB')) %>% 
  mutate( decade = as.integer( floor( year/10)*10 )) %>%
  mutate( decade = paste0( as.character( decade), 's' ))   %>% 
  mutate( type = ifelse( type == 'afgAGB', 'Annual', type )) %>%
  mutate( type = ifelse( type == 'pfgAGB', 'Perennial', type)) %>% 
  mutate( class = 'Herbaceous AGB') %>% 
  left_join(allotment_info, by = 'uname') %>% 
  mutate( ecogroup = 
            ifelse( ecogroup == 'Mediterranean California', 'Mediterranean\nCalifornia', ecogroup))


AGB_for_plotting %>% 
  plot_trends( my_colors = my_colors) + 
  facet_grid( ecogroup ~ class, scales = 'free_y') +
  scale_y_continuous(name = 'Above Ground Biomass (lbs. per acre)') + 
  theme( strip.text.y = element_text(angle = 0), 
         axis.title.x = element_text()) + 
  ggsave(filename = 'output/figures/fig4a_AGB_trend_by_ecogroup.pdf', 
       height = 10, width = 7, units = 'in') 

AGB_for_plotting %>% 
  plot_trends( my_colors = my_colors) + 
  facet_grid( er2label ~ class, scales = 'free_y') +
  scale_y_continuous(name = 'Above Ground Biomass (lbs. per acre)') + 
  theme( strip.text.y = element_text(angle = 0), 
         axis.title.x = element_text())
  ggsave(filename = 'output/figures/fig4b_AGB_trend_by_level_II_ecoregion.pdf', 
         height = 10, width = 7, units = 'in') 


AGB_for_plotting %>% 
  plot_trends( my_colors = my_colors) + 
  facet_wrap(~ admin_st ) +
  scale_y_continuous(name = 'Above Ground Biomass (lbs. per acre)') + 
  ggsave(filename = 'output/figures/fig4c_AGB_trend_by_state.pdf', 
         height = 7, width = 10, units = 'in') 


AGB_for_plotting %>% 
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
