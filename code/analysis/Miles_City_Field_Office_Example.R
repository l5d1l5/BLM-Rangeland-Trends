rm(list = ls()) 
library(tidyverse)
library(sf)
library(lme4)
library(mapedit)
unloadNamespace('raster')

source('code/analysis/parameters.R')

annual_data <- read_rds('data/temp/annual_data.rds')
allotments <- read_csv('data/temp/allotment_info.csv')
tree_data <- annual_data %>% 
  left_join(allotments, by = 'uname') %>% 
  filter( ecogroup != "Marine West Coast Forest") %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover', type == 'TREE', year > 1990 ) %>% 
  mutate( log_cover = log(value))

rate_shps <- read_sf( 'data/temp/veg_rates/allotments_with_rates.shp') 
fo <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds') 
mc <- fo %>% filter( ADM_UNIT_CD == 'MTC02000')

scales <- read_csv('data/temp/trend_scales.csv')
model  <- read_rds('output/TREE_cover_trend_model.rds')

mc <- mc %>% st_transform(crs = 'EPSG:5070')


tree_rates <- rate_shps %>% 
  filter( ADMIN_ST == 'MT', 
          ADM_OFC_CD == 'C02000') %>% 
  select( ADMIN_ST, uname, ADM_OFC_CD, Tree, geometry) %>% 
  st_transform( crs = 4326 )

outline_merged <- bind_rows(
  ( mc %>% 
      select(Shape) %>% 
      rename('geometry' = Shape) ), 
  tree_rates %>% 
    select(geometry) ) %>% 
  summarise()

mt_wy <- fo %>% 
           filter( str_detect( ADM_UNIT_CD , '^MT|^WY' )) %>% 
           st_transform(crs = 'EPSG:5070')

mt_wy <- mt_wy %>% st_transform(crs = 4326)
outline_merged <- outline_merged %>% st_transform(crs = 4326)

tree_rates$Tree2 <- tree_rates$Tree
tree_rates$Tree2[ tree_rates$Tree > 0.05] <- 0.05
tree_rates$Tree2[ tree_rates$Tree < -0.025] <- -0.025

map <- outline_merged %>% 
  ggplot() + 
  xlim( c(-108.75, -103)) + 
  ylim( c(44.3, 47.8)) + 
  geom_sf(data = mt_wy, fill = 'lightgray', color = 'darkgray', size = 0.1) + 
  geom_sf(data = tree_rates, aes( fill = Tree2, alpha = Tree2), color = 'white', size = 0.1) + 
  ggspatial::annotation_scale( location = 'br') + 
  scale_alpha_continuous(range = c(0.8, 1), guide = 'none') +  
  geom_sf( fill = NA, color = 1, size = 0.25) +  
  scale_color_discrete(guide = "none") + 
  scale_fill_gradient2(midpoint = 0.0125, 
                       limits = c(-0.0251, 0.050999), low = 'blue', mid = 'yellow', high = 'red',
                       na.value = 'gray', 
                       name = 'Tree cover trend', 
                       guide = guide_colorbar(direction = 'horizontal', 
                                              title.theme = element_text(size = 10), 
                                              title.position = 'top', 
                                              barwidth = unit( 120, units = 'pt'))) +
  theme(axis.title = element_blank(), 
        legend.position = c(0.2, 0.1), 
        legend.margin = margin(5, 15, 5, 15), 
        legend.box.background = element_rect(colour = 'black'), 
        title = element_text(size = 14), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'lightgray'), 
        panel.grid = element_blank()) + 
  ggtitle('B) Field Office') + 
  theme(title = element_text(size = 14), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())

x <- c(-108, -105, -103.5, -103.5 )
y <- c(45.5, 44.6, 45.5, 47.5)
labels <- c( 'Montana', 'Wyoming', 'South\nDakota', 'North\nDakota')
geo_labels <- data.frame( x = x, y = y, labels = labels )

map +
  annotate( geom = 'label', label = 'Miles City Field Office', x = -106.3, y = 46.9 ) + 
  annotate(geom = 'text', 
         x = geo_labels$x, 
         y = geo_labels$y, 
         label = geo_labels$labels, size = 3.2) + 
  ggsave(filename = 'output/figures/Miles_City_Field_office.png', 
         width = 6, height = 5.2, units = 'in', dpi = 600)

# load and return basemap map as many different classes:
#ext <- st_bbox(outline_merged$geometry)
#bm <- basemap_ggmap( ext ) 
load('data/analysis_data/cover.rda')

MC_trees <- cover$TREE %>% 
  filter( office_label == 'MILES CITY FIELD OFFICE') %>% 
  distinct(year, year2, ecogroup , office_label)

MC_trees$yhat <- predict( model, 
         newdata = MC_trees, re.form = ~ (year2|ecogroup:office_label))

MC_trees <- MC_trees %>% 
  mutate( yhat2 = exp( yhat*attr(cover$TREE$value2, 'scaled:scale') + attr(cover$TREE$value2, 'scaled:center') ))



tree_ts <- cover$TREE %>% 
  filter( admin_st == 'MT', office_label == 'MILES CITY FIELD OFFICE') %>% 
  left_join(tree_rates %>% select( uname, Tree)) %>% 
  filter( !is.na(Tree)) 



tree_ts %>% 
  ggplot( aes( x = year, y= value)) + 
  geom_point(alpha = 0.1, aes( group = uname)) + 
  #stat_summary(geom = 'line', fun = 'mean', size = 2) + 
  geom_line(data = MC_trees,
            aes( y = yhat2, x = year, group = 1),
            size = 2, lty = 2, color = 'blue', alpha = 0.9) 

tree_quantile <- tree_ts %>% 
  group_by( year ) %>% 
  summarise( lq = quantile(value, 0.25), med = median(value), uq = quantile( value, 0.75))

cornwell_ts <- tree_ts %>% 
  filter( uname == 7272) %>% 
  mutate( yhat = predict( model, newdata = . )) %>% 
  mutate( yhat2 = exp( yhat*attr(cover$TREE$value2, 'scaled:scale') + attr(cover$TREE$value2, 'scaled:center')))

sg_pops <- read_rds('data/temp/Tree_Cover_in_SG_POPS.rds') %>% 
  filter( POPULATION == "Yellowstone Watershed")

tree_ts$yhat2 <- predict(model, newdata = tree_ts )
v2_att <- attributes(model@frame$value2)

tree_ts$yhat <- exp( tree_ts$yhat2*v2_att$`scaled:scale` + v2_att$`scaled:center` )

sample_allotmens <- 
  tree_ts %>% 
  filter( uname %in% c(7272, 8197, 8224, 7202, 7220, 7786, 7649, 7539, 7965 )) 
  

cornwell <- tree_ts %>% filter(allot_name == 'CORNWELL') 


tree_quantile %>% 
  ggplot( aes( x = year, y = med )) + 
  #geom_line( data = tree_ts, aes( x = year, y = value, group = uname), alpha = 0.1)  + 
  #geom_ribbon(aes( ymax = uq, ymin = lq), alpha = 0.4, fill = NA) + 
  #geom_line( color = 'green') + 
  #geom_line( data = sg_pops, aes( x = year, y = mean, group = 1), color = 'blue') + 
  geom_line( data = cornwell, aes( x = year, y = yhat )) + 
  geom_line(data = MC_trees,
            aes( y = yhat2, x = year, group = 1),
            size = 1, lty = 2, color = 'blue', alpha = 0.9)  + 
  xlab( "Year")  + 
  ylab( "Tree Cover (%)") + 
  theme_bw() +
  #annotate( geom = 'label', label = 'Yellowstone Watershed', x = 2014, 
  #          y = 1.8, alpha = 0.7, color = 'blue') + 
  annotate( geom = 'label', 
            label = 'Miles City Field Office Trend', 
            x = 2014, 
            y = 2.4, 
            color = 'blue', 
            alpha = 1) +
  annotate( geom = 'label', 
            label = 'Individual Allotments', 
            x = 1998, y = 5, 
            color = 'darkslategray', 
            alpha = 0.7) +
  ggsave(filename = 'output/figures/Miles_City_Field_Office_TS.png', 
         width = 6, height = 5, units = 'in', dpi = 600)
