rm(list = ls()) 
library(raster) 
library(tidyverse)
library(sf)

#cornwell<- raster::brick( 'data/temp/example_allotment_cover.tif')
cornwell <- raster::brick('data/temp/example_allotment_trends2.tif')
allotment_rates <- st_read('data/temp/agb_rates/allotments.shp')

fo <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds') 
mc <- fo %>% filter( ADM_UNIT_CD == 'MTC02000') %>% st_transform(crs = st_crs(allotment_rates))
mc_diff <- mc %>% st_difference(allotment_rates %>% filter( ALLOT_NAME == "CORNWELL"))

tree_cover <- cornwell$TREE_scale %>% 
  projectRaster(crs = "EPSG:5070")

mt_allotments <- 
  allotment_rates %>% 
  filter( ADMIN_ST == 'MT')

tree_cover <- crop( tree_cover, allotment_rates)
ee2 <- extent( tree_cover) 
tree_cover_smaller <- crop(tree_cover, extent(tree_cover)*0.4) 

tree_cover_smaller[tree_cover_smaller >= 0.075 ] <- 0.075
tree_cover_smaller[tree_cover_smaller <= -0.01 ] <- -0.01

tree_dat <- 
  tree_cover_smaller %>% 
  projectRaster( crs = "EPSG:4326") %>% 
  as.data.frame( xy = TRUE)  

mt_allotments <- 
  mt_allotments %>% 
  st_transform(crs =  4326 )

mt_allotments %>% 
  ggplot() + 
  geom_sf(data = mt_allotments %>% 
            filter(ALLOT_NAME == 'CORNWELL'), fill = 'lightgray', alpha = 1, color = NA) + 
  xlim( c(-106.51, -106.38)) + 
  ylim( c( 45.915, 45.994 )) + 
  geom_tile( data = tree_dat, aes( x = x, y = y, fill = TREE_scale, alpha = TREE_scale)) +
  geom_sf(data = mc_diff, fill = 'lightgray', alpha = 1, color = NA) + 
  geom_sf(data = mt_allotments, fill = NA,  size = 0.5, color = 'darkgray', alpha = 1) + 
  geom_sf(data = mt_allotments %>% 
            filter(ALLOT_NAME == 'CORNWELL'), 
          fill = NA, 
          alpha = 1, 
          color = 1, 
          size = 0.25) + 
  ggspatial::annotation_scale( location = 'br') + 
  scale_fill_gradient2(midpoint = 0.03, 
                       limits = c(-0.011, 0.076), 
                       low = 'blue', mid = 'yellow', high = 'red',
                     na.value = 'gray', 
                     name = 'Tree cover trends', 
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
  scale_alpha_continuous(range = c(0.6, 1), guide = "none") + 
  theme(axis.title = element_blank(), 
      legend.position = c(0.2, 0.1), 
      legend.margin = margin(5, 15, 5, 15), 
      legend.box.background = element_rect(colour = 'black')) + 
  ggtitle('C ) Single Allotment') + theme(title = element_text(size = 14), 
                                     axis.text = element_blank(), 
                                     axis.ticks = element_blank()) + 
  annotate( geom = 'label', y = 45.985, x = -106.435, label = 'Cornwell Allotment') + 
  ggsave(filename = 'output/figures/Cornwell_raster.png', 
         width = 6, height = 5.2, units = 'in')

# -------------------------------------------- # 
load('data/analysis_data/cover.rda')
unloadNamespace('raster')
treemod <- read_rds( 'output/TREE_cover_trend_model.rds')
uname_dat <- treemod@frame %>% filter( uname == 7272 )
uname_dat$yhat <- predict( treemod, newdata = uname_dat ) 
uname_dat$CornwellTrend <- exp( (uname_dat$yhat*attr( treemod@frame$value2, 'scaled:scale') + attr( treemod@frame$value2, 'scaled:center') )) 

uname_dat$yhatFO <- predict( treemod, newdata = uname_dat, re.form = ~ (year2|ecogroup:office_label))
uname_dat$FOTrend <- exp( (uname_dat$yhatFO*attr( treemod@frame$value2, 'scaled:scale') + attr( treemod@frame$value2, 'scaled:center') )) 
sg_cover <- read_rds( 'data/temp/Tree_Cover_in_SG_POPS.rds') %>% 
  filter(POPULATION == "Yellowstone Watershed")

mc_cover <- cover$TREE %>% 
  filter( office_label == 'MILES CITY FIELD OFFICE') %>% 
  group_by( office_label , year )  %>% 
  summarise( avg_cover = mean(value )) 

uname_dat$year <- 1991:2020
uname_dat$allotment_cover <- cover$TREE %>% filter( uname == 7272) %>% pull(value)

cover$TREE %>% filter( uname == 7712)  %>% ggplot( aes( x = year, y = value)) + geom_line()
cover$TREE %>% filter( uname == 7712)  %>% ggplot( aes( x = year, y = value)) + geom_line()

library(sf)
rate_shps <- read_sf( 'data/temp/veg_rates/allotments_with_rates.shp') 

tree_rates <- rate_shps %>% 
  filter( ADMIN_ST == 'MT', 
          ADM_OFC_CD == 'C02000') %>% 
  dplyr::select( ADMIN_ST, uname, ADM_OFC_CD, Tree, geometry) %>% 
  st_transform( crs = 4326 )

tree_ts <- cover$TREE %>% 
  filter( admin_st == 'MT', office_label == 'MILES CITY FIELD OFFICE') %>% 
  left_join(tree_rates %>% dplyr::select( uname, Tree)) %>% 
  filter( !is.na(Tree)) 

tree_quantile <- tree_ts %>% 
  group_by( year ) %>% 
  summarise( lq = quantile(value, 0.25), med = median(value), uq = quantile( value, 0.75))


source('app/parameters.R')
sp <- read_csv('data/RAP_EE_exports/cornwell_sample_points.csv') %>% 
  separate( `system:index`, c('year', 'pt')) %>% 
  mutate( year = as.numeric(year)) 


sp %>% 
  group_by( year ) %>% 
  summarise( uq = quantile( first, 0.75, na.rm = T), 
             lq = quantile( first, 0.25, na.rm = T), 
             med = median( first, na.rm = T)) 

sp <- sp %>% mutate( xy = str_extract(pattern = '-[0-9.]+,[0-9.]+', .geo)) %>% 
  separate( xy, c('lon', 'lat'), sep = ',') 

sp_select <- sp %>% 
  mutate( pt = as.numeric( pt ) ) %>% 
  group_by( pt) %>% 
  filter( min(first) > 0)


ts_plot <- uname_dat %>% 
  filter( year > 1990 ) %>% 
  ggplot(aes( x = year, y = allotment_cover)) + 
  geom_line(data = sp_select %>% filter( year > 1990), 
             aes( x = year, y = first, group = pt), alpha = 0.6 ) + 
  # geom_ribbon(data = tree_quantile, 
  #             aes( x = year, ymin = lq, ymax = uq, y = med ), fill = my_colors['Tree'], 
  #             alpha = 0.4) + 
  #geom_line( data = sg_cover, aes( x = year, y = mean ), linetype = 1, color = 'blue', alpha = 1) + 
  #geom_line( color = 'black') + 
  #geom_line(aes( y = FOTrend), linetype = 2, color = 'darkslategray', size = 1, alpha = 0.9) + 
  #annotate( geom = 'label', label = "Yellowstone Watershed", x = 2014, y = 1.8, color = 'blue', alpha = 1 ) + 
  #annotate( geom = 'label', label = "Miles City Field Office Trend", x = 2007, y = 2.8, 
  #          color = 'darkslategray' , alpha = 1)  + 
  theme_bw() + 
  ylab( 'Tree Cover (%)') + 
  xlab( 'Year') + 
  geom_line(aes( x = year, y = CornwellTrend), lty = 2, color = 'blue', size = 1.2)  + 
  annotate(geom = 'label', label = 'Allotment Trend', x = 2005, y = 1, color = 'blue')
  
ts_plot

ts_plot + 
  ggsave(filename = 'output/figures/Cornwell_ts.png', 
         width = 6, height = 5, units = 'in')


