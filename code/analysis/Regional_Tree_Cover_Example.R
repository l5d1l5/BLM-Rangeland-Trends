rm(list = ls())
library(sf)
library(tidyverse)

annual_data <- read_rds('data/temp/annual_data.rds')
allotments <- read_csv('data/temp/allotment_info.csv')
state_layer <- read_rds('data/temp/western_states_outlines_shapefile.rds')  
sg <- read_sf( 'data/spatial/COT_SG_Populations_2013/COT_SG_Populations_2013.shp')
shps <- read_sf('data/temp/BLM_allotments_cleaned/allotments.shp')
rate_shps <- read_sf( 'data/temp/veg_rates/allotments_with_rates.shp') 

fo <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds') 
mc <- fo %>% filter( ADM_UNIT_CD == 'MTC02000') %>% 
  st_transform(crs = st_crs( shps))

tree_data <- annual_data %>% 
  left_join(allotments, by = 'uname') %>% 
  filter( ecogroup != "Marine West Coast Forest") %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover', type == 'TREE', year > 1990 ) %>% 
  mutate( log_cover = log(value))

# 
sg1 <- sg %>% filter( MgmtZone == 1) %>% 
  st_transform(crs = st_crs(shps))

MT_layer <- state_layer %>% 
  st_transform( crs = st_crs(shps))

in_sg <- shps %>% 
  st_join(sg1) %>% 
  filter( !is.na(POPULATION)) 

tree_cover_shps <- 
  in_sg %>% 
  left_join(tree_data %>% filter( year == 2020))  %>% 
  filter( !is.na(value)) 

tree_rates <- rate_shps[ rate_shps$uname %in% c( in_sg %>% pull( 'uname' ) ),  ] %>% 
  select( uname, ADMIN_ST,  ADM_OFC_CD, ALLOT_NAME, Tree, geometry)

MT_layer <- MT_layer %>% st_transform( crs = 4326 ) 
mt_bbox <- MT_layer %>%  filter( STUSPS == 'MT') %>% st_bbox() 

add_x <- diff( mt_bbox[c(1,3)] )*0.1
add_y <- diff( mt_bbox[c(2,4)])*0.25 

sg1 <- sg1 %>% st_transform(crs = 4326 )
sg_centers <- sg1 %>% st_centroid()

tree_cover_shps <- tree_cover_shps %>% st_transform( crs = 4326)
tree_rates <- tree_rates %>% st_transform( crs = 4326)
mc <- mc %>% st_transform( crs = 4326 )

tree_cover_shps$value[ tree_cover_shps$value > 20 ] <- 20 

hist( tree_rates$Tree ) 
range( tree_rates$Tree, na.rm = T)

tree_rates$Tree2 <- tree_rates$Tree
tree_rates$Tree2[ tree_rates$Tree > 0.05] <- 0.05
tree_rates$Tree2[ tree_rates$Tree < -0.025] <- -0.025

# State Labels
x <- c(-112, -106, -103, -103, -105 )
y <- c(47.5, 44.5, 44.5, 47.5 , 49.2 )
labels <- c('Montana', 'Wyoming', 'South Dakota', 'North Dakota', "Canada")
geo_labels <- data.frame( x = x, y = y, labels = labels )
geo_labels <- geo_labels %>% 
  filter( labels == "Montana")

bbox <- st_bbox(sg1)

sg_centers <- sg_centers %>% mutate( X = st_coordinates(. )[, 1 ] , Y = st_coordinates(.)[, 2])

sg_centers <- sg_centers %>% 
  mutate( Yadj = Y + 0.8 ) %>% 
  mutate( Xadj = ifelse( POPULATION == "Powder River Basin", -109, X)) %>% 
  mutate( Yadj = ifelse( POPULATION == "Powder River Basin", Y + 0.5, Yadj)) %>% 
  mutate( Xadj = ifelse( POPULATION != "Powder River Basin",  X + 1.5, Xadj )) %>% 
  mutate( Xadj = ifelse( POPULATION == "Dakotas", X + 0.8, Xadj)) %>% 
  mutate( Yadj = ifelse( POPULATION == "Dakotas", Y + 1, Yadj)) %>% 
  mutate( Xadj = ifelse( POPULATION == "Yellowstone Watershed", X - 3.7, Xadj )) %>% 
  mutate( Yadj = ifelse( POPULATION == "Yellowstone Watershed", Y - 0.7, Yadj))

tree_cover_shps %>%  
  ggplot( ) + 
  geom_sf( data = MT_layer, fill = NA, color = 'darkgray', size = 0.4) +
  #geom_sf( data = mc, fill = NA, size = 0.2, alpha = 1, color = 'black') + 
  ylim( bbox[c(2, 4)] + c(-0.2, 0)) + 
  xlim( bbox[c(1, 3)] + c(-2.8, 1.75)) + 
  #geom_sf( aes( fill = value, alpha = value), color = 'white', size = 0.01) + 
  geom_sf( data = tree_rates, aes( fill = Tree, alpha = Tree), color = 'white', size = 0.02) +
  geom_sf( data = sg1, fill = NA, linetype = 2, size = 0.6, alpha = 1) +  
  geom_label( data = sg_centers, aes( x = Xadj, y = Yadj, label = POPULATION)) + 
  ggspatial::annotation_scale( location = 'br') + 
  scale_alpha_continuous(range = c(0.8,1), guide = 'none') +  
  scale_color_discrete(guide = "none") + 
  scale_fill_gradient2(midpoint = 0.0125, 
                       limits = c(-0.0251, 0.050999), low = 'blue', mid = 'yellow', high = 'red',
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
  annotate(geom = 'text', x = geo_labels$x, y = geo_labels$y, label = geo_labels$labels, size = 3.2) + 
  ggtitle('A) Regional')  + 
  ggsave(filename = 'output/figures/Regional_tree_cover_example.png', 
         width = 6, height = 5.2, units = 'in', dpi = 600)


# Time series plots for each sagegrouse zone 
shps <- shps %>% st_transform(crs = 4326 )
uname_pops <- shps %>% 
  st_join(sg1) %>% 
  filter( !is.na(POPULATION)) %>% 
  select( uname, POPULATION )

sg_tree_data <- tree_data %>% 
  left_join(uname_pops ) %>% 
  filter( !is.na(POPULATION)) 

sg_tree_data %>% 
  group_by( year, POPULATION ) %>% 
  summarise( n() )

sg_tree_data %>% 
  group_by( year , POPULATION) %>% 
  summarise(perc = 100*sum(value > 3)/n() ) %>% 
  ggplot( aes( x = year, y= perc , color = POPULATION)) + 
  geom_line() + 
  scale_y_continuous() + 
  ylab( "Tree Cover > 3% of allotments)")

#mPois <- glm(data = sg_tree_data, value ~ year*POPULATION, family = 'poisson')
#pred_grid <- sg_tree_data %>% distinct(year, POPULATION)
#pred_grid$yhatP <- predict( mPois, newdata = pred_grid, type = 'response')

sg_mean_cover <- sg_tree_data %>% 
  group_by( year , POPULATION) %>% 
  summarise(mean = mean(value) ) 

ts_labels <- 
  sg_mean_cover %>% 
  group_by( POPULATION ) %>% 
  summarise( mean = mean(mean)) %>%  
  mutate( xpos = ifelse( POPULATION == "Yellowstone Watershed",  2014, 2000  )) %>% 
  mutate( xpos = ifelse( POPULATION == "Dakotas",  2011, xpos  )) %>% 
  mutate( ypos = mean )  %>% 
  mutate( ypos = ifelse( POPULATION == "Yellowstone Watershed", ypos - 0.6, ypos)) %>% 
  mutate( ypos = ifelse( POPULATION == "Powder River Basin", ypos - 0.1, ypos))

load('data/analysis_data/cover.rda')

mod <- read_rds( 'output/TREE_cover_trend_model.rds')
df <- mod@frame %>% 
  filter( ecogroup == 'N Great Plains') %>% 
  distinct( ecogroup, year2 )

df$yhat <- predict(mod,  newdata = df , re.form = NA )
v2_att <- attributes( mod@frame$value2 )
y2_att <- attributes( mod@frame$year2)
df$yhat_pred <-  exp( ( df$yhat*v2_att$`scaled:scale` + v2_att$`scaled:center` )  ) 
df$year <- (df$year2*y2_att$`scaled:scale` + y2_att$`scaled:center`)

tree_ts <- cover$TREE %>% 
  filter( admin_st == 'MT', office_label == 'MILES CITY FIELD OFFICE') %>% 
  left_join(tree_rates %>% select( uname, Tree)) %>% 
  filter( !is.na(Tree)) 

tree_quantile <- tree_ts %>% 
  group_by( year ) %>% 
  summarise( lq = quantile(value, 0.25), med = median(value), uq = quantile( value, 0.75))

source('code/analysis/parameters.R')

library(scales)
show_col(hue_pal()(4))

colpal <- hue_pal()(4)
colpal[4] <- 'blue'

sg_mean_cover %>% 
  ggplot( aes( x = year, y= mean,color = POPULATION)) + 
  geom_line() + 
  scale_y_continuous(name = "Tree Cover (%)") + 
#  geom_ribbon(data = tree_quantile,  aes( x = year , y = med, ymin = lq, ymax = uq), 
#              fill = NA, color = NA, alpha =  0 ) + 
  geom_line( data = df, aes( x = year, y = yhat_pred, group = 1), size = 1.1, alpha = 1 , 
             color = my_colors['Tree'], linetype = 2)  + 
  geom_label( data = ts_labels, aes( x = xpos, y = ypos, label = POPULATION)) + 
  #annotate( geom = 'label', x = 2016, y = 4, 
  #          label = 'N Great Plains Trend', color = my_colors['Tree'], size = 4.2) + 
  scale_color_manual(values = colpal,guide = 'none') + 
  #scale_alpha_manual(values = c(0.6, 0.6, 0.6, 1), guide = 'none') + 
  theme_bw() +
  xlab( "Year" ) + 
  ylim( c(0, 5.8)) + 
  ggsave(filename = 'output/figures/Regional_SG_Tree_Cover.png', 
         width = 6, height = 5, units = 'in', dpi = 600)


write_rds( sg_median_cover, 'data/temp/Tree_Cover_in_SG_POPS.rds')

