rm(list = ls())
library(lme4)
library(tidyverse)
library(sf)
library(emmeans)

model_files <- dir('output', pattern = 'cover_trend_model.rds', full.names = T)
model_type <- str_extract( basename(model_files), '[A-Za-z]+')

model_files2 <- dir('output', pattern = 'agb_trend_model.rds', full.names = T)
model_type2 <- str_extract(basename( model_files2), '[A-Za-z]+')

model_files <- c(model_files, model_files2)
model_type <- c(paste0( model_type, ' cover'), paste0( model_type2, ' biomass'))

base_layer <- read_rds('data/temp/allotments_by_ecogroup_for_mapping.RDS') %>% 
  st_as_sf()

state_map <- read_rds('data/temp/western_states_outlines_shapefile.rds')

base_map <- 
  state_map %>% 
  ggplot() +
  geom_sf()  

i <- 1

pdf(file = 'output/figures/veg_trends_mapped_by_ecoregion.pdf', paper = 'letter')

for( i in 1:length( model_files)){ 
  
  m <- read_rds( model_files[i] )
  type <- model_type[i]
  ecogroup_trends <- emtrends(m, ~ ecogroup, 'year2')

  val_attrib <- attributes(m@frame$value2)
  yr_attrib <- attributes(m@frame$year2)

  ecogroup_trends <- 
    ecogroup_trends %>% 
    data.frame() %>% 
    mutate( trend_bt = year2.trend*val_attrib$`scaled:scale`/yr_attrib$`scaled:scale` )
  
  map <- base_layer %>% left_join(ecogroup_trends)

  print( 
    state_map %>% 
      ggplot() +
      geom_sf() + 
      geom_sf(data = map, 
              aes( 
                fill = trend_bt), 
                color = NA, 
                alpha = 0.7) + 
      scale_fill_gradient(low = 'blue',
                          high = 'red') + 
      geom_sf(fill = NA) + 
      ggtitle(paste0(type, ' Rate of Change')) 
  )
}
dev.off() 



# load files into Postgres database 
library(DBI)
library(dbplyr)

source('code/analysis/plot_tools.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

allotments <- tbl(con, 'allotments') %>% 
  filter( ecogroup != 'Coastal Forests') %>%
  collect() %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 'epsg:4326') %>% 
  st_transform(crs = st_crs(state_map))

trend_table_files <- dir('output', pattern = 'trends.csv', full.names = T)

trend_types <- str_extract( basename(trend_table_files), '[A-Za-z]+')
trend_units <- str_extract( basename(trend_table_files), 'agb|cover')
trend_types <- paste( trend_types, trend_units)

i <- 1
pdf('output/figures/veg_trends_mapped_by_allotment.pdf', paper = 'letter')

for( i in 1:length(trend_table_files)){ 
  
  trends <-read_csv(trend_table_files[i])

  trends <- allotments %>% 
    left_join(trends, by = 'uname')  %>% 
    filter( !is.na(full_trend))

  print( 
    base_map + 
      geom_sf(data = trends, aes( color = full_trend ), alpha = 0.7, size = 1) + 
      geom_sf(fill = NA) + 
      scale_color_gradient(low = 'blue', high = 'red') + 
      ggtitle(trend_types)
  )
  
}

dev.off() 

# Phenology 

load('output/phenology_models.rda')
models <- c(SOS_mer, MS_mer, EOS_mer, GSL_mer)
model_type <- c('Start-of-Season', 'Mid-Season', 'End-of-Season', 'Growing Season Length')

pdf( file = file.path('output/figures', 'phenology_trends_mapped_by_ecoregion.pdf'), paper = 'letter')
for( i in 1:length( models)){ 
  
  m <- models[[i]]
  type <- model_type[i]
  ecogroup_trends <- emtrends(m, ~ ecogroup, 'year2')
  
  val_attrib <- attributes(m@frame[,1])
  yr_attrib <- attributes(m@frame$year2)
  
  ecogroup_trends <- 
    ecogroup_trends %>% 
    data.frame() %>% 
    mutate( trend_bt = year2.trend*val_attrib$`scaled:scale`/yr_attrib$`scaled:scale` )
  
  map <- base_layer %>% left_join(ecogroup_trends)
  
  print( 
    state_map %>% 
      ggplot() +
      geom_sf() + 
      geom_sf(data = map, 
              aes( 
                fill = trend_bt), 
              color = NA, 
              alpha = 0.7) + 
      scale_fill_gradient(low = 'blue',
                          high = 'red', name = 'trend (days per year)') + 
      geom_sf(fill = NA) + 
      ggtitle(paste0(type, ' Phenology Rate of Change')) 
  )
}
dev.off()


