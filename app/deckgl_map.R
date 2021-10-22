# app with template
rm(list = ls())
library(tidyverse)
library(sf)
library(deckgl)
##############
##############

load('app/data/mapdata.rda')
load('app/data/vegdata.rda')
source('app/functions.R')
source('app/parameters.R')

# ---------- Mapbox 
my_tkn <- "pk.eyJ1IjoiYXJrbGVpbiIsImEiOiJja3J5NXAxZmUwN21zMnFxZ3pyOGZtdzg3In0.Lth7OfC8elztneoAtw2ohQ"
Sys.setenv(MAPBOX_API_TOKEN = my_tkn)

##############
##############

my_ramp <- colorRamp(colors = c('Blue', 'Gold', 'Red'))

initial_view_state <- list(
  longitude = -115.985130,
  latitude = 40.758896,
  zoom = 3.5,
  maxZoom = 12,
  minZoom = 3, 
  pitch = 0,
  bearing = 0
)

#

allotment_shps <- allotment_shps %>% 
  st_cast(to = 'MULTIPOLYGON') %>% 
  rename( 'geometry' = SHAPE )

trend_colors <- 
  allotment_shps %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = dplyr::contains( match = c( 'production', 'cover')), 
               names_to = 'type', 
               values_to = 'value' ) %>%
  group_by( type ) %>%
  mutate( lower = min(value, na.rm = T), upper = max(value, na.rm = T)) %>%
  rowwise() %>%
  mutate( Color = list( getRGB(value, 
                               ramp = my_ramp, 
                               lower = lower, 
                               upper = upper, 
                               alpha = 0.75))) %>% 
  select(uname, type, Color) %>%
  pivot_wider(uname, 
              names_from = type, 
              values_from = Color, 
              names_prefix = 'color_') %>% 
  rename('color_Bare_Ground_cover' = `color_Bare Ground_cover`)

allotment_shps <- 
  allotment_shps %>% 
  left_join(trend_colors)

base_deck <- 
  deckgl(initial_view_state = initial_view_state) %>% 
  add_mapbox_basemap("mapbox://styles/mapbox/light-v9") %>% 
  add_source( id = 'shps', data = allotment_shps)

make_props <- function( type, unit, ... ){ 
  
  color_by <- paste( 'color', type, unit, sep = '_')
  
  props <- list( 
    pickable = TRUE, 
    filled = TRUE,
    stroked = TRUE,
    lineWidthUnits = 'pixels', 
    getPolygon = ~ geometry,   
    lineWidthScale = 1,
    lineWidthMinPixels = 1,
    getLineWidth = 1,
    get_line_color = ~ eval(parse(text = paste('~', color_by))), 
    get_fill_color = eval(parse(text = paste('~', color_by))), 
    tooltip = '{{Name}}', 
    ... 
  )
  return(props)
}

Aprops <- make_props(type = 'Annual', unit = 'cover')
Pprops <- make_props( type = 'Perennial', unit = 'cover')

base_deck <- 
  base_deck %>% 
  add_geojson_layer(data = .$x$calls[[1]]$args$data, properties = Aprops) %>% 
  add_geojson_layer(data = .$x$calls[[1]]$args$data, properties = Pprops) 

save( base_deck, file = 'app/data/base_deck.rda')
