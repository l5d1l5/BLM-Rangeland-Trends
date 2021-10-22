########

rm(list = ls())
library(tidyverse)
library(leaflet)
library(sf)
library(RColorBrewer)
library(viridis)
library(shiny)
library(plotly)
library(htmlwidgets)
library(leafpop)

##############
##############

load('data/mapdata.rda') 
load('data/vegdata.rda')

# load('app/data/mapdata.rda')
# load('app/data/vegdata.rda')

##############
##############
main_title <- "Rangeland Cover Trends in BLM Allotments"
subtitle <- paste("Updated on", last_year)
map_title <- "BLM Grazing Allotments"
plotly_title <- "Selected allotment (click on map)"

x_title <- "Year"
y_title <- c( cover = "Cover (%)", production = 'Production (kg/ha)')

x_range <- c(first_year, last_year + 1 )
default_name <- ''

hoverformat <- c(cover = "%{text}: %{y:.0f}%", 
                 production = "%{text}: %{y:0f} kg/ha")


empty_plot <- function(title = NULL) {
  
  plotly_empty(type = "scatter", mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = title,
                        yref = "paper",
                        y = 0.5))
  
}

# Filter Selections 
# These will be drop downs populated by 
# Values in veg_data 
uname_choice <- 10000
unit_choice <- 'production'
type_choice <- 'Annual'
scale_choice <- 'Ecoregion'

choices2 <- c(uname = uname_choice, unit = unit_choice, type = type_choice, scale = scale_choice)

format_ts_data <- function(map_data, veg_data, choices){  

  selected_allotment <- 
    map_data %>% 
    st_drop_geometry() %>% 
    filter( uname == choices['uname'] ) 

  temp_veg <- veg_data %>% 
    filter( type_label == choices['type'], unit == choices['unit'])
    
  # Joins
  temp_data <- 
    selected_allotment %>% 
    left_join(temp_veg, by = 'uname') 
  
  temp_data <- temp_data %>% 
    left_join( ecoregion_veg, 
               by = c('Ecoregion', 'type_label', 'unit', 'year')) %>% 
    left_join( district_veg, suffix = c('_Ecoregion', '_District'), 
                 by = c('District', 'type_label', 'unit', 'year')) %>% 
    left_join( field_office_veg, 
               by = c( 'Field Office', 'type_label', 'unit', 'year')) %>% 
    rename( "Field_Office" = `Field Office`, 
            "median_Field_Office" = median, 
            "lq_Field_Office" = lq ,
            "uq_Field_Office" = uq)
  return(temp_data)
} 

temp_data <- format_ts_data(map_data , veg, choices2)

temp_allotment_name <- 
  temp_data %>% 
  distinct(Name) %>% 
  pull(Name)

temp_Ecoregion <- 
  temp_data %>% 
  distinct( Ecoregion) %>% pull(Ecoregion)

temp_District <- 
  temp_data %>% 
  distinct( District) %>% pull(District) 

temp_field_office <- 
  temp_data %>% 
  distinct( `Field_Office`) %>% pull( `Field_Office`)

# Choose comparison scale  
# These will be user selected to display 
# ecoregion, district or field office aggregation of data 

fig_pars2 <- list( 
  hoverformat = hoverformat, 
  x_title = x_title, 
  x_range = x_range, 
  y_title = y_title, 
  temp_allotment_name = temp_allotment_name)


allotment_timeseries_plotly <- function( temp_data, choices, fig_pars ){ 
  
  scale_vars <- paste0( c( 'median_', 'lq_', 'uq_') , choices['scale'])
  
  temp_data %>% 
    plot_ly( x = ~ year, 
             y = ~ eval(parse( text = scale_vars[1])), 
             type = 'scatter', 
             mode = 'lines', 
             name = paste( scale_choice, 'median'),
             text = ~ eval(parse(text = scale_choice)),   
             color = I("darkorange"), 
             hovertemplate = paste(
               "<br>",
               fig_pars$hoverformat[unit_choice],
               "<extra></extra>")) %>%  
    add_ribbons(x = ~ year, 
                ymin = ~ eval(parse( text = scale_vars[2])) , 
                ymax = ~  eval(parse( text = scale_vars[3])), 
                hoverlabel = F, 
                hovertemplate = NA, 
                showlegend = T,
                name = paste( scale_choice, 'IQR'), 
                fillcolor = 'rgba(254, 196, 79, 0.6)', 
                line = list( color = 'rgba(0, 0, 1, 00)')) %>% 
    add_lines( x = ~ year, y = ~ value, 
               text = ~ Name, 
               name = 'Allotment', 
               color = I("black"), 
               hovertemplate = paste(
                 "<br>",
                 fig_pars$hoverformat[unit_choice],
                 "<extra></extra>")) %>% 
    layout( 
      xaxis = list(
        title = fig_pars$x_title,
        showgrid = FALSE,
        autotick = T,
        range = fig_pars$x_range
      ),
      yaxis = list(title = fig_pars$y_title[unit_choice], 
                   showgrid = FALSE,
                   showline = T, 
                   rangemode = "tozero"), 
      title = list(
        text = fig_pars$temp_allotment_name,
        x = 0.1,
        y = 0.9,
        xref = "paper",
        yref = 'paper'
      )) %>%
    config(displayModeBar = F)
}

allotment_timeseries_plotly(temp_data, choices2, fig_pars2 )
