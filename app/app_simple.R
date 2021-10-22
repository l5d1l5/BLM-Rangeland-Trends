########
####

rm(list = ls())
library(tidyverse)
library(leaflet)
library(sf)
library(shiny)
library(plotly)

##############
##############

load('data/mapdata.rda')
load('data/vegdata.rda')
source('functions.R')
source('parameters.R')
# 
#load('app/data/mapdata.rda')
#load('app/data/vegdata.rda')
# source( 'app/functions.R')
# source( 'app/parameters.R')

#, "select all" = all_OFC_CD)
## --------------------------------------- #

ui <- fluidPage(
  tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  titlePanel(h2(main_title)),
  fluidRow(  
    column(width = 7, wellPanel(
      leafletOutput("map",  width = "100%", height = "551px")
    )),
    column( width = 5, wellPanel(
      selectInput("type", "Select Vegetation Type:", 
                  choices = unique( veg$type_label), 
                  selected = "Annual", multiple = F),
      selectInput("unit", "Select Unit", 
                  choices = unique(veg$unit), 
                  selected = 'cover', multiple = F), 
      selectInput("scale", "Select Scale of Comparison", 
                  choices = c('Ecoregion', 'District', 'Field Office'), 
                  selected = 'Ecoregion', multiple = F), 
      h4(plotly_title),
      plotlyOutput("timeseries", 
                   height = "364px")
    )
  )),
  fluidRow(column(width = 12, wellPanel(
    htmlOutput("about_text")
  )))
)

##############
##############

server <- function(input, output) {
  
  output$about_text <- renderText({
    about
  })
  
  output$map <- renderLeaflet({
    allotment_map( map_data )
  })
  
  id <-  eventReactive(input$map_shape_click, {
    input$map_shape_click$id
  })
  
  last_year <- max(veg$year)
  first_year <- min(veg$year)
  x_range <- c(first_year, last_year + 1 )
  
  # Community Plot
  output$timeseries <- renderPlotly({
    
    if( !is.null(id()) ){
      
      choices <- c(uname = id(), 
                   unit = input$unit, 
                   type = input$type, 
                   scale = input$scale)
    
      # choices <- c(uname = 1000,
      #              unit = 'cover',
      #              type = 'Bare Ground',
      #              scale = 'Ecoregion')

      temp_data <- format_ts_data(map_data, veg, choices ) 
      
      temp_allotment_name <- 
        temp_data %>% 
        distinct(Name) %>% 
        pull(Name)
      
      # Choose comparison scale  
      # These will be user selected to display 
      # ecoregion, district or field office aggregation of data 
      
      fig_pars <- list( 
        my_colors = my_colors, 
        my_colors_rgba = my_colors_rgba, 
        hoverformat = hoverformat, 
        x_title = x_title, 
        x_range = x_range, 
        y_title = y_title, 
        temp_allotment_name = temp_allotment_name)
      
      allotment_timeseries_plotly(temp_data, choices, fig_pars)  
      
    }else{
      
      empty_plot(title = "Click on the map to display allotment data")
    }
  })
}


shinyApp(ui = ui, server = server)
