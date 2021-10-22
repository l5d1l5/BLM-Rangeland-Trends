# app with template
rm(list = ls())
library(tidyverse)
library(leaflet)
library(sf)
library(shiny)
library(plotly)
library(shinydashboard)
library(deckgl)
##############
##############

load('data/mapdata.rda')
load('data/vegdata.rda')
load('data/layer_data.rda')

source('functions.R')
source('parameters.R')
#source('app/deckgl_map.R')

header <- dashboardHeader(title = 'BLM Allotments')
sidebar <- dashboardSidebar()
body <- dashboardBody(# Boxes need to be put in a row (or column)
  fluidRow(column(width = 7,
                  wellPanel(
                    deckglOutput("rdeck",  width = "100%", height = "551px")
                  ))
           ,
           column(
             width = 5,
             wellPanel(
               selectInput(
                 "type",
                 "Select Vegetation Type:",
                 choices = unique(veg$type_label),
                 selected = "Annual",
                 multiple = F
               ),
               selectInput(
                 "unit",
                 "Select Unit",
                 choices = unique(veg$unit),
                 selected = 'cover',
                 multiple = F
               ),
               selectInput(
                 "scale",
                 "Select Scale of Comparison",
                 choices = c('Ecoregion', 'District', 'Field Office'),
                 selected = 'Ecoregion',
                 multiple = F
               ),
               h4(plotly_title),
               plotlyOutput("timeseries",
                            height = "364px")
             )
           )))

ui <- dashboardPage(
  header,
  sidebar,
  body)

server <- function(input, output) {
  
  output$rdeck <- renderDeckgl({
    
    choices <- c(unit = input$unit, 
                 type = input$type, 
                 scale = input$scale)
    
    base_deck %>% 
      add_trend_layer(shps = allotment_shps, choices)
    
  })
  
  observeEvent(input$deck_onclick, {
    info <- input$deck_onclick
    object <- info$object
    # print(info)
    print(object$points %>% length())
    print(names(object))
  })
  
  
  id <-  eventReactive(input$deck_onclick, {
    
    print(  input$deck_onclick$uname )
    
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
      
      temp_data <- format_ts_data(allotment_ctrs, 
                                  veg, 
                                  ecoregion_veg, 
                                  district_veg, 
                                  field_office_veg, 
                                  choices ) 
      
      temp_allotment_name <- 
        temp_data %>% 
        distinct(Name) %>% 
        pull(Name)
      
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

shinyApp(ui, server)
