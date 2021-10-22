rm(list = ls())
library(leaflet)
library(leafgl)
library(sf)
library(shiny)
library(tidyverse)
library(plotly)

load('data/mapdata.rda')
load('data/vegdata.rda')

source('functions.R')

ui <- fluidPage(
  
  fluidRow(column(width = 7,
                  wellPanel(
                    leafglOutput("map",  width = "100%", height = "551px")
                  )),
           column(
             width = 5,
             
             wellPanel(
               plotlyOutput("timeseries", height = "364px")
             )
           )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet( { 
    
    leaflet(data =allotment_ctrs, 
            options = leafletOptions(minZoom = 4, maxZoom = 11)) %>%
      setView(lng = -118, lat = 40, zoom = 5) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addGlPoints(data = allotment_ctrs, 
                  layerId = ~ uname )
  
  })
  
  id <- eventReactive( input$map_shape_click, {
    input$map_shape_click$id
  })
  
  output$timeseries <- renderPlotly({

    if( !is.null( id() ) ){ 
    
    print(id() )
      
    temp <- 
      veg %>%
      filter( uname == id() ) %>% 
      filter( type_label == 'Annual', 
              unit == 'cover')
    

    plot_ly(data = temp ) %>%
      add_lines(x = ~ year, 
                y = ~ value, 
                text = ~ uname, 
                name = 'Allotment', 
                color = I("black"))
    }else{ 
      print(id())
      empty_plot(title = "Click on the map to display allotment data")
    }

  })
}

shinyApp(ui, server)
