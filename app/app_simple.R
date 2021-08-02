########
### TODO:  filter by BLM office 
### TODO:  Add forage production data 
### TODO:  Add date of peak forage
### TODO:  hover piechart on leaflet map 
####

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
source('functions.R')

# load('app/data/mapdata.rda')
# load('app/data/vegdata.rda')
# source( 'app/functions.R')

##############
##############

main_title <- "Rangeland Vegetation on BLM Allotments"
subtitle <- paste("Updated on", last_year)
map_title <- "BLM Grazing Allotments"
plotly_title <- "Selected allotment (click on map)"

hoverformat <- c(cover = "%{text}: %{y:.0f}%", 
                 production = "%{text}: %{y:0f} kg/ha")

x_title <- "Year"
y_title <- c( cover = "Cover (%)", production = 'Production (kg/ha)')

x_range <- c(first_year, last_year + 1 )
default_name <- ''

RAP_link <-
  "https://rangelands.app/"

latest_update_link <-
  "https://rangelands.app/"

about <-
  '<!DOCTYPE html>
<b>ABOUT:</b>
<br></br>
<body style="width:70%%"><P>
Grassland cover in BLM grazing allotments. Click on a grazing allotment on the 
map to see an annual timeseries of cover in that grazing allotment. Select one of 
the plant cover categories above.  Cover data is derived from the <a href = "https://rangelands.app/">Rangeland Analysis Platform</a>.
<P>
<td>
<center>#####</center><br>
</td>
</body>
</html>'




#, "select all" = all_OFC_CD)
## --------------------------------------- #

ui <- fluidPage(
  tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  titlePanel(h2(main_title)),
  titlePanel(h4(subtitle)),
  fluidRow(column(
    width = 5, wellPanel(
      # selectInput("ADM_OFC_CD", "Select BLM Districts:", 
      #             choices = office_list, 
      #             selected = c('L06000'), multiple = T),
      selectInput("type", "Select Vegetation Type:", 
                  choices = unique( veg$type_label), 
                  selected = "Annual", multiple = F),
      selectInput("unit", "Select Unit", 
                  choices = unique(veg$unit), 
                  selected = 'cover', multiple = F), 
      selectInput("scale", "Select Scale of Comparison", 
                  choices = c('Ecoregion', 'District', 'Field Office'), 
                  selected = 'Ecoregion', multiple = F), 
      # selectInput("year", "Select year:", 
      #             choices = unique(cover$year), 
      #             selected = 2000), 
      h4(plotly_title),
      plotlyOutput("timeseries", 
                   height = "364px")
    )
  ),
  column(width = 7, wellPanel(
    leafletOutput("map",  width = "100%", height = "551px")
  ))),
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
    
    center_ll <- c(-114.07, 42.04) 
    
    bounds <- c( -94.8, 27.9, 
                 -125.0, 49.1) 
    
    map_data %>%
      leaflet(options = leafletOptions(minZoom = 4, maxZoom = 11)) %>%
      setView(-114.07, 42.04, 
              zoom = 4
              ) %>% 
      setMaxBounds( lng1 = -94.8, 
                    lat1 = 27.9, 
                    lng2 = -125.0, 
                    lat2 = 49.1) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircles(
      layerId = ~ uname, 
      weight = 4, 
      highlight = highlightOptions(
        fillColor = "Cyan",
        fillOpacity = 0.8,
        bringToFront = TRUE
      ),
      group = "Centers",
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", "padding" = "5px 10px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>% 
    addPolygons( 
      data = allotment_shps, 
      layerId = ~uname, 
      highlight = highlightOptions(
        fillColor = "Cyan",
        fillOpacity = 0.8,
        bringToFront = TRUE
      ),
      weight = 2, 
      group = "Shapes",
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", "padding" = "5px 10px"),
        textsize = "15px",
        direction = "auto"
      )) %>% 
    groupOptions(group = 'Centers', zoomLevels = 4:10) %>% 
    groupOptions(group = 'Shapes', zoomLevels = 8:12)
  })
  
  id <-  eventReactive(input$map_shape_click, {
    input$map_shape_click$id
  })
  
  # Community Plot
  output$timeseries <- renderPlotly({
    
    if( !is.null(id()) ){
      
      choices <- c(uname = id(), 
                   unit = input$unit, 
                   type = input$type, 
                   scale = input$scale)
      
      scale_labs = c('Ecoregion' = 'Ecoregion', 'District' = 'District', 
                     `Field Office` = 'Field_Office')
      
      # choices <- c(uname = 1000,
      #              unit = 'cover',
      #              type = 'Annual',
      #              scale = 'Field Office')
      
      temp_data <- format_ts_data(map_data, veg, choices ) 
      
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
      
      fig_pars <- list( 
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
