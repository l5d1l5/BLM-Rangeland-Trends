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


#load('app/data/mapdata.rda')
##############
##############
main_title <- "Rangeland Cover Trends in BLM Allotments"
subtitle <- paste("Updated on", last_year)
map_title <- "BLM Grazing Allotments"
plotly_title <- "Selected allotment (click on map)"
map_legend_title <- "Cover Trend (% per year)"

x_title <- "Year"
y_title <- "Cover"
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


plotly_hoverformat <-  "%{text}: %{y:.0f}"


empty_plot <- function(title = NULL) {
  
  plotly_empty(type = "scatter", mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = title,
                        yref = "paper",
                        y = 0.5))
  
}


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
      selectInput("type", "Select Cover Type for Map:", 
                  choices = unique( veg$type ), 
                  selected = "AFGC", multiple = F),
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
    
    #input <- list( type = 'AFGC', ADM_OFC_CD = 'B01000', year = 2000)
    
  
    
    # pal_cover <-colorNumeric(palette= "RdBu",
    #                          domain = map_data$trend,
    #                          na.color = 'lightgrey')
    
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
      
      temp_name <- 
        allotment_shps %>% 
        filter( uname == id()) %>% 
        pull('Name')
      
      veg %>%
        filter( uname == id()) %>% 
        group_by( type ) %>% 
        plot_ly( x = ~ year, y = ~ values, text = ~ type) %>%
        add_lines( color = ~ type,
                   hovertemplate = paste(
                     "<br>",
                     plotly_hoverformat,
                     "<extra></extra>")) %>% 
        layout( 
          xaxis = list(
            title = x_title,
            showgrid = FALSE,
            autotick = T,
            range = x_range
          ),
          yaxis = list(title = 'cover', 
                       showgrid = FALSE,
                       range = c(0, 100), 
                       rangemode = "tozero"), 
          title = list(
            text = temp_name,
            x = 0.1,
            y = 0.9,
            xref = "paper",
            yref = 'paper'
          )) %>%
        config(displayModeBar = F)
      
    }else{
      
      empty_plot(title = "Click on the map to display allotment data")
    }
  })
}


shinyApp(ui = ui, server = server)
