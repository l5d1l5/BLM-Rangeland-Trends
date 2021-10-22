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
##############
##############

load('data/mapdata.rda') 

#load('app/data/mapdata.rda')
##############
##############

plotly_hoverformat <-  "%{text}: %{y:.0f}"

empty_plot <- function(title = NULL) {
  
  plotly_empty(type = "scatter", mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = title,
                        yref = "paper",
                        y = 0.5))
  
}

##############
##############
table_html <-
  '<table style="width:100%%">
<tr>
<th><span style="float:left"> %s (%i) </span><br/></th>
</tr>
<tr>
<td><span style="float:left"> Type </span><br/></td>
<td><span style="float:right"> Cover </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> AFGC </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> BG </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> LTR </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> PFGC </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> TREE </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
</table>'

LA_county_link <-
  "https://rangelands.app/"
latest_update_link <-
  "https://rangelands.app/"


about <-
  '<!DOCTYPE html>
<b>ABOUT:</b>
<br></br>
<body style="width:70%%"><P>
Grassland cover in BLM grazing allotments. Click on a grazing allotment on the 
map see an annual timeseries of cover in that grazing allotment. Select one of 
the plant cover categories above.  Cover data is derived from the <a href = "https://rangelands.app/">Rangeland Analysis Platform</a>.
<P>
<td>
<center>#####</center><br>
</td>
</body>
</html>'


last_year <- max(cover$year)
first_year <- min(cover$year)

main_title <- "Rangeland Cover Trends in BLM Allotments"
subtitle <- paste("Updated on", last_year)
map_title <- "BLM Grazing Allotments"
plotly_title <- "Selected allotment (click on map)"
map_legend_title <- "Cover Trend (% per year)"

x_title <- "Year"
y_title <- "Cover"
x_range <- c(first_year, last_year + 1 )
default_name <- ''

# Create Choices List for Admin Office 
office_list <- unique( MT_allotments$ADM_OFC_CD) 
names(office_list) <- office_list

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
                  choices = unique( cover$type ), 
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
    
    map_data <- 
        MT_allotments %>% 
        # filter( ADM_OFC_CD %in% input$ADM_OFC_CD ) %>% 
        rename( label = ALLOT_NAME ) %>% 
        rename('trend' = input$type ) %>%
        select( uname, label, acres, trend ) 
    
    centroid <- 
      map_data %>% 
      st_combine() %>% 
      st_centroid() %>% 
      st_coordinates()

    label_df <-
      map_data %>%
      st_drop_geometry() %>% 
      select( uname, label, acres, trend) %>% 
      left_join( cover %>% 
                   filter( year == last_year ) %>% 
                   spread( type, cover )  , 
                 by = 'uname') 
    
    labels <- sprintf(
      table_html,
      label_df$label, 
      as.numeric(label_df$year),
      as.numeric(label_df$AFGC), 
      as.numeric(label_df$BG), 
      as.numeric(label_df$LTR), 
      as.numeric(label_df$PFGC), 
      as.numeric(label_df$TREE)) %>% 
      lapply(htmltools::HTML)
    
    pal_cover <-colorNumeric(palette= "RdBu",
                         domain = map_data$trend,
                         na.color = 'lightgrey')
    map_data %>%
      leaflet() %>%
      setView(centroid[1], centroid[2], zoom = 7) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        color = "black",
        weight = 0.5,
        fillColor = ~ pal_cover(trend),
        fillOpacity = 0.5,
        highlight = highlightOptions(
          fillColor = "Cyan",
          fillOpacity = 0.8,
          bringToFront = TRUE
        ),
        group = "Cover Trends",
        layerId = ~ uname,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "5px 10px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal_cover,
        na.label = "No Data",
        values = map_data$trend,
        title =  map_legend_title,
        opacity = 0.8
      )
  })
  
  id <-  eventReactive(input$map_shape_click, {
    input$map_shape_click$id
  })
  
  # Community Plot
  output$timeseries <- renderPlotly({
    
    if( !is.null(id()) ){
      
      temp_name <- 
        MT_allotments %>% 
        filter( uname == id()) %>% 
        pull('ALLOT_NAME')

      cover %>%
        filter( uname == id()) %>% 
        group_by( type ) %>% 
        plot_ly( x = ~ year, y = ~ cover, text = ~ type) %>%
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
