rm(list = ls())

library(tidyverse)
library(googledrive)
library(plotly)
library(leaflet)
library(sf)
# googledrive::drive_download(file = 'RAP_EE_exports/MT_BLM.csv', 
#                             path = 'data/cover_median.csv', 
#                             overwrite = T)


line_plot <- function( my_data, 
                       cover_category, 
                       x_title, 
                       x_range, 
                       hover_format = "%{yaxis.title.text}: %{y:.0f}<br>"  ) { 
  
  my_data %>%
    filter(type == cover_category ) %>%
    plot_ly(
      x = ~ year,
      y = ~ cover,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~ year,
      hovertemplate = paste(
        "<b>%{text}</b><br><br>",
        hoverformat,
        "<extra></extra>"),
      marker = list(size = 8, color = "red"),
      line = list(
        shape = "linear",
        dash = "dot",
        width = 3,
        color = "red"
      )
    ) %>%
    layout(
      xaxis = list(
        title = x_title,
        showgrid = FALSE,
        autotick = T,
        range = x_range
      ),
      yaxis = list(title = cover_category, 
                   showgrid = FALSE, 
                   rangemode = "tozero")
    ) %>%
    config(displayModeBar = F)
}

BLM_shapes <- readRDS('output/BLM_cleaned_shape_sf.RDS') %>% st_transform(4326)

cover <- 
  read_csv('data/MT_mean_allotment_cover.csv') %>% 
  select( AFGC:year) %>% 
  pivot_longer( cols = AFGC:TREE, names_to = 'type', values_to = 'cover')


my_variable <- 'afg'
my_allotment <- 'BAD RIVER'
x_title <- 'year'
x_range <- range( cover$year)
y_title <- 'cover'
hoverformat = "%{yaxis.title.text}: %{y:.0f}<br>"

line_plot(cover %>% 
            filter(uname == 6070), 'AFGC', x_title = 'year', x_range = c(2000, 2010)) 

BLM_shapes %>%  
  filter( ADMIN_ST == "MT") %>%
  mutate( label = paste( ALLOT_NAME )) %>% 
  #filter( ALLOT_NA == 'MUSSELSHELL TRAIL') %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    color = "black",
    weight = 1,
    fillColor = NA,
    popup = ~ label, 
    highlight = highlightOptions(
      color = 'Cyan', 
      fillColor = 'Red',
      fillOpacity = 0.5,
      weight = 2, 
      bringToFront = T
    ))

# cover %>%
#   filter( ALLOT_NA == my_allotment)  %>% 
#   filter( var == my_variable) %>% 
#   plot_ly(
#     x = ~ year,
#     y = ~ value,
#     type = 'scatter',
#     mode = 'lines+markers',
#     text = ~ my_allotment,
#     hovertemplate = paste(
#       "<b>%{text}</b><br><br>",
#       hover_format,
#       "<extra></extra>"),
#     marker = list(size = 8, color = "black"),
#     line = list(
#       shape = "linear",
#       dash = NA,
#       width = 3,
#       color = "gray"
#     )
#   ) %>%
#   layout(
#     xaxis = list(
#       title = x_title,
#       showgrid = FALSE,
#       autotick = T,
#       range = x_range
#     ),
#     yaxis = list(title = my_variable, 
#                  showgrid = FALSE, 
#                  rangemode = "tozero")
#   ) %>%
#   config(displayModeBar = F)



#fillOpacity = 0.5,popup = ~X, highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE))
# %>%
    # addLegend(
    #   position = "bottomleft",
    #   pal = 'red',
    #   na.label = "No Data",
    #   values = map_data$`Cases per thousand`,
    #   title = map_legend_title,
    #   opacity = 0.8
    # )
    # 
