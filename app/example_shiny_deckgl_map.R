rm(list =ls())
library(shiny)
library(deckgl)

load('data/layer_data.rda')
source('functions.R')

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

.app = reactiveValues(visible = TRUE)

view <- fluidPage(
  h1("deckgl for R"),
  selectInput("type", label = NULL, choices = c("Annual", "Perennial"), selected = 'Annual'),
  deckglOutput("deck"),
  tableOutput("selected"),
  style = "font-family: Helvetica, Arial, sans-serif;"
)

backend <- function(input, output) {
  
  output$deck <- renderDeckgl({
    base_deck   
  })
  
  observeEvent(input$deck_onclick, {
    info <- input$deck_onclick
    #print(info)
    object <- info$object
    # print(info)
    print(object$uname)
  })

  observeEvent(input$type, {
    
    deckgl_proxy("deck") %>%
      add_(allotment_shps, 
                      choices = choices, 
                      visible = .app$visible) %>%
      update_deckgl(it = "works")
  })
  
  id <- eventReactive(input$deck_onclick, {
    input$deck_onclick$object[['uname']]
  })
  
  output$selected <- renderTable({
    id()
  })
}

if (interactive()) shinyApp(view, backend)
