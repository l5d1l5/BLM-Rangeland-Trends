format_ts_data <- function(map_data, 
                           veg_data, 
                           ecoregion_veg, 
                           district_veg, 
                           field_office_veg, 
                           choices){  
  
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

allotment_timeseries_plotly <- function( temp_data, choices, fig_pars ){ 
  
  choices['scale'] <- str_replace( choices['scale'] , ' ', '_')
  
  scale_vars <- paste0( c( 'median_', 'lq_', 'uq_') , choices['scale'])
  #scale_vars <- str_replace( scale_vars, ' ', '_')
  
  temp_data %>% 
    plot_ly() %>%
    add_ribbons(x = ~ year, 
                y = ~ eval(parse(text = scale_vars[1])), 
                ymin = ~ eval(parse( text = scale_vars[2])) , 
                ymax = ~  eval(parse( text = scale_vars[3])), 
                hoverlabel = F, 
                hovertemplate = NA, 
                showlegend = T,
                name = paste( choices['scale'], 'IQR'), 
                fillcolor = fig_pars$my_colors_rgba[choices['type']], 
                line = list( color = 'rgba(1, 1, 1, 00)')) %>% 
    add_lines( x = ~ year, 
              y = ~ eval(parse( text = scale_vars[1])),
              name = paste( choices['scale'], 'median'),
              text = ~ eval(parse(text = choices['scale'])),
              line = list(color = fig_pars$my_colors[choices['type']]),
              hovertemplate = paste(
                          "<br>",
                          fig_pars$hoverformat[choices['unit']],
                          "<extra></extra>") ) %>%
    add_lines( x = ~ year, y = ~ value, 
               text = ~ Name, 
               name = 'Allotment', 
               color = I("black"), 
               hovertemplate = paste(
                 "<br>",
                 fig_pars$hoverformat[choices['unit']],
                 "<extra></extra>")) %>% 
    layout( 
      xaxis = list(
        title = fig_pars$x_title,
        showgrid = FALSE,
        autotick = T,
        range = fig_pars$x_range
      ),
      yaxis = list(
        title = paste( choices['type'], fig_pars$y_title[choices['unit']]), 
                   showgrid = FALSE,
                   showline = T, 
                   rangemode = "tozero"), 
      title = list(
        text = fig_pars$temp_allotment_name,
        x = 0.05,
        y = 1,
        xref = "paper",
        yref = 'paper'
      )) %>%
    config(displayModeBar = F)
}



empty_plot <- function(title = NULL) {
  
  plotly_empty(type = "scatter", mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = title,
                        yref = "paper",
                        y = 0.5))
  
}


allotment_map <- function( ctrs, shps , choices ){ 
  
  trend_choice <- paste( choices['type'], 
                         choices['unit'], sep = '_')
  
  rng <- ctrs %>% 
    pull(eval(parse( text = trend_choice))) %>% 
    range(na.rm = T)
  
  pal <- colorNumeric(
    palette = c('blue', 'yellow', 'red'),
    domain = rng, 
    na.color = NA)
  
  legend_label <- 
    paste( 
    paste(
      choices['type'], choices['unit']
      ), 
    'trend') 
  
  print(legend_label)
  
  ctrs %>% 
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 11)) %>%
    setView(center_ll[1], center_ll[2], zoom = 5) %>% 
    setMaxBounds( lng1 = bounds[1], 
                  lat1 = bounds[2], 
                  lng2 = bounds[3], 
                  lat2 = bounds[4]) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addCircles(
      layerId = ~ uname, 
      color = ~ pal( eval( parse( text = trend_choice))), 
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
        direction = "auto")
      ) %>% 
    addLegend("bottomright",
              pal = pal,
              values = ~ eval( parse( text = trend_choice)),
              title = legend_label,
              labFormat = labelFormat(digits = 2),
              opacity = 0.8, na.label = 'Gray') %>%
    groupOptions(group = 'Centers', zoomLevels = 4:8) %>% 
    groupOptions(group = 'Shapes', zoomLevels = 8:12) %>%
    addPolygons( 
       data = shps, 
       layerId = ~uname, 
       color = ~ pal( eval( parse( text = trend_choice))),       
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
      ))

}

add_trend_layer <- function(obj, type, unit = 'cover', ... ){ 
  
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
    tooltip = '{{Name}}\\n{{District}}{{Field Office}}{{Ecoregion}}', 
    ... 
  )
  
  
  obj %>% 
    add_geojson_layer(properties = props)
  
}   

getRGB <- function( x, ramp, lower, upper, alpha = 0.5) { 
  if( !is.na( x)) { 
    rescaled <- ( x - lower)/( upper - lower )   
    c( ramp(rescaled), alpha*255 )  
  }else{ 
    c( col2rgb('lightgray'), alpha*255) 
  }
}

