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

allotment_timeseries_plotly <- function( temp_data, choices, fig_pars ){ 
  
  choices['scale'] <- str_replace( choices['scale'] , ' ', '_')
  
  scale_vars <- paste0( c( 'median_', 'lq_', 'uq_') , choices['scale'])
  #scale_vars <- str_replace( scale_vars, ' ', '_')
  
  temp_data %>% 
    plot_ly( x = ~ year, 
             y = ~ eval(parse( text = scale_vars[1])), 
             type = 'scatter', 
             mode = 'lines', 
             name = paste( choices['scale'], 'median'),
             text = ~ eval(parse(text = choices['scale'])),   
             color = I("darkorange"), 
             hovertemplate = paste(
               "<br>",
               fig_pars$hoverformat[choices['unit']],
               "<extra></extra>")) %>%  
    add_ribbons(x = ~ year, 
                ymin = ~ eval(parse( text = scale_vars[2])) , 
                ymax = ~  eval(parse( text = scale_vars[3])), 
                hoverlabel = F, 
                hovertemplate = NA, 
                showlegend = T,
                name = paste( choices['scale'], 'IQR'), 
                fillcolor = 'rgba(254, 196, 79, 0.6)', 
                line = list( color = 'rgba(0, 0, 1, 00)')) %>% 
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
      yaxis = list(title = fig_pars$y_title[choices['unit']], 
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



empty_plot <- function(title = NULL) {
  
  plotly_empty(type = "scatter", mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = title,
                        yref = "paper",
                        y = 0.5))
  
}

