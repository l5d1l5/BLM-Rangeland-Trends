# plot functions 

require( tidyverse) 

my_colors0 <- c('sienna1', 'black','8dd3c7','#fb8072','#80b1d3', 'magenta' )
names(my_colors0) <- c("Annual", "Bare", "Perennial", "Shrub", "Tree", "Litter")

my_colors <- c( 'Annual' = '#b2df8a', 
                'Bare'  = 'darkgray',
                'Perennial' = '#1f78b4', 
                'Shrub' = '#fb9a99', 
                'Tree' = '#33a02c')

WESTERN_STATES <- c('AZ', 'CA', 'CO', 'ID', 'NE', 'NV', 'NM',
                    'ND', 'OR', 'MT', 'SD', 'UT', 'WA', 'WY')

ecogroup_colors <- RColorBrewer::brewer.pal(n = 7, 'Set2')
names( ecogroup_colors  ) <- c(
  'W Cold Deserts', 
  'E Cold Deserts', 
  'Great Plains', 
  'Mediterranean California', 
  'NW Forested Mts', 
  'AZ/NM Highlands', 
  'Warm Deserts')


decade_function <- function(x ) { 
  paste0( 
    str_pad( 
      floor(( x %% 100 ) %/% 10 )*10, 
      width = 2, 
      side = 'left', 
      pad = '0'), 's')
}


plot_trends <- function( dataset, my_colors ){ 
  # plot annual state of allotment (e.g. cover, production) 
  # by year. 
  # Different colors for different types (e.g. annual, perennial)

  dataset %>% 
    ggplot( aes( x = year , y= value, fill = type, color = type)) + 
    stat_summary(fun.max = function(x) quantile(x, 0.75),
                 fun.min = function(x) quantile(x, 0.25), 
                 geom = 'ribbon', alpha = 0.4, color = NA) + 
    stat_summary(fun = function( x ) median(x), geom = 'line') + 
    scale_fill_manual(values = my_colors) +   
    scale_color_manual(values = my_colors) + 
    theme_bw() + 
    theme( axis.title.x =  element_blank() , legend.title = element_blank() ) 
}


back_transform <- function(x, attributes_obj, log = T ){ 
  
  center <- attributes_obj$`scaled:center`
  sd <- attributes_obj$`scaled:scale`
  
  if( log ){ 
    exp( (sd*x) + center) 
  }else{ 
    sd*x + center 
  }
}


plot_single_allotment_trends <- function( dataset, my_colors ){ 
  dataset %>% 
    ggplot( aes( x = year , y= value, fill = type, color = type)) + 
    geom_line() + 
    scale_color_manual(values = my_colors) + 
    theme_bw() + 
    theme( axis.title.x =  element_blank() , legend.title = element_blank() ) 
}


ecogroup_trends_as_df <- function(trend_model, type){ 
  
  trends <- emmeans::emtrends(trend_model, ~ ecogroup, var = 'year2') %>% 
    data.frame(type = type)
}

plot_ecogroup_trend_coefficients <- function( trend_summary, my_colors ){ 
  
  trend_summary %>%
    ggplot(aes( x = ecogroup, color = type, 
                y = year2.trend, ymin = asymp.LCL, ymax = asymp.UCL )) + 
    geom_point(position = position_dodge(width = 1)) + 
    geom_errorbar(position = position_dodge( width = 1)) + 
    geom_hline(aes( yintercept = 0), lty = 2, color = 'darkgray') + 
    scale_color_manual(values = my_colors, name = 'Trend') + 
    theme_bw() + 
    theme(axis.text.x =  element_text(angle = 45 , hjust = 1), axis.title.x = element_blank()) + 
    ylab( 'Annual Coefficient')    
  
}



# Functions for summarizing trends at the Ecogroup and BLM Admin Level
get_ecogroup_trends <- function( model ){ 
  
  fixeffects <- emtrends(model, ~ ecogroup, 'year2') %>% 
    as.data.frame()
  
  ecogroup_effect <- fixeffects$year2.trend
  names(ecogroup_effect) <-  c( str_trim( fixeffects$ecogroup ) )
  
  return( ecogroup_effect ) 
} 


get_blm_random_effects <- function( model ) { 
  
  dist_effects <- ranef(model)$district_label 
  office_effects <- ranef(model)$office_label
  allot_effects <- ranef(model)$uname
  
  out <- list( dist_effects, office_effects, allot_effects ) 
  names( out ) <- c("dist_effects", "office_effects", "allot_effects")
  
  out$allot_effects$uname <- as.integer( row.names( out$allot_effects ) )
  out$dist_effects$district_label <- row.names( out$dist_effects )
  out$office_effects$office_label <- row.names( out$office_effects)
  
  out$dist_effects$district_trend <- out$dist_effects$year2
  out$office_effects$office_trend <- out$office_effects$year2
  out$allot_effects$allot_trend <- out$allot_effects$year2
  
  return( out )
} 


blm_trend_summary <- function( my_data, ecogroup_trends, group_trends ){ 
  
  if( all( c('allot_effects', 'dist_effects', 'office_effects') %in% names(group_trends)) ){ 
    my_data %>% 
      distinct(ecogroup, district_label, office_label, uname) %>%
      mutate( ecogroup_trend = ecogroup_trends[ecogroup]) %>%
      left_join( group_trends$dist_effects, by = 'district_label') %>%
      left_join( group_trends$office_effects, by = 'office_label') %>% 
      left_join( group_trends$allot_effects, by = 'uname') %>%
      dplyr::select( uname, ecogroup, district_label, office_label, 
                     ecogroup_trend, district_trend, office_trend, allot_trend ) %>%
      rowwise() %>% 
      mutate( full_trend = ecogroup_trend + district_trend + 
                office_trend + allot_trend )
  }else{ 
    my_data %>% 
      distinct(ecogroup, district_label, office_label) %>%
      mutate( ecogroup_trend = ecogroup_trends[ecogroup]) %>%
      left_join( group_trends$dist_effects, by = 'district_label') %>%
      left_join( group_trends$office_effects, by = 'office_label') %>% 
      dplyr::select(ecogroup, district_label, office_label, 
                    ecogroup_trend, district_trend, office_trend) %>%
      rowwise() %>% 
      mutate( full_trend = ecogroup_trend + district_trend + office_trend )
  }
  
  
}
