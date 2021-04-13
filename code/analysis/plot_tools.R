# plot functions 

require( tidyverse) 

my_colors0 <- c('sienna1', 'black','8dd3c7','#fb8072','#80b1d3', 'magenta' )
names(my_colors0) <- c("Annual", "Bare", "Perennial", "Shrub", "Tree", "Litter")

my_colors <- c( 'Annual' = '#b2df8a', 
                'Bare'  = 'darkgray',
                'Perennial' = '#1f78b4', 
                'Shrub' = '#fb9a99', 
                'Tree' = '#33a02c')



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
    sd*x - center 
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
