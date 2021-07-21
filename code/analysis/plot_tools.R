# plot functions 

require( tidyverse) 

my_colors0 <- c('sienna1', 'black','8dd3c7','#fb8072','#80b1d3', 'magenta' )
names(my_colors0) <- c("Annual", "Bare", "Perennial", "Shrub", "Tree", "Litter")

# lbs /acre to kg/ha
kgHa <- 1.12085

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
  'Forested Mts', 
  'AZ/NM Highlands', 
  'Warm Deserts')

# For relabeling figures with shorter labels 
ecogroup_labels = c('AZ/NM\nHighlands', 
                    'E Cold Deserts', 
                    'Great Plains', 
                    'Mediterranean\nCalifornia', 
                    'Forested Mts', 
                    'W Cold Deserts',
                    'Warm Deserts')

group_variance_colors <- c('#f1b6da','cornsilk4','#b8e186','#4dac26','cadetblue3')
names(group_variance_colors) <- c('District', 'Field Office', 'Allotment', 'Year', 'Residual')


# GLMER/LMER options 
control_glmer = glmerControl(optimizer = "optimx", 
                             calc.derivs = FALSE,
                             optCtrl = list(method = "nlminb", 
                                            starttests = FALSE, 
                                            kkt = FALSE, 
                                            maxit = 10000))

control_lmer = lmerControl(optimizer = "optimx", 
                           calc.derivs = FALSE,
                           optCtrl = list(method = "nlminb", 
                                          starttests = FALSE, 
                                          kkt = FALSE))

my_control =
  lmerControl(
    optimizer = 'optimx',
    optCtrl = list(
      method = 'nlminb',
      eval.max = 1e3,
      iter.max = 1e3
    )
  )


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

plot_trend_coefficients <- function(beta_table, my_colors) { 
  # beta_table = table of trend coefficients for each ecogroup 
  beta_table %>% 
    ggplot( aes( x = type, 
                 y = year2.trend, 
                 ymin = asymp.LCL, 
                 ymax = asymp.UCL, 
                 color = type )) + 
    geom_point() + 
    geom_errorbar() + 
    geom_hline(aes(yintercept = 0 ), linetype = 2) + 
    facet_grid( ~ ecogroup ) + 
    scale_color_manual(name = 'Functional Type', 
                       values = my_colors) + 
    scale_y_continuous(name = 'Annual Rate') + 
    theme_bw() +   
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank())
}

plot_trend_coefficients_vertical <- function(my_trend_table, my_colors ) { 
  
  my_trend_table %>%   
    ggplot(aes( x = year2.trend, y = type, color = type  )) + 
    geom_point() + 
    geom_errorbar(aes( xmin = asymp.LCL, xmax = asymp.UCL)) + 
    geom_vline( aes( xintercept = 0 ), linetype = 2, alpha = 0.5) + 
    facet_grid( ecogroup ~ . , switch = 'y') + 
    scale_x_continuous(name = 'Trend Coefficient') + 
    scale_color_manual(values = my_colors , guide = guide_legend(reverse = T)) + 
    theme_bw() + 
    theme(axis.text.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text.y.left = element_text( angle = 0), 
          legend.title = element_blank())
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
  
  if( all( !is.null( nrow( group_trends$allot_effects )), 
           !is.null( nrow( group_trends$office_effects)), 
           !is.null( nrow( group_trends$dist_effects))) ){ 
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
  }else if( all( !is.null( nrow( group_trends$allot_effects )), 
                 !is.null( nrow( group_trends$office_effects)))){ 
    my_data %>% 
      distinct(ecogroup, office_label, uname) %>%
      mutate( ecogroup_trend = ecogroup_trends[ecogroup]) %>%
      left_join( group_trends$office_effects, by = 'office_label') %>% 
      left_join( group_trends$allot_effects, by = 'uname') %>%
      dplyr::select( uname, ecogroup, office_label, 
                     ecogroup_trend, office_trend, allot_trend ) %>%
      rowwise() %>% 
      mutate( full_trend = ecogroup_trend + office_trend + allot_trend )
  }else if( all( !is.null( nrow( group_trends$district_effects )), 
                 !is.null( nrow( group_trends$office_effects)))){
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

back_trans_frames <- function(m, type = "NAME"){ 
  dat <- m@frame
  dat$value <- back_transform( dat$value2, attributes_obj = attributes(dat$value2))
  dat$year <- back_transform( dat$year2, 
                              attributes_obj = c(attributes(dat$year2), 'scaled:scale' = 1), log = F)
  dat$type <- type 
  return( dat )   
}



scale_comparison_df <- function( x ) { 
  
  x %>% 
    distinct(type, uname, allot_trend) %>%
    pivot_wider(names_from = type, values_from = allot_trend ) %>% 
    mutate( scale = 'Allotment') %>% 
    rename( label = uname) %>% 
    mutate( label = as.character(label)) %>% 
    bind_rows(
      x %>% 
        distinct(type, office_label, office_trend) %>%
        pivot_wider(names_from = type, values_from = office_trend ) %>% 
        mutate( scale = 'Field Office') %>%
        rename( label = office_label)
    ) %>% 
    bind_rows(
      x %>% 
        distinct(type, ecogroup, ecogroup_trend) %>%
        pivot_wider(names_from = type, values_from = ecogroup_trend ) %>% 
        mutate( scale = 'Ecoregion') %>% 
        rename( label = ecogroup)
    ) %>% 
    bind_rows(
      x %>% 
        distinct(uname, full_trend, type ) %>%
        pivot_wider(names_from = type, values_from = full_trend ) %>% 
        mutate( scale = 'Total') %>% 
        rename( label = uname) %>% 
        mutate( label = as.character( label )) 
    ) 
  
}

get_rhos <- function( x, vars = c('AFGC', 'PFGC') ) { 
  
  x %>% filter( scale != 'Total') %>% 
    group_by( scale ) %>% 
    summarise( rho = list(cor.test( eval(parse( text = vars[1])), 
                                    eval(parse( text = vars[2]))))) %>% 
    rowwise() %>%
    mutate( label  = paste0('r==', sprintf('%.2f', rho$estimate)))
  
}

make_corplot_by_scale <- function( x, types = c('AFG', 'BG') ){ 
  
  correlations <- 
    x %>% 
    filter(type %in% types) %>% 
    select(ecogroup, office_label, uname, type, allot_trend, 
           office_trend, ecogroup_trend, full_trend )
  
  scale_comparison <- scale_comparison_df(correlations)
  scale_rhos <- get_rhos(scale_comparison, vars = types)
  
  return( 
    scale_comparison %>% 
      filter( scale != 'Total' )  %>% 
      ggplot( aes_string( x = types[1], y = types[2])  ) + 
      geom_point() + 
      stat_smooth(method = 'lm', se = F, size = 0.5) + 
      geom_text( data = scale_rhos, 
                 aes( x = Inf, y = Inf, label = label), vjust = 2, hjust = 1.1, parse = T) +  
      facet_wrap( ~ factor( scale, levels = c('Ecoregion', 'Field Office', 'Allotment'), ordered = T), 
                  scale = 'free') +
      theme_bw() 
  )
}


ecogroup_detail_plot <- function( x, 
                                  sel_type = 'Bare', 
                                  sel_ecogroup = 'Warm Deserts', 
                                  my_colors = 'black'){ 
  
  
  title <- paste0(  paste( sel_type, sep = '&' ), 'Cover: ', 
                   paste( sel_ecogroup))   
  
  
  if(length(sel_type) > 1 ){   
    
  gg_out <- x %>% 
      filter( type %in% sel_type, 
              ecogroup == sel_ecogroup ) %>% 
      ggplot( aes( x = year, y = value, color = type )) + 
      geom_line(size = 0.2, alpha = 0.5, aes( group = paste(uname, type))) + 
      scale_y_log10(name = 'Cover (%)') + 
      scale_x_continuous(breaks = c(1995, 2005, 2015)) + 
      scale_color_manual(values = my_colors) + 
      ggtitle(title) + 
      theme_bw() 
    
  }else if( length(sel_type) == 1 ){
  gg_out <- x %>% 
      filter( type == sel_type, ecogroup == sel_ecogroup ) %>% 
      ggplot( aes( x = year, y = value, group = uname)) + 
      geom_line(size = 0.2, alpha = 0.5) + 
      scale_y_log10(name = 'Cover (%)') + 
      scale_x_continuous(breaks = c(1995, 2005, 2015)) + 
      ggtitle(title) + 
      theme_bw()
  }
  if( n_distinct( gg_out$data$district_label) > 2 ){
    gg_out <- 
      gg_out + 
      facet_wrap( ~ district_label)
  }
  
  return( gg_out )
}
