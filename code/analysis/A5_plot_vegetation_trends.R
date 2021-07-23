rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(gridExtra )
library(ggpubr)
require(kableExtra)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

# cover trends:  ------------------------------------------ # 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_model_files, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Total', 'Perennial', 'Shrub', 'Tree'))
names( cover_models ) <- types 

trend_table_cover <- mapply( x = cover_models, y = types, FUN = function(x,y) ecogroup_trends_as_df(x, y), SIMPLIFY = F)
trend_table_cover <- do.call(rbind, trend_table_cover)

# Drop Total Herb Cover Type
trend_table_cover <- 
  trend_table_cover %>% 
  filter( type != 'Total') %>%
  mutate( type = factor(type ))

cover_models <- cover_models[ -which(names(cover_models) == 'Total') ]
#
cover_attribs <- lapply( cover_models, 
                         function( x ) { attributes( x@frame$value2)  } )

year2_attributes <- lapply(cover_models, function( x ) attributes( x@frame$year2 ))


trend_table_cover <- trend_table_cover %>% 
  mutate( ecogroup = factor( ecogroup, labels = ecogroup_labels)) %>%  
  left_join(
  data.frame( type = unique( trend_table_cover$type ), 
            scale = c(
              cover_attribs[[1]]$`scaled:scale`, 
              cover_attribs[[2]]$`scaled:scale`, 
              cover_attribs[[3]]$`scaled:scale`,             
              cover_attribs[[4]]$`scaled:scale`, 
              cover_attribs[[5]]$`scaled:scale`), 
            year2_scale = c( year2_attributes[[1]]$`scaled:scale`,
                            year2_attributes[[2]]$`scaled:scale`,
                            year2_attributes[[3]]$`scaled:scale`, 
                            year2_attributes[[4]]$`scaled:scale`, 
                            year2_attributes[[5]]$`scaled:scale`)), 
  by = 'type') %>%
  mutate( year2.trend = year2.trend*scale/year2_scale, 
          asymp.LCL = asymp.LCL*scale/year2_scale, 
          asymp.UCL = asymp.UCL*scale/year2_scale ) 


trend_table_cover$unit <- 'Cover' 

# Biomass Models and Trends--------------------------- # 
agb_model_files <- dir(path = 'output', pattern = '.*_agb_trend_model.rds', full.names = T)
agb_models <- lapply(agb_model_files, read_rds)
types  <- c( str_extract( basename( agb_model_files), pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Total', 'Perennial'))
trend_table <- mapply( x = agb_models, y = types, FUN = function(x,y) ecogroup_trends_as_df(x, y), SIMPLIFY = F)
trend_table <- do.call(rbind, trend_table)

# Transform rate to get back to raw un-scaled proportion increase 
# Multiply rates by the scale (sd) of log response 
response_attributes <- lapply( agb_models, function(x ) attributes( x@frame$value2) )
year2_attributes <- lapply(agb_models, function( x ) attributes( x@frame$year2 ))

trend_table_agb <- trend_table %>% 
  left_join( 
    data.frame(type = c( 'Annual', 'Total', 'Perennial'), 
               scale = c(response_attributes[[1]]$`scaled:scale`, 
                         response_attributes[[2]]$`scaled:scale`, 
                         response_attributes[[3]]$`scaled:scale`), 
               year2_scale = c( year2_attributes[[1]]$`scaled:scale`,
                                year2_attributes[[2]]$`scaled:scale`,
                                year2_attributes[[3]]$`scaled:scale`)), 
    by = 'type') %>% 
  mutate( year2.trend = year2.trend*scale/year2_scale, 
          asymp.LCL = asymp.LCL*scale/year2_scale , 
          asymp.UCL = asymp.UCL*scale/year2_scale)   %>% 
  mutate( ecogroup = factor( ecogroup, labels = ecogroup_labels)) 

trend_table_agb$unit <- 'Biomass'

# Plot Cover and Biomass Trends together 
trend_table <- 
  trend_table_cover %>% 
  bind_rows(trend_table_agb) %>%
  filter( type != 'Total') %>%
  mutate( unit = factor( unit, levels = c('Cover', 'Biomass'), ordered = T)) %>%
  mutate( type = factor(type, levels = sort(names(my_colors), decreasing = T), ordered = T)) %>% 
  mutate( sig = ifelse( (asymp.LCL > 0 | asymp.UCL < 0), "*", '' )) 

# Plot ------------------------- 
trend_table %>%
  plot_trend_coefficients_vertical(my_colors = my_colors ) + 
  facet_grid( ecogroup  ~ unit, switch = 'y', scales = 'free_x' ) + 
  theme( legend.title = element_text()) + 
  scale_color_manual(name = 'Functional Type', values = my_colors) + 
  scale_x_continuous(name = 'Trend Slope (+/- 95%CI)') + 
  ggsave(filename = 'output/figures/Fig_4_veg_trends_by_Ecoregion.png', 
         height = 8, width = 8, units = 'in', dpi = 'print') 


# trend_table_cover %>% 
#   mutate( type = factor(type, levels =c('Tree', 'Shrub', 'Perennial', 'Bare', 'Annual'), ordered = T)) %>%
#   bind_rows( (trend_table %>% mutate(unit = as.character(unit)))) %>% 
#   filter( type != 'Total') %>% 
#   mutate( unit = factor( unit, levels = c('Cover', 'Biomass'), ordered = T)) %>%
#   plot_trend_coefficients(my_colors = my_colors ) + 
#   facet_grid( unit ~ ecogroup, scales = 'free_y' ) + 
#   scale_y_continuous(name = 'Trend Slope') + 
#   ggsave(filename = 'output/figures/Fig_3_HORIZONTAL_veg_trends_by_Ecoregion.png', 
#        height = 5, width = 9, units = 'in', dpi = 'print') 
# 

#  Save Tables -------------------------------------------- # 
ttlist <- trend_table %>%
  split(f = .$type:.$unit ) 

ttlist <- ttlist[ which( unlist( lapply(ttlist, nrow ) ) != 0  ) ] 

for( i in names(ttlist)){ 
  
  ttlist[[i]] %>% kableExtra::kbl(digits = 3, caption = i ) %>% kableExtra::kable_classic_2() %>% 
    kableExtra::save_kable(file = file.path('output/tables/', 
                                paste0( i, '_trend_estimates.html')))
} 

# Plot observed and predicted over time 
## Plot Allotment Cover over time : 
dat <- mapply( x = cover_models, 
               y = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'), 
               FUN = function(x, y) back_trans_frames(x, y), 
               SIMPLIFY = F)

dat <- mapply( x = dat, y = cover_models, FUN = function( x, y ) { 
  x$yhat <- back_transform( predict( y, newdata = x, re.form = ~ (1|office_label)), attributes(y@frame$value2) )
  return(x)
}, SIMPLIFY = F)

dat <- do.call(rbind, dat)

dat2 <- dat %>% 
  mutate(Class = ifelse(type %in% c('Tree', 'Shrub'), 'Woody', 'Herbaceous')) %>% 
  mutate( ecogroup = factor(ecogroup, labels = c(ecogroup_labels)))

cover_plot <- dat2 %>% 
  ggplot(aes( x = year, y = value, color = type, fill = type , group = type )) + 
  stat_summary(fun.max = function(x) quantile(x, 0.75),
               fun.min = function(x) quantile(x, 0.25), 
               geom = 'ribbon', alpha = 0.4, color = NA) + 
  stat_summary(fun = function( x ) median(x), geom = 'line') + 
  facet_grid( ecogroup ~ Class) + 
  scale_color_manual(name = 'Vegetation Type', values = my_colors) + 
  scale_fill_manual(name = 'Vegetation Type', values = my_colors) + 
  theme_bw() + 
  scale_x_continuous(breaks = c(1995, 2005, 2015)) + 
  scale_y_continuous(limits = c(0,NA)) + 
  theme(strip.text.y = element_text(angle = 0)) + 
  ylab( 'Cover (%)')   + 
  xlab( 'Year')

# cover_plot %>% 
#   ggsave(filename = 'output/figures/Fig_2b_average_cover_over_time.png', 
#          dpi = 'print', height = 8, width = 7, units = 'in')
# 

# Plot AGB over time 
agb_types <- c('Annual Biomass', 'Herb Biomass', 'Perennial Biomass')
dat <- mapply( x = agb_models, 
               y = agb_types, 
               FUN = function(x, y) back_trans_frames(x, y), 
               SIMPLIFY = F)

dat <- mapply( x = dat, y = agb_models, FUN = function( x, y ) { 
  x$yhat <- back_transform( predict( y, newdata = x, re.form = ~ (1|office_label)), attributes(y@frame$value2) )
  return(x)
  }, SIMPLIFY = F)

dat <- do.call(rbind, dat)
ylab <- expression( Aboveground~Biomass~"("*kg~ha^-1*")")

my_colors <- c(my_colors, 'Total' = 'darkgray')

agb_biomass_plot <- dat %>% 
  #filter( type != 'Herb Biomass') %>% 
  mutate( type = factor( type , labels = c('Annual', 'Total', 'Perennial'))) %>% 
  mutate( value_kg_ha = value*kgHa, yhat_kg_ha = yhat*kgHa) %>%
  mutate( ecogroup  = factor(ecogroup, labels = ecogroup_labels)) %>%
  ggplot(aes( x = year, y = value_kg_ha, color = type, fill = type , group = type )) + 
  stat_summary(fun.max = function(x) quantile(x, 0.75),
               fun.min = function(x) quantile(x, 0.25), 
               geom = 'ribbon', alpha = 0.4, color = NA) + 
  stat_summary(fun = function( x ) median(x), geom = 'line') + 
  facet_grid(ecogroup ~ .) + 
  scale_color_manual(name = 'Vegetation Type', 
                     values = my_colors) + 
  scale_fill_manual(name = 'Vegetation Type', 
                    values = my_colors) + 
  theme_bw() + 
  theme( strip.text.y = element_text(angle = 0 )) + 
  scale_x_continuous(breaks = c(1995, 2005, 2015)) + 
  scale_y_log10() + 
  ylab( ylab)   + 
  xlab( 'Year')

agb_biomass_plot <- 
  agb_biomass_plot %+% 
  ( agb_biomass_plot$data %>% filter( type != 'Total'))  

# agb_biomass_plot %>% 
#   ggsave(filename = 'output/figures/Fig_2a_average_agb_over_time.png', 
#   dpi = 'print', height = 8, width = 6, units = 'in')

# agb_biomass_plot + 
#   facet_grid(. ~ ecogroup  ) + 
#   theme(axis.text.x  = element_text(angle = -45, hjust = 0 )) + 
#   ggsave(filename = 'output/figures/AGB_over_time_HORIZONTAL_no_trend.png',
#        dpi = 'print', height = 4, width = 10, units = 'in')

agb_biomass_plot_no_lines <- agb_biomass_plot 
agb_biomass_plot_no_lines$layers[[2]] <- NULL

preds <- agb_biomass_plot$data %>% 
  dplyr::select( year, type, ecogroup, office_label , yhat_kg_ha) %>% 
  distinct() 

# preds %>% 
#   ggplot( aes( x= year, y = yhat_kg_ha, group = office_label, color = type )) +
#   geom_line() + 
#   facet_grid( type ~ ecogroup )

# agb_biomass_plot_no_lines + 
#   stat_summary(aes( y = yhat_kg_ha) ,
#                fun = function(x) median(x),
#                geom = 'line', linetype = 2, size = 0.5) +
#     facet_grid(. ~ ecogroup  ) + 
#     theme(axis.text.x  = element_text(angle = -45, hjust = 0 )) + 
#   ggsave(filename = 'output/figures/AGB_over_time_HORIZONTAL_with_trend.png',
#        dpi = 'print', height = 4, width = 10, units = 'in')


# Make Horizontal plot for presentation 
# cover_plot + 
#   facet_grid( Class ~ ecogroup, scales = 'free_y') + 
#   theme( axis.text.x = element_text(angle = -45, hjust = 0)) + 
#   ggsave(filename = 'output/figures/cover_over_time_HORIZONTAL_no_fitted_lines.png', 
#          dpi = 'print', height = 6, width = 10, units = 'in')

cover_plot  + 
  facet_grid( type ~ ecogroup , scales = 'free_y') + 
  scale_y_log10() + 
  theme( axis.text.x = element_text( angle = -45, hjust = 0)) + 
  ggsave('output/figures/Fig_2_cover_series_by_ecoregion.png', 
         height = 7, width = 10, units = 'in', dpi = 'print')

pred_df <- 
  cover_plot$data %>% 
  group_by(type, year, ecogroup ) %>% 
  dplyr::summarise(yhat = median(yhat))

# cover_plot  + 
#   facet_grid( type ~ ecogroup , scales = 'free_y') + 
#   geom_line(data = pred_df, aes( y = yhat )) + 
#   scale_y_log10() + 
#   theme( axis.text.x = element_text( angle = -45, hjust = 0)) + 
#   ggsave('output/figures/cover_series_detail_with_predictions.png', 
#          height = 7, width = 10, units = 'in', dpi = 'print')

agb_biomass_plot + 
  facet_grid( type ~ ecogroup, scales = 'free_y' ) + 
  theme( axis.text.x = element_text( angle = -45, hjust = 0)) + 
  ggsave('output/figures/Fig_3_biomass_series_by_ecoregion.png', 
       height = 4, width = 10, units = 'in', dpi = 'print')

dat %>% 
  ggplot( aes( x = year, y = value, color = type, fill = type )) + 
  #stat_summary(fun = function( x ) median(x), geom = 'line') + 
  stat_summary(fun.max = function(x) quantile(x, 0.75),
               fun.min = function(x) quantile(x, 0.25), 
               geom = 'ribbon', alpha = 0.4, color = NA) + 
  stat_summary(fun = function( x) median( x), 
               geom = 'line', aes( y = yhat))  + 
  facet_grid( type ~ ecogroup, scales = 'free_y')  + 
  scale_fill_manual(values = my_colors) + 
  scale_color_manual(values = my_colors) + 
  scale_y_log10()

# Now add fitted lines 
cover_plot_no_lines <- cover_plot
cover_plot_no_lines$layers[[2]] <- NULL 

# cover_plot_no_lines + 
#   stat_summary(aes( y = yhat), 
#                fun = function(x) median(x), 
#                geom = 'line', linetype = 2, size = 0.5) + 
#   facet_grid( Class ~ ecogroup, scales = 'free_y') + 
#   theme( axis.text.x = element_text(angle = -45, hjust = 0)) + 
#   ggsave(filename = 'output/figures/cover_over_time_HORIZONTAL_with_fitted_lines.png', 
#          dpi = 'print', height = 6, width = 10, units = 'in')

herbs <- cover_plot %+% 
  (dat2 %>% filter( Class == 'Herbaceous')) + 
  facet_grid( Class ~ ecogroup ) + 
  theme(strip.background.y  = element_blank(), 
        strip.text.y = element_blank()) + 
  guides( fill = F, color = F)

woody <- cover_plot %+% 
  (dat2 %>% filter( Class == 'Woody')) + 
  facet_grid( ecogroup ~ Class ) + 
  guides( fill = F, color = F) + 
  theme( axis.title.y = element_blank())


layout <- rbind( c(1, 2, 3))

full_legend <- get_legend(cover_plot)

grid.arrange(herbs, woody, as_ggplot(full_legend),  
             layout_matrix = layout , 
             widths = c(1, 1.25, 0.5)) %>%   
  ggsave(filename = 'output/figures/Herbaceous_and_Woody_veg_series.png', 
          dpi = 'print', height = 8, width = 8, units = 'in')

# Variance Plots 
# Variance partitioning -------------------------------- 
vc <- lapply(cover_models, VarCorr)
vc <- lapply(vc, data.frame )

cover_types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
cover_types <- cover_types[cover_types!='HERB']

cover_types <- factor( cover_types, 
                       labels = c('Annual', 
                                  'Bare Ground', 
                                  'Perennial',
                                  'Shrub',
                                  'Tree'))

vc <- mapply(x =cover_types, y = vc, 
             function(x, y) {y$type <- x; return(y)}, SIMPLIFY = F)


vc_agb <- lapply(agb_models, VarCorr )
vc_agb <- lapply( vc_agb, data.frame)
agb_types  <- c( str_extract(basename( agb_model_files), pattern = '[A-Z]+') )
agb_types <- factor( agb_types, labels = c('Annual Biomass','Herb Biomass', 'Perennial Biomass'))
vc_agb <- mapply( x = agb_types, y = vc_agb, function(x,y){y$type <- x; return(y)}, SIMPLIFY = F)

trend_variance <- 
  vc_agb %>% 
  bind_rows() %>% 
  filter(type != 'Herb Biomass') %>%
  bind_rows(vc ) %>% 
  filter( var1 == 'year2') %>% 
  group_by( type ) %>%
  mutate( sd = sqrt( vcov )) %>% 
  mutate(prop_var = vcov/sum(vcov)) %>% 
  ungroup() %>%
  mutate( Group = factor( grp, labels = c('Field Office', 
                                          'Allotment'))) %>%
  mutate( Group = factor( Group, levels = c('Allotment', 'Field Office'), 
                          ordered = T )) %>%
  mutate( type_labels = factor( type , 
                                labels = c( 'Annual Biomass',
                                            'Perennial Biomass', 
                                            'Annual Cover', 
                                            'Bare Cover', 
                                            'Perennial Cover', 
                                            'Shrub Cover', 
                                            'Tree Cover')))
intcpt_variance <- 
  vc_agb %>% 
  bind_rows() %>% 
  filter(type != 'Herb Biomass') %>%
  bind_rows(vc ) %>% 
  filter( is.na(var2)) %>% 
  filter( (var1 != 'year2' | grp == 'Residual') ) %>% 
  group_by( type )  %>% 
  mutate(prop_var = vcov/sum(vcov)) %>% 
  ungroup() %>% 
  mutate( Group = factor(grp, labels = c('Field Office', 'Residual', 
                                          'Allotment', 'Year'))) %>%
  mutate( Group = factor( Group, levels = c('Residual', 'Year', 'Allotment', 'Field Office'), 
                          ordered = T )) %>%
  mutate( type_labels = factor( type , 
                                labels = c( 'Annual Biomass',
                                            'Perennial Biomass', 
                                            'Annual Cover', 
                                            'Bare Cover', 
                                            'Perennial Cover', 
                                            'Shrub Cover', 
                                            'Tree Cover')))

RE_plot1 <- intcpt_variance %>% 
  group_by(type_labels) %>% 
  arrange(type_labels, desc(Group)) %>% 
  mutate( cmsm = cumsum(vcov)) %>% 
  mutate( cmsm0 = lag(cmsm) ) %>%
  mutate( cmsm0 = ifelse(is.na(cmsm0),0, cmsm0 )) %>% 
  mutate( ypos = (cmsm + cmsm0) / 2, 
          lab = paste0(round(prop_var*100, 0), '%')) %>% 
  ungroup() %>% 
  ggplot( aes( x = type_labels, y = vcov, fill = Group )) + 
  geom_bar(stat = 'identity') + 
  ggrepel::geom_text_repel( aes( y = ypos, label = lab), force =0) + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = - 30, hjust = 0), 
        axis.title.x = element_blank()) + 
  ylab( 'Intercept Error Variance') + 
  scale_fill_manual(values = group_variance_colors)  

# RE_plot1 + ggsave( 'output/figures/Fig_S2_Intercept_Error_Variance.png', 
#         width = 7, height = 4, dpi = 'print', units = 'in')

RE_plot2 <- trend_variance %>% 
  group_by(type_labels) %>% 
  arrange(type_labels, desc(Group)) %>% 
  mutate( cmsm = cumsum(vcov)) %>% 
  mutate( cmsm0 = lag(cmsm) ) %>%
  mutate( cmsm0 = ifelse(is.na(cmsm0),0, cmsm0 )) %>% 
  mutate( ypos = (cmsm + cmsm0) / 2, 
          lab = paste0(round(prop_var*100, 0), '%')) %>% 
  ungroup() %>% 
  ggplot( aes( x = type_labels, y = vcov, fill = Group )) + 
  geom_bar(stat = 'identity') + 
  ggrepel::geom_text_repel( aes( y = ypos, label = lab), force =0) + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = - 30, hjust = 0), 
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = group_variance_colors) + 
  ylab( 'Trend Error Variance')  


# RE_plot2 + 
#   ggsave( 'output/figures/Fig_9b_Trend_Error_Variance.png', 
#           width = 7, height = 4, dpi = 'print', units = 'in')

variance_plots <- intcpt_variance %>% 
  mutate( type2 = "Random Intercept") %>% 
  dplyr::select( Group, vcov, type2, type, type_labels, prop_var ) %>% 
  rbind(trend_variance %>% 
          mutate( type2 = 'Random Slope') %>% 
          dplyr::select( Group, vcov, type2, type, type_labels, prop_var)) %>% 
  group_by(type2, type_labels) %>% 
  arrange(type2, type_labels, desc(Group)) %>% 
  mutate( cmsm = cumsum(vcov)) %>% 
  mutate( cmsm0 = lag(cmsm) ) %>%
  mutate( cmsm0 = ifelse(is.na(cmsm0),0, cmsm0 )) %>% 
  mutate( ypos = (cmsm + cmsm0) / 2, 
          lab = paste0(round(prop_var*100, 0), '%')) %>% 
  ungroup() %>% 
  rename( 'Scale' = Group ) %>% 
  ggplot( aes( x = type_labels, y = vcov, fill = Scale )) + 
  geom_bar(stat = 'identity') + 
  ggrepel::geom_text_repel( aes( y = ypos, label = lab), force =0, size = 2) + 
  facet_wrap( ~ type2, ncol = 1, scales = 'free_y' ) + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = - 30, hjust = 0), 
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = group_variance_colors) + 
  ylab( 'Error Variance' ) 

  
variance_plots + 
  ggsave( 'output/figures/Fig_S2_Variance_Plots_Combined.png',
          height = 7, width = 8, dpi = 'print', units = 'in')




  