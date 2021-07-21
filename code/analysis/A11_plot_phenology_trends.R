rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(lmerTest)

source('code/analysis/plot_tools.R')
load( 'output/phenology_models.rda')

# GSL_mer@frame %>% 
#   select( ecogroup, office_label, climate_region, uname ) %>%
#   group_by( ecogroup  ) %>% 
#   summarise( n_distinct(uname))

pheno_df <- GSL_mer@frame 
pheno_ranef <- get_blm_random_effects(GSL_mer)
pheno_ecogroup <- get_ecogroup_trends(GSL_mer)

pheno_ranef[ 
  unlist( lapply( pheno_ranef, function(x) class(x) == 'data.frame' ) ) ] 

which( length( pheno_ranef ) > 0 )

trend_summary <- blm_trend_summary(pheno_df, pheno_ecogroup, pheno_ranef)

trend_table_GSL <-ecogroup_trends_as_df(trend_model = GSL_mer, type = 'Growing Season')

any(is.na(trend_summary$allot_trend)) 
stopifnot( all(complete.cases(trend_summary)) )

y_sd <- as.numeric( attributes( pheno_df$GSL2)$`scaled:scale` )
year_sd <- as.numeric(attributes(pheno_df$year2)$`scaled:scale`)

trend_table_per_year <- trend_table_GSL %>%
  mutate_at( .vars = c( 'year2.trend', 'SE', 'asymp.LCL', 'asymp.UCL'), 
             .funs =function(x) (x/year_sd)*y_sd)

trend_table_per_year <- trend_table_per_year %>% 
  mutate( ecogroup = factor(ecogroup, 
                            labels = str_squish(ecogroup_labels))) 

pheno_trends_vert <- 
  plot_trend_coefficients_vertical( trend_table_per_year, 
                                    my_colors = 'black') + 
  facet_wrap( ~ ecogroup, ncol = 1, strip.position = 'right') + 
  scale_x_continuous('Trend (days per year)') + 
  guides(color = F) + 
  theme( strip.text.y = element_text( angle = 0 )) 


# Predict Growing Season Length over time per year --------------- # 
dat <- GSL_mer@frame
dat$yhat <- predict( GSL_mer )
dat$year <- back_transform(dat$year2, 
                           attributes_obj = attributes(dat$year2), 
                           log = F)

dat$GSL <- back_transform(dat$GSL2, 
                          attributes_obj = attributes(dat$GSL2), 
                          log = F)

dat$yhat_bt <- back_transform( dat$yhat, 
                               attributes_obj = attributes(dat$GSL2), 
                               log = F)

dat_preds <- dat %>% 
  dplyr::select( ecogroup, year2, year , climate_region, office_label, uname ) %>%
  group_by( ecogroup, year2 ) %>% 
  distinct() %>%
  mutate( year0 = back_transform( year2, c(attributes(year2), `scaled:scale` = 1), log = F))

dat_preds$yhat <- predict( GSL_mer, newdata = dat_preds)
dat_preds$yhat_bt <- back_transform( dat_preds$yhat, attributes( dat$GSL2), log = F) 

dat_preds <- dat_preds %>%
  group_by(ecogroup, year) %>% 
  dplyr::summarise( yhat_bt = median(yhat_bt ))

dat_preds %>% 
  group_by(ecogroup) %>% 
  dplyr::summarise( ymin = min( yhat_bt), ymax = max(yhat_bt), yrmin = min(year[,1]), yrmax = max(year[,1])) %>%
  mutate( rate = (ymax - ymin)/(yrmax - yrmin) ) %>%
  mutate( rate_per_year = rate )

pheno_ts_vert <- dat %>%
  mutate( ecogroup = factor(ecogroup, labels = ecogroup_labels)) %>%
  ggplot( aes( x = year, y = GSL )) + 
  #geom_line( data=dat_preds, aes( y = yhat_bt), linetype = 2, alpha = 0.75) + 
  stat_summary( geom = 'ribbon', fun.max = function(x) quantile(x,0.75), 
                fun.min = function(x) quantile(x,0.25), alpha = 0.5)  +
  stat_summary( geom = 'line', fun = 'median', alpha = 0.7 ) + 
  facet_wrap(~ecogroup, ncol = 1, strip.position = 'right', scales = 'free_y') + 
  theme_bw()  + 
  theme( strip.text.y = element_blank()) + 
  scale_y_continuous("Growing Season Length (days)") + 
  scale_x_continuous("Year", breaks = c(1995, 2005, 2015)) 


# use this for manuscript 
gridExtra::grid.arrange(pheno_ts_vert, 
                        pheno_trends_vert, ncol = 2) %>%  
  ggsave( file = 'output/figures/phenology_ts_trends_combined_vertical.png', 
          width = 8, 
          height = 9)

# Save Trend Table 
trend_table_GSL_per_year <- trend_table_per_year 

# Horizontal orientation plots 

# dat %>%
#   ggplot( aes( x = year, y = GSL )) + 
#   #geom_line( data=dat_preds, aes( y = yhat_bt), linetype = 2, alpha = 0.75) + 
#   stat_summary( geom = 'ribbon', fun.max = function(x) quantile(x,0.75), 
#                 fun.min = function(x) quantile(x,0.25), alpha = 0.5)  +
#   stat_summary( geom = 'line', fun = 'median', alpha = 0.7) + 
#   facet_wrap(~ecogroup, ncol = 2, scales = 'free_y') + 
#   theme_bw()  + 
#   scale_y_continuous("Growing Season Length* (days)") + 
#   scale_x_continuous("Year") + 
#   ggsave( filename = 'output/figures/Fig_6b_average_growing_season_length_per_year.png',
#           dpi = 'print', height = 6, width = 4)

# dat %>%
#   ggplot( aes( x = year, y = GSL )) + 
#   #geom_line( data=dat_preds, aes( y = yhat_bt), linetype = 2, alpha = 0.75) + 
#   stat_summary( geom = 'ribbon', fun.max = function(x) quantile(x,0.75), 
#                 fun.min = function(x) quantile(x,0.25), alpha = 0.5)  +
#   stat_summary( geom = 'line', fun = 'median', alpha = 0.7 ) + 
#   facet_wrap(~ecogroup, nrow = 2, scales = 'free_y') + 
#   theme_bw()  + 
#   scale_y_continuous("Growing Season Length (no. days)") + 
#   scale_x_continuous("Year", breaks = c(1995, 2005, 2015)) + 
#   ggsave( filename = 'output/figures/growing_season_duration_HORIZONTAL.png',
#         dpi = 'print', height = 5, width = 9)


# 
# pheno_trends <- trend_table_per_year %>% 
#   mutate( ecogroup = factor( ecogroup, labels = c('AZ/NM Highlands', 'E Cold Desert', 
#                                          'Great Plains' ,'Mediterranean\nCalifornia', 
#                                          'Forested Mts', 'W Cold Deserts', 
#                                          'Warm Deserts'))) %>% 
#   plot_ecogroup_trend_coefficients(my_colors = 'black') + 
#   guides(color = F) + 
#   facet_wrap( ~ ecogroup, nrow = 1, scale = 'free_x') + 
#   theme( axis.text.x = element_blank()) + 
#   scale_y_continuous("Trend (days per year)") 
# 
# gridExtra::grid.arrange(pheno_ts + facet_wrap( ~ ecogroup , nrow = 1) + 
#                           theme(axis.text.y = element_text( margin = unit(c(1,0.7,1,1), units = 'lines'))), 
#                         pheno_trends + 
#                           theme(strip.text = element_blank(), 
#                                 axis.text.y = element_text(
#                                   margin = unit(c(1,0.5,1,1), 
#                                                 units = 'lines'))) , heights = c(1.6,1)) %>% 
#   ggsave( filename = 'output/figures/phenology_ts_trends_combined.png', 
#           width = 14, 
#           height = 6)


#  Separate phenophase models  ---------------------------------------- # 

# SOS "Start of Season" 
pheno_df <- SOS_mer@frame 
pheno_ranef <- get_blm_random_effects(SOS_mer)
pheno_ecogroup <- get_ecogroup_trends(SOS_mer)
trend_summary <- blm_trend_summary(pheno_df, pheno_ecogroup, pheno_ranef)
trend_table_SOS <-ecogroup_trends_as_df(trend_model = SOS_mer, 
                                         type = 'Early Growing Season')

y_sd_SOS <- as.numeric( attributes( pheno_df$SOS2)$`scaled:scale` )
year_scale_SOS <- as.numeric( attributes(pheno_df$year2)$`scaled:scale`)

#  EOS
pheno_df <- EOS_mer@frame 
pheno_ranef <- get_blm_random_effects(EOS_mer)
pheno_ecogroup <- get_ecogroup_trends(EOS_mer)
trend_summary <- blm_trend_summary(pheno_df, pheno_ecogroup, pheno_ranef)
trend_table_EOS <-ecogroup_trends_as_df(trend_model = EOS_mer, 
                                         type = 'Late Growing Season')

y_sd_EOS <- as.numeric( attributes( pheno_df$EOS2)$`scaled:scale` )
year_scale_EOS <- as.numeric( attributes(pheno_df$year2)$`scaled:scale`)

# MS 
pheno_df <- MS_mer@frame 
pheno_ranef <- get_blm_random_effects(MS_mer)
pheno_ecogroup <- get_ecogroup_trends(MS_mer)
trend_summary <- blm_trend_summary(pheno_df, pheno_ecogroup, pheno_ranef)
trend_table_MS <-ecogroup_trends_as_df(trend_model = MS_mer, 
                                         type = 'Mid Growing Season')

y_sd_MS <- as.numeric( attributes( pheno_df$MS2)$`scaled:scale` )
year_scale_MS <- as.numeric( attributes(pheno_df$year2)$`scaled:scale`)

growing_season_labels <- c(Early~'('*SOS*')', 
                           Mid~'('*MS*')', 
                           Late~'('*EOS*')')


trend_table <- trend_table_EOS %>% 
  rbind(trend_table_SOS) %>% 
  rbind(trend_table_MS) %>% 
  mutate( type = factor(type, levels = c('Early Growing Season', 
                                         'Mid Growing Season', 
                                         'Late Growing Season'), ordered = T))

trend_table %>% 
  plot_ecogroup_trend_coefficients(my_colors = c('forestgreen', 'darkgray', 'blue')) + 
  scale_color_manual(name = 'Phenological Period', values = c('forestgreen', 'darkgray', 'blue') ) + 
  scale_y_continuous(name = 'Phenology Trend') +   
  ggsave(filename = 'output/figures/Fig_7a_phenology_trends_by_Ecoregion_HORIZONTAL.png', 
         height = 5, width = 8, units = 'in', dpi = 'print') 


trend_scales <- c(y_sd_SOS, y_sd_MS, y_sd_EOS) 
year_scales <- c(year_scale_SOS, year_scale_MS, year_scale_EOS)

names( trend_scales ) <- c('Early Growing Season', 
                                         'Mid Growing Season', 
                                         'Late Growing Season')

names(year_scales) <- names(trend_scales)

trend_table$y_sd <- trend_scales[ trend_table$type  ] 
trend_table$year_sd <- year_scales[trend_table$type]

trend_table_per_year <- trend_table %>%
  mutate_at( .vars = c( 'year2.trend', 'SE', 'asymp.LCL', 'asymp.UCL'), 
             .funs =function(x) (x/year_sd)*y_sd) 

trend_table_per_year %>% 
  plot_ecogroup_trend_coefficients(my_colors = c('forestgreen', 'darkgray', 'blue')) + 
  scale_color_manual(name = 'Phenological Period', 
                       values = c('forestgreen', 'darkgray', 'blue')) + 
  scale_y_continuous(name = "Trend (days per year)") +   
  theme(axis.text.x = element_text(angle = -30, hjust = 0)) + 
  scale_color_manual(values = c('forestgreen', 'darkgray', 'blue'), 
                     labels = growing_season_labels)  + 
  ggsave(filename = 'output/figures/rescaled_phenology_trends_by_Ecoregion_HORIZONTAL.png', 
       height = 5, width = 9, units = 'in', dpi = 'print') 


# Phenology over time 
dat <- SOS_mer@frame
dat$year <- back_transform(dat$year2, 
                           attributes_obj = attributes(dat$year2), 
                           log = F)

dat$`SOS` <- back_transform(dat$SOS2, 
                          attributes_obj = attributes(dat$SOS2), 
                          log = F)

dat$`EOS` <- back_transform(EOS_mer@frame$EOS2, 
                                attributes_obj = attributes(EOS_mer@frame$EOS2), 
                                log = F)

dat$`MS` <- back_transform(MS_mer@frame$MS2, 
                                attributes_obj = attributes(MS_mer@frame$MS2), 
                                log = F)

dat_preds <- dat %>% 
  dplyr::select( ecogroup, year2 , year ) %>%
  group_by( ecogroup, year2, year ) %>% 
  distinct() %>%
  mutate( year0 = back_transform( year2, 
                                 attributes(year2), 
                                 log = F))


dat_preds$yhat_late <- predict( EOS_mer, newdata = dat_preds, re.form = NA)
dat_preds$yhat_late_bt <- back_transform( dat_preds$yhat_late, 
                                          attributes(EOS_mer@frame$EOS2), 
                                          log = F) 

dat_preds$yhat_early <- predict( SOS_mer, newdata = dat_preds, re.form = NA)
dat_preds$yhat_early_bt <- back_transform( dat_preds$yhat_early, 
                                          attributes(SOS_mer@frame$SOS2), 
                                          log = F) 

dat_preds$yhat_mid <- predict( MS_mer, newdata = dat_preds, re.form = NA)
dat_preds$yhat_mid_bt <- back_transform( dat_preds$yhat_mid, 
                                           attributes(MS_mer@frame$MS2), 
                                         log = F) 
dat <- dat %>% 
  dplyr::select( year, ecogroup, office_label, SOS, MS, EOS) %>%
  rename( 'Early Growing Season' = SOS,
          'Mid Growing Season' = MS,  
          'Late Growing Season' = EOS) %>%
  pivot_longer( cols = c( `Early Growing Season`:`Late Growing Season`) ,names_to = "type", values_to = "phenology") %>% 
  mutate( type = factor( type, levels = c('Early Growing Season', 
                                          'Mid Growing Season', 
                                          'Late Growing Season'), ordered = T))
dat_preds <- 
  dat_preds %>% 
  ungroup() %>%
  dplyr::select( ecogroup, year, 
                 yhat_early_bt, yhat_mid_bt, yhat_late_bt) %>% 
  rename( 'Early Growing Season' = yhat_early_bt, 
          'Mid Growing Season' = yhat_mid_bt, 
          'Late Growing Season' = yhat_late_bt) %>% 
  pivot_longer( cols = c(`Early Growing Season`:`Late Growing Season`), names_to = 'type', values_to = 'yhat') %>% 
  mutate( type = factor( type, levels = c('Early Growing Season', 
                                          'Mid Growing Season', 
                                          'Late Growing Season'), ordered = T))

dat_preds <- dat_preds %>%
  group_by(ecogroup, year, type ) %>% 
  summarise( yhat = median(yhat ))


pheno_ts <- dat %>%
  mutate( ecogroup = factor(ecogroup, labels = ecogroup_labels)) %>%
  ggplot( aes( x = year, y = phenology, fill = type )) + 
  stat_summary( geom = 'ribbon', fun.max = function(x) quantile(x,0.75), 
                fun.min = function(x) quantile(x,0.25), alpha = 0.5)  +
  stat_summary( geom = 'line', fun = 'median', alpha = 0.7, aes( color = type)) + 
  facet_wrap(~ecogroup, ncol = 1, strip.position = 'right',  scales = 'free_y') + 
  theme_bw()  + 
  theme( strip.text.y = element_blank()) + 
  scale_y_continuous("Day of Year") + 
  scale_x_continuous("Year", breaks = c(1995, 2005, 2015))  + 
  scale_fill_manual(name = 'Phenological Period', values = c('darkgreen', 'darkgray', 'blue')) + 
  scale_color_manual(name = 'Phenological Period', values = c('darkgreen', 'darkgray', 'blue'))

pheno_trends_vert <- 
  trend_table_per_year %>% 
  mutate( ecogroup = factor( ecogroup, labels = ecogroup_labels)) %>% 
  plot_trend_coefficients_vertical(
    my_colors = c('forestgreen', 'darkgray', 'blue')) + 
  facet_wrap( ~ ecogroup, 
              ncol = 1, 
              strip.position = 'right', ) + 
  scale_color_manual(name = 'Phenological Period', 
                     values = c('forestgreen', 'darkgray', 'blue'), 
                     labels = growing_season_labels) + 
  scale_x_continuous(name = "Trend (days per year)", 
                     breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)) +  
  theme( strip.text.y = element_text( angle = 0))


plot <- gridExtra::grid.arrange(pheno_ts + 
                          guides( fill = 'none', color = 'none'), 
                        pheno_trends_vert, ncol = 2, widths = c(1, 1.5))  


plot %>% 
  ggsave(filename = 'output/figures/phenology_combined_VERTICAL.png',
                dpi = 'print', height = 8, width = 8)

summary(GSL_mer)
summary(SOS_mer)
summary(EOS_mer)
summary(MS_mer)


all_trends <- trend_table_per_year %>% 
  bind_rows(trend_table_GSL_per_year) %>% 
  mutate( sig = ifelse( (asymp.LCL > 0 | asymp.UCL < 0), "*", '' )) 


ttlist <- all_trends %>%
  split(f = .$type) 

ttlist <- ttlist[ which( unlist( lapply(ttlist, nrow ) ) != 0  ) ] 
library(kableExtra)
for( i in names(ttlist)){ 
  
  ttlist[[i]] %>% kbl(digits = 3, caption = i ) %>% 
    kable_classic_2() %>% 
    kableExtra::save_kable(file = file.path('output/tables/', 
                                paste0( i, '_pheno_trend_estimates.html')))
} 

