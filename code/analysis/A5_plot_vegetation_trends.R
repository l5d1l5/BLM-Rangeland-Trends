rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(gridExtra)
library(ggpubr)
require(kableExtra)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

# cover trends:  ------------------------------------------ #
cover_model_files <-
  dir(path = 'output',
      pattern = '.*_cover_trend_model.rds',
      full.names = T)

cover_models <- lapply(cover_model_files, read_rds)
types  <- c(str_extract(cover_model_files, pattern = '[A-Z]+'))

types <-
  factor(types,
         labels = c('Annual', 'Bare', 'Total', 'Perennial', 'Shrub', 'Tree'))

names(cover_models) <- types

trend_table_cover <- mapply(
  x = cover_models,
  y = types,
  FUN = function(x, y)
    ecogroup_trends_as_df(x, y),
  SIMPLIFY = F
)

trend_table_cover <- do.call(rbind, trend_table_cover)

# Drop Total Herb Cover Type
trend_table_cover <-
  trend_table_cover %>%
  filter(type != 'Total') %>%
  mutate(type = factor(type))

cover_models <-
  cover_models[-which(names(cover_models) == 'Total')]
#
cover_attribs <- lapply(cover_models,
                        function(x) {
                          attributes(x@frame$value2)
                        })

year2_attributes <-
  lapply(cover_models, function(x)
    attributes(x@frame$year2))


trend_table_cover <- trend_table_cover %>%
  mutate( ecogroup = factor(ecogroup)) %>% 
  mutate(ecogroup = factor(ecogroup, labels = ecogroup_labels)) %>%
  left_join(data.frame(
    type = unique(trend_table_cover$type),
    scale = c(
      cover_attribs[[1]]$`scaled:scale`,
      cover_attribs[[2]]$`scaled:scale`,
      cover_attribs[[3]]$`scaled:scale`,
      cover_attribs[[4]]$`scaled:scale`,
      cover_attribs[[5]]$`scaled:scale`
    ),
    year2_scale = c(
      year2_attributes[[1]]$`scaled:scale`,
      year2_attributes[[2]]$`scaled:scale`,
      year2_attributes[[3]]$`scaled:scale`,
      year2_attributes[[4]]$`scaled:scale`,
      year2_attributes[[5]]$`scaled:scale`
    )
  ),
  by = 'type') %>%
  mutate(
    year2.trend = year2.trend * scale / year2_scale,
    asymp.LCL = asymp.LCL * scale / year2_scale,
    asymp.UCL = asymp.UCL * scale / year2_scale
  )


trend_table_cover$unit <- 'Cover'

# Biomass Models and Trends--------------------------- #
agb_model_files <-
  dir(path = 'output',
      pattern = '.*_agb_trend_model.rds',
      full.names = T)

agb_models <- lapply(agb_model_files, read_rds)

types  <-
  c(str_extract(basename(agb_model_files), pattern = '[A-Z]+'))

types <- factor(types, labels = c('Annual', 'Total', 'Perennial'))

trend_table <-
  mapply(
    x = agb_models,
    y = types,
    FUN = function(x, y)
      ecogroup_trends_as_df(x, y),
    SIMPLIFY = F
  )

trend_table <- do.call(rbind, trend_table)

# Transform rate to get back to raw un-scaled proportion increase
# Multiply rates by the scale (sd) of log response
response_attributes <-
  lapply(agb_models, function(x)
    attributes(x@frame$value2))

year2_attributes <-
  lapply(agb_models, function(x)
    attributes(x@frame$year2))


trend_table_agb <- trend_table %>%
  left_join(data.frame(
    type = c('Annual', 'Total', 'Perennial'),
    scale = c(
      response_attributes[[1]]$`scaled:scale`,
      response_attributes[[2]]$`scaled:scale`,
      response_attributes[[3]]$`scaled:scale`
    ),
    year2_scale = c(
      year2_attributes[[1]]$`scaled:scale`,
      year2_attributes[[2]]$`scaled:scale`,
      year2_attributes[[3]]$`scaled:scale`
    )
  ),
  by = 'type') %>%
  mutate(
    year2.trend = year2.trend * scale / year2_scale,
    asymp.LCL = asymp.LCL * scale / year2_scale ,
    asymp.UCL = asymp.UCL * scale / year2_scale
  )   %>%
  mutate( ecogroup = factor(ecogroup)) %>%
  mutate(ecogroup = factor(ecogroup, labels = ecogroup_labels))


trend_table_agb$unit <- 'Biomass'

# Plot Cover and Biomass Trends together
trend_table <-
  trend_table_cover %>%
  bind_rows(trend_table_agb) %>%
  filter(type != 'Total') %>%
  mutate(unit = factor(
    unit,
    levels = c('Cover', 'Biomass'),
    ordered = T
  )) %>%
  mutate(type = factor(
    type,
    levels = sort(names(my_colors), decreasing = T),
    ordered = T
  )) %>%
  mutate(sig = ifelse((asymp.LCL > 0 | asymp.UCL < 0), "*", ''))

# Plot -------------------------
trend_table %>%
  plot_trend_coefficients_vertical(my_colors = my_colors) +
  facet_grid(ecogroup  ~ unit, switch = 'y', scales = 'free_x') +
  theme(legend.title = element_text()) +
  scale_color_manual(name = 'Functional Type', values = my_colors) +
  scale_x_continuous(name = 'Trend Slope (+/- 95%CI)') +
  scale_color_manual(name = 'Functional Type',
                     values = my_colors,
                     guide = guide_legend(reverse = T)) +
  ggsave(
    filename = 'output/figures/Fig_4_veg_trends_by_Ecoregion.png',
    height = 8,
    width = 8,
    units = 'in',
    dpi = 'print'
  )


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
  split(f = .$type:.$unit)

ttlist <- ttlist[which(unlist(lapply(ttlist, nrow)) != 0)]

for (i in names(ttlist)) {
  ttlist[[i]] %>% kableExtra::kbl(digits = 3, caption = i) %>% kableExtra::kable_classic_2() %>%
    kableExtra::save_kable(file = file.path('output/tables/',
                                            paste0(i, '_trend_estimates.html')))
}

# Plot observed and predicted over time
## Plot Allotment Cover over time :
dat <- mapply(
  x = cover_models,
  y = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'),
  FUN = function(x, y)
    back_trans_frames(x, y),
  SIMPLIFY = F
)

dat <- mapply(
  x = dat,
  y = cover_models,
  FUN = function(x, y) {
    x$yhat <-
      back_transform(predict(y, 
                             newdata = x, 
                             re.form = ~ (1 | ecogroup:office_label) ),
                     attributes(y@frame$value2))
    return(x)
  },
  SIMPLIFY = F
)

dat <- do.call(rbind, dat)

dat2 <- dat %>%
  mutate(Class = ifelse(type %in% c('Tree', 'Shrub'), 'Woody', 'Herbaceous')) %>%
  mutate(ecogroup = factor(ecogroup, labels = c(ecogroup_labels)))

cover_plot <- dat2 %>%
  ggplot(aes(
    x = year,
    y = value,
    color = type,
    fill = type ,
    group = type
  )) +
  stat_summary(
    fun.max = function(x)
      quantile(x, 0.75),
    fun.min = function(x)
      quantile(x, 0.25),
    geom = 'ribbon',
    alpha = 0.4,
    color = NA
  ) +
  stat_summary(
    fun = function(x)
      median(x),
    geom = 'line'
  ) +
  facet_grid(ecogroup ~ Class) +
  scale_color_manual(name = 'Vegetation Type', values = my_colors) +
  scale_fill_manual(name = 'Vegetation Type', values = my_colors) +
  theme_bw() +
  scale_x_continuous(breaks = c(1995, 2005, 2015)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(strip.text.y = element_text(angle = 0)) +
  ylab('Cover (%)')   +
  xlab('Year')


dat <- mapply(
  x = cover_models,
  y = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'),
  FUN = function(x, y)
    back_trans_frames(x, y),
  SIMPLIFY = F
)

#y = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'),

get_plotting_data <-
  function(m, type_label, quantile_ribbon = T, ...) {
    temp <- m@frame
    year2 <- temp$year2
    #ecogroup <- temp$ecogroup
    #state <- temp$admin_st
    #office <- temp$office_label
    #uname <- temp$uname
    att1 <- attributes(temp$value2)
    att2 <- attributes(temp$year2)
    temp$type <- type_label
    temp$year <- back_transform(temp$year2, att2, log = F)
    
    # find back-transformed ecoregion level averages
    temp_pred <- temp %>%
      group_by(year2, year, ecogroup, office_label, uname, type) %>%
      summarise(n = n()) %>%
      data.frame() %>%
      mutate(yhat2 = predict(newdata = .,  m, 
                             re.form = ~ (1 | ecogroup:office_label) + (1 | uname)) ) %>%
      group_by(year, ecogroup , type) %>%
      summarise( yhat2 = weighted.mean(yhat2, n)) %>% 
      #summarise( yhat2 = sum(yhat2 * n) / (sum(n))) %>% 
      mutate(yhat = back_transform(yhat2, att1)) # avg weighted by no. allots
    
    # Get quantiles for empirical data for plotting
    if (quantile_ribbon) {
      temp_obs <-
        temp %>%
        group_by(year, ecogroup, type) %>%
        #sample_n( size , replace = T ) %>%
        summarise(
          m = median(value2),
          ul = quantile(value2, 0.75),
          ll = quantile(value2, 0.25)
        ) %>%
        pivot_longer(
          cols = c(m, ul, ll),
          names_to = 'stat',
          values_to = 'value2'
        ) %>%
        mutate(value = back_transform(value2 , att1))
      
    } else{
      temp_obs <-
        temp %>%
        group_by(year, ecogroup, type) %>%
        sample_n(... , replace = T) %>%
        #summarise( m = median( value2 ), ul = quantile( value2, 0.75), ll = quantile( value2, 0.25)) %>%
        #pivot_longer( cols = c(m, ul, ll), names_to = 'stat', values_to = 'value2') %>%
        mutate(value = back_transform(value2 , att1))
    }
    return(list(predictions = temp_pred, observations = temp_obs))
  }


temp <- mapply(
  x = cover_models,
  y = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'),
  FUN = function(x, y, ...)
    get_plotting_data(x, y, ...),
  SIMPLIFY = F
)

predictions <-
  do.call(rbind,  lapply(temp, function(x)
    x$predictions))

observations <-
  do.call(rbind, lapply(temp, function(x)
    x$observations))

old_labels <- levels(trend_table$ecogroup)
new_labels <- levels(predictions$ecogroup)

predictions$ecogroup <-
  factor(predictions$ecogroup , labels = old_labels)

preds <- predictions %>%
  left_join(trend_table %>%
              filter(unit == 'Cover', sig == "*"),
            by = c('ecogroup', 'type')) %>%
  filter(!is.na(sig))

observations$ecogroup <-
  factor(observations$ecogroup, labels = ecogroup_labels)

levels(observations$ecogroup)
levels(preds$ecogroup)
ylab = "Cover (%)"

observations %>%
  select(-value2) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  ggplot(aes(x = year, y = m, color = type)) +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = type),
              alpha = 0.5,
              color = NA) +
  geom_line(data = preds,
            aes(x = year, y = yhat),
            color = 'black',
            alpha = 0.5) +
  facet_grid(type ~ ecogroup, scales = 'free_y') +
  scale_fill_manual(name = 'Cover Type', values = my_colors) +
  scale_x_continuous(breaks = c(1995, 2005, 2015)) +
  theme_bw() +
  scale_y_log10(name = ylab) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = -45, hjust = 0)) +
  xlab('Year') +
  theme(legend.position = "none") +
  ggsave(
    filename = 'output/figures/Fig_2_cover_series_by_ecoregion.png',
    height = 7,
    width = 10,
    units = 'in',
    dpi = 'print'
  )

# Do cover series alternate version "dots" ------------------------------ #
temp <- mapply(
  x = cover_models,
  y = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'),
  FUN = function(x, y, ...)
    get_plotting_data(x, y, quantile_ribbon = F, ...),
  size = 10,
  SIMPLIFY = F
)

predictions <-
  do.call(rbind,  lapply(temp, function(x)
    x$predictions))

observations <-
  do.call(rbind, lapply(temp, function(x)
    x$observations))

old_labels <- levels(trend_table$ecogroup)
new_labels <- levels(predictions$ecogroup)

predictions$ecogroup <-
  factor(predictions$ecogroup , labels = old_labels)

preds <- predictions %>%
  left_join(trend_table %>%
              filter(unit == 'Cover', sig == "*"),
            by = c('ecogroup', 'type')) %>%
  filter(!is.na(sig))

observations$ecogroup <-
  factor(observations$ecogroup, labels = ecogroup_labels)

observations %>%
  ggplot(aes(x = year, y = value, color = type)) +
  geom_point(alpha = 0.5) +
  geom_line(data = preds, aes(x = year, y = yhat), color = 'black') +
  facet_grid(type ~ ecogroup, scales = 'free_y') +
  scale_color_manual(name = 'Type', values = my_colors) +
  scale_x_continuous(breaks = c(1995, 2005, 2015)) +
  theme_bw() +
  scale_y_log10(name = ylab) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = -45, hjust = 0)) +
  xlab('Year') +
  theme(legend.position = "none") +
  ggsave(
    filename = 'output/figures/Fig_2_cover_series_by_ecoregion_dots.png',
    height = 7,
    width = 10,
    units = 'in',
    dpi = 'print'
  )

# Plot AGB over time ----------------------------------------------------- #
temp <- mapply(
  x = agb_models,
  y = c('Annual', 'Herb', 'Perennial'),
  FUN = function(x, y, ...)
    get_plotting_data(x, y, quantile_ribbon = T, ...),
  SIMPLIFY = F
)

predictions <-
  do.call(rbind,  lapply(temp, function(x)
    x$predictions))

observations <-
  do.call(rbind, lapply(temp, function(x)
    x$observations))

predictions <- predictions %>% filter(type != 'Herb')
observations <- observations %>% filter(type != 'Herb')
predictions$ecogroup <-
  factor(predictions$ecogroup , labels = old_labels)

preds <- predictions %>%
  left_join(trend_table %>%
              filter(unit == 'Biomass', sig == "*"),
            by = c('ecogroup', 'type')) %>%
  filter(!is.na(sig))

observations$ecogroup <-
  factor(observations$ecogroup, labels = ecogroup_labels)

ylab <- expression(Aboveground ~ Production ~ "(" * kg ~ ha ^ -1 * ")")

observations %>%
  select(-value2) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  ggplot(aes(x = year, y = m, color = type)) +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = type),
              alpha = 0.5,
              color = NA) +
  geom_line(data = preds,
            aes(x = year, y = yhat),
            color = 'black',
            alpha = 0.5) +
  facet_grid(type ~ ecogroup, scales = 'free_y') +
  scale_fill_manual(name = 'Cover Type', values = my_colors) +
  scale_x_continuous(breaks = c(1995, 2005, 2015)) +
  theme_bw() +
  scale_y_log10(name = ylab) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = -45, hjust = 0)) +
  xlab('Year') +
  theme(legend.position = "none")  +
  ggsave(
    filename = 'output/figures/Fig_3_production_series_by_ecoregion.png',
    height = 7,
    width = 10,
    units = 'in',
    dpi = 'print'
  )

# Alternate production series using "dots" --------------------------------- #
temp <- mapply(
  x = agb_models,
  y = c('Annual', 'Herb', 'Perennial'),
  FUN = function(x, y, ...)
    get_plotting_data(x, y, quantile_ribbon = F, ...),
  size = 10,
  SIMPLIFY = F
)

predictions <-
  do.call(rbind,  lapply(temp, function(x)
    x$predictions))

observations <-
  do.call(rbind, lapply(temp, function(x)
    x$observations))

predictions <- predictions %>% filter(type != 'Herb')
observations <- observations %>% filter(type != 'Herb')
predictions$ecogroup <-
  factor(predictions$ecogroup , labels = old_labels)

preds <- predictions %>%
  left_join(trend_table %>%
              filter(unit == 'Biomass', sig == "*"),
            by = c('ecogroup', 'type')) %>%
  filter(!is.na(sig))

observations$ecogroup <-
  factor(observations$ecogroup, labels = ecogroup_labels)

ylab <- expression(Aboveground ~ Production ~ "(" * kg ~ ha ^ -1 * ")")

observations %>%
  ggplot(aes(x = year, y = value, color = type)) +
  geom_point(alpha = 0.7) +
  geom_line(data = preds, aes(x = year, y = yhat), color = 'black') +
  facet_grid(type ~ ecogroup, scales = 'free_y') +
  scale_color_manual(name = 'Type', values = my_colors) +
  scale_x_continuous(breaks = c(1995, 2005, 2015)) +
  theme_bw() +
  scale_y_log10(name = ylab) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = -45, hjust = 0)) +
  xlab('Year') +
  theme(legend.position = "none") +
  ggsave(
    filename = 'output/figures/Fig_3_production_series_by_ecoregion_dots.png',
    height = 7,
    width = 10,
    units = 'in',
    dpi = 'print'
  )


cover_plot  +
  facet_grid(type ~ ecogroup , scales = 'free_y') +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

herbs <- cover_plot %+%
  (dat2 %>% filter(Class == 'Herbaceous')) +
  facet_grid(Class ~ ecogroup) +
  theme(strip.background.y  = element_blank(),
        strip.text.y = element_blank()) +
  guides(fill = F, color = F)

woody <- cover_plot %+%
  (dat2 %>% filter(Class == 'Woody')) +
  facet_grid(ecogroup ~ Class) +
  guides(fill = F, color = F) +
  theme(axis.title.y = element_blank())


layout <- rbind(c(1, 2, 3))

full_legend <- get_legend(cover_plot)

grid.arrange(
  herbs,
  woody,
  as_ggplot(full_legend),
  layout_matrix = layout ,
  widths = c(1, 1.25, 0.5)
) %>%
  ggsave(
    filename = 'output/figures/Herbaceous_and_Woody_veg_series.png',
    dpi = 'print',
    height = 8,
    width = 8,
    units = 'in'
  )
