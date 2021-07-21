rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)
library(GGally)
source('code/analysis/plot_tools.R')


model_fls <- dir('output', pattern = 'trend_model.rds' ,full.names =T )
model_names <- str_extract(basename(model_fls), pattern = '[A-Za-z]+_[a-z]+')

model_list <- data.frame( model_names = model_names , file = model_fls) %>% 
  separate( model_names , c('type', 'unit')) %>%
  mutate( type = str_to_upper(type )) %>% 
  filter( type != 'HERB' , unit != 'agb')

format_ranefs <- function( x , type = 'AFG'){ 
  
  x$uname <- 
    x$uname %>% 
    mutate( Group = rownames(.) ) %>% 
    mutate( scale = 'Allotment' , type = type )
  
  x$`year:climate_region` <- 
    x$`year:climate_region` %>% 
    mutate( Group = row.names(.)) %>%
    mutate( scale = 'Year' , type = type )
  
  x$office_label <- 
    x$office_label %>% 
    mutate( Group = rownames(.) ) %>% 
    mutate( scale = 'Office' , type = type )
  
  return(x)
} 

out <- list()
for( i in 1:nrow( model_list )){ 
  
  temp <- ranef( read_rds(file = model_list$file[i])) 
  temp <- lapply( temp, as.data.frame ) 
  temp <- format_ranefs(temp, model_list$type[i])
  fixef_temp <- get_ecogroup_trends(read_rds(file = model_list$file[[i]]))
  fixef_temp <- data.frame( Group = names( fixef_temp), 
              scale = 'Ecogroup', 
              type = model_list$type[i], year2 = fixef_temp  )
  
  temp <- do.call(bind_rows, temp )    
  
  temp <- 
    temp %>% 
    bind_rows(fixef_temp)
  
  out[[i ]] <- temp 
  
} 
out_all <- do.call(bind_rows, out ) 

office_year2 <- out_all %>% 
  pivot_longer(cols=c(`(Intercept)`, year2), names_to = 'param', values_to = 'value' ) %>% 
  filter( scale == 'Office', param == 'year2') %>% 
  pivot_wider(names_from = type, values_from = value )

allotment_year2 <- out_all %>% 
  pivot_longer(cols=c(`(Intercept)`, year2), names_to = 'param', values_to = 'value' ) %>% 
  filter( scale == 'Allotment', param == 'year2') %>% 
  pivot_wider(names_from = type, values_from = value )

ecogroup_year2 <- out_all %>% 
  pivot_longer(cols=c(`(Intercept)`, year2), names_to = 'param', values_to = 'value' ) %>% 
  filter( scale == 'Ecogroup', param == 'year2') %>% 
  pivot_wider(names_from = type, values_from = value )

year_intercept <- out_all %>% 
  pivot_longer(cols=c(`(Intercept)`, year2), names_to = 'param', values_to = 'value' ) %>% 
  filter( scale == 'Year', param == '(Intercept)') %>% 
  pivot_wider(names_from = type, values_from = value )

# Correlate Residuals across models 
combos <- expand.grid( c('AFG', 'BG', 'PFG', 'SHR', 'TREE'), c('AFG', 'BG', 'PFG', 'SHR', 'TREE'))
ecogroup_cor <- year_cor <- allotment_cor <- office_cor <- list(NA) 
office_cor$scale <- "Office"
allotment_cor$scale <- "Allotment"
year_cor$scale <- "Year"
ecogroup_cor$scale <- "Ecogroup"

office_cor <- office_year2 %>% 
  dplyr::select( AFG:TREE ) %>% 
  cor(. , use = 'pairwise') %>% 
  round( .  , 2)

allotment_cor <- allotment_year2 %>% 
  dplyr::select( AFG:TREE ) %>% 
  cor(. , use = 'pairwise') %>% 
  round( .  , 2)

year_cor <- year_intercept %>% 
  dplyr::select( AFG:TREE ) %>%
  cor( . , use = 'pairwise') %>%
  round( . , 2 )

ecogroup_cor <- ecogroup_year2 %>% 
  dplyr::select( AFG:TREE) %>% 
  cor( . , use = 'pairwise') %>% 
  round( . , 2 )

write_lines(knitr::kable(year_cor),file = 'output/cover_year_cor.txt')
write_lines(knitr::kable(ecogroup_cor),file = 'output/cover_ecogroup_trend_cor.txt')
write_lines(knitr::kable(office_cor),file = 'output/cover_office_trend_cor.txt')
write_lines(knitr::kable(allotment_cor),file = 'output/cover_allotment_trend_cor.txt')

test <- year_intercept %>% 
  separate( Group, into = c('year', 'region'), sep = ':') %>% 
  group_by( region ) %>% 
  do( cormat = list( cor( .[, c('AFG','BG', 'PFG', 'SHR', 'TREE')])))

test[ test$region == 'NR', ]$cormat 
test[ test$region == 'NW', ]$cormat 
test[ test$region == 'SW', ]$cormat 
test[ test$region == 'W ', ]$cormat 

bind_rows(
  ecogroup_cor %>% data.frame( scale = 'Ecogroup' ), 
  office_cor %>% data.frame( scale = 'Office' ), 
  allotment_cor %>% data.frame( scale = 'Allotment' ), 
  year_cor %>% data.frame( scale = 'Year' )) %>% 
  mutate( type = str_extract(pattern = '[A-Z]+', row.names(.))) %>%
  pivot_longer( cols = c(AFG:TREE), names_to = 'type2', values_to = 'r') %>% 
  filter( type != type2) %>% 
  mutate( scale = factor( scale, levels = c('Ecogroup', 'Office', 'Allotment', 'Year'), ordered = T)) %>%
  ggplot( aes( x =type, y= r, fill = scale)) + 
  geom_bar(position = position_dodge(), stat = 'identity') + 
  facet_wrap( type2 ~.   ) +
  scale_fill_manual( values = group_variance_colors[-1] )

group_variance_colors

bind_rows( 
  ecogroup_cor[1, ], 
  office_cor[1, ], 
  allotment_cor[1, ], 
  year_cor[1, ]) %>% 
  mutate( scale = c('ecogroup', 'office', 'allotment', 'year')) %>% 
  mutate( scale = factor( scale, levels = c('ecogroup', 'office', 'allotment', 'year'), ordered = T)) %>%
  pivot_longer( BG:TREE) %>% 
  ggplot(aes( x = name, y = value, fill = scale)) +
  geom_bar(stat = 'identity', position = position_dodge())
