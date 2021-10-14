rm(list = ls())
library(tidyverse)
library(sf)
library(lme4)
library(gridExtra)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')


# Functions ---------------------- # 
format_ranefs <- function( x , type = 'AFG'){ 
  
  x$uname <- 
    x$uname %>% 
    mutate( Group = rownames(.) ) %>% 
    mutate( scale = 'Allotment' , type = type )
  
  x$`ecogroup:office_label` <- 
    x$`ecogroup:office_label` %>% 
    mutate( Group = rownames(.) ) %>% 
    mutate( scale = 'Office' , type = type )
  
  return(x)
} 

unit_scale <- function( y ) { 
  (y - min(y,na.rm = T))/diff(range(y, na.rm = T)) 
}

get_pairs <- function(vars = c('AFG', 'BG'), cutoff = c(0.3, 0.3)){ 
  pixel_file <- paste0( 'data/RAP_EE_exports/', paste( tolower(vars), collapse = '_'), '_resid_samples.csv')
  
  out <- 
    ecogroup_year2 %>%
    bind_rows(
      office_year2 
    ) %>% 
    bind_rows(
      allotment_year2  
    ) %>%
    bind_rows( 
      read_csv(pixel_file ) %>%
        filter( abs( .data[[vars[[1]]]] )  < cutoff[1]) %>% 
        filter( abs( .data[[vars[[2]]]])   < cutoff[2]) %>% 
        mutate( scale = 'Pixel' )
    ) %>% 
    mutate( scale = factor(scale, 
                           levels = c('Ecogroup', 'Office', 'Allotment', 'Pixel'), 
                           ordered = T)) %>% 
    group_by( scale ) %>%
    mutate_at( vars, unit_scale)  
  
  return(out ) 
}

make_labels <- function( res, vars = c('AFG', 'BG') ) { 
  cortests <- res %>% 
    group_by( scale  ) %>% 
    summarise( res = list(cor.test(.data[[vars[1]]], .data[[vars[2]]])))
  
  cortests$pval <- unlist( lapply( cortests$res, FUN = function(x) x$p.value) )
  cortests$r <- unlist( lapply( cortests$res, FUN = function(x) x$estimate) )
  
  lab_df <- cortests %>% 
    select(scale, pval, r ) %>%
    mutate( lab = paste( "~italic(r)~'='~", round(r, 2) )) %>% 
    mutate( lab = ifelse( pval > 0.05, ' n.s.', lab ))
  
  return(lab_df)
} 

make_panel <- function( vars, title_text, cutoff = c(0.3,0.3) ) { 
  
  res <- get_pairs(vars, cutoff = cutoff ) 
  labels = make_labels(res, vars = vars)
  
  res <- left_join(res, labels[, c('scale', 'pval')]) %>% 
    mutate( dum = ifelse(pval < 0.05, T, F ))
  
  res %>%   
    ggplot( aes( x = .data[[vars[1]]], y = .data[[vars[2]]] )) + 
    geom_point(color = 'black', alpha = 0.5) + 
    facet_grid( ~ scale ) + 
    geom_smooth(data = res %>% filter(dum) , method = 'lm', se = F, size = 0.5) +
    geom_text( data = labels, 
               aes( label = lab,  
                    x = -Inf, y = -Inf), 
               hjust = 0, nudge_x = 1,
               vjust = -0.5, parse = T, size = 4) + 
    scale_x_continuous(breaks = c(0.2, 0.5, 0.8)) + 
    scale_y_continuous(breaks = c(0.2, 0.5, 0.8)) + 
    ggtitle(title_text ) + 
    coord_equal(1) + 
    theme_bw() 
}  


model_fls <- dir('output', pattern = 'cover_trend_model.rds' ,full.names =T )
model_names <- str_extract(basename(model_fls), pattern = '[A-Za-z]+_[a-z]+')

model_list <- data.frame( model_names = model_names , file = model_fls) %>% 
  separate( model_names , c('type', 'unit')) %>%
  mutate( type = str_to_upper(type )) %>% 
  filter( type != 'HERB' , unit != 'agb')

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

p1 <- make_panel(vars = c('AFG', 'BG'), title_text = 'A) Annuals vs. Bare Ground') 
p2 <- make_panel(vars = c('AFG', 'PFG'), title_text = 'B) Annuals vs. Perennials') 
p3 <- make_panel(vars = c('AFG', 'SHR'), title_text = 'C) Annuals vs. Shrubs', c(0.3, 0.2)) 
p4 <- make_panel(vars = c('PFG', 'BG'), title_text = 'D) Perennials vs. Bare Ground') 
p5 <- make_panel(vars = c('PFG', 'SHR'), title_text = 'E) Perennials vs. Shrubs', c(0.3, 0.2)) 
p6 <- make_panel(vars = c('SHR', 'BG'), title_text = 'F) Shrubs vs. Bare Ground' , c(0.2, 0.3)) 

ggsave(plot = grid.arrange( p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2), 
       filename =  'output/figures/Fig_trend_correlations.png', 
       width = 14, 
       height = 8, 
       units = 'in', 
       dpi = 600)

