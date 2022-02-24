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
  pixel_file <- paste0( 'data/RAP_EE_exports/', paste( tolower(vars), collapse = '_'), '_paired_samples.csv')
  pix_dat <- read_csv(pixel_file)
  col_names <- names(pix_dat) 
  
  col_names[ str_detect( col_names, vars[1]) ] <- vars[1]
  col_names[ str_detect( col_names, vars[2]) ] <- vars[2]
  names( pix_dat ) <- col_names
  
  
  out <- 
    ecogroup_year2 %>%
    bind_rows(
      office_year2 
    ) %>% 
    bind_rows(
      allotment_year2  
    ) %>%
    bind_rows( 
        pix_dat %>% 
        filter( abs( .data[[vars[[1]]]])  < cutoff[1]) %>% 
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

i <- 1 

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

afg_v_BG_PFG <- p1$data %>% 
  select( Group, scale, param, AFG, BG, dum, pval, .group) %>% 
  pivot_longer(cols = c(BG), names_to = 'type')  %>% 
  bind_rows(
    p2$data %>% 
    select( Group, scale, param, AFG, PFG, dum, pval, .group) %>% 
    pivot_longer(cols = c(PFG), names_to = 'type') 
  )  %>% 
  mutate( type = factor( type, labels = c("Bare Ground Trend", "Perennial Cover Trend")))


labels <- afg_v_BG_PFG %>% ungroup %>% 
  distinct( type, scale, dum, pval ) %>% 
  mutate( AFG = 0.2, value = 0.1 )


x <- afg_v_BG_PFG %>% 
  group_by(scale,type ) %>% 
  summarise( 
    res = list( cor.test( AFG, value ))
  ) 

x <- x %>% 
  rowwise( ) %>% 
  mutate( r = res$estimate[1] ) %>% 
  mutate( pval = res$p.value[1]) %>% 
  mutate( label = paste( "~italic(r)~'='~", round( r, 2))) %>% 
  filter( pval < 0.05) %>% 
  mutate( AFG = 0.2, value = 0.01) 
  
afg_v_BG_PFG %>% 
  ggplot( aes( x = AFG, y = value) ) + 
  facet_grid( type ~ scale, switch = 'y' ) + 
  geom_point(alpha = 0.5) + 
  geom_smooth( data = afg_v_BG_PFG %>% filter( dum) , se = F, method = 'lm') + 
  geom_text( data = x, 
             aes( label = label ), parse = T) + 
  theme_bw() + 
  xlab( 'Annual Cover Trend')  + 
  #scale_x_continuous(breaks = c( 0, 0.2, 0.4, 0.6, 0.8, 1) , labels = function(x) { sprintf( "%.1f" , x )})
  theme(strip.placement = 'outside', 
        strip.background.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_text( size = 7), 
        strip.text.y = element_text( size = 10)) + 
  ggsave( 'output/figures/Fig_6_trend_correlation.png', 
          width = 7, height = 4, units = 'in', dpi = 600)


ggsave(plot = grid.arrange( p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2), 
       filename =  'output/figures/Fig_S5_trend_correlations.png', 
       width = 14, 
       height = 8, 
       units = 'in', 
       dpi = 600)

