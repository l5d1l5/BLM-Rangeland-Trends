# Tree Quantile regression 
rm(list = ls() )
library(quantreg)
library(tidyverse)
source('code/analysis/parameters.R')
source('code/analysis/functions.R')

tree_model <- read_rds('output/TREE_cover_trend_model.rds')
annual_data <- read_rds('data/temp/annual_data.rds')
allotments <- read_csv('data/temp/allotment_info.csv')

annual_data <- annual_data %>% 
  left_join(allotments, by = 'uname')

tree_data <- annual_data %>% 
  filter( year > 1990 ) %>% 
  filter( !is.na(value)) %>% 
  filter( ecogroup != "Marine West Coast Forest") %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover', type == 'TREE')

tree_data <- tree_data %>%
  split( ., .$ecogroup)

tree_qreg <- lapply( tree_data, function(x) rq( value ~ year , data = x , tau = c(0.25, 0.5, 0.75, 0.9)))

predict_qreg <- function( x, y ){ 
  yhat <- data.frame( predict( y, newdata = x %>% distinct(year) ) ) 
  ecogroup <- unique(x$ecogroup)
  names(yhat) <- c('25th', '50th', '75th', '90th')
  cbind( x%>% distinct(year), yhat) %>%
    pivot_longer( c('25th', '50th', '75th', '90th'), names_to = 'quantile', values_to = 'yhat') %>% 
    mutate( ecogroup = ecogroup )
}

yhats <- mapply(FUN = predict_qreg,  tree_data, tree_qreg, SIMPLIFY = F)

yhats <- do.call(rbind, yhats ) 
tree_dat2 <- do.call(bind_rows, tree_data)

coef_df <- lapply( tree_qreg, coefficients) 

coef_df <- lapply( coef_df, data.frame) %>% do.call(rbind, .  ) %>% 
  mutate( type = row.names(.)) %>% 
  separate( type , c('ecogroup', 'par'), sep = '\\.') %>% 
  mutate( ecogroup = str_trim(ecogroup))

label_df <- coef_df %>% 
  pivot_longer(starts_with('tau'), names_to = 'quantile', values_to = 'value') %>% 
  mutate( quantile = str_extract(quantile, '[0-9]+$') %>% paste0( . , 'th')) %>% 
  mutate( label = paste0( '' , round(value, 2))) %>% 
  select( ecogroup, par, quantile, label )

label_df1 <- 
  label_df %>% 
  left_join( 
    yhats %>% 
      group_by(ecogroup, quantile) %>% 
      summarise( year1 = max(year, na.rm = T), 
                 value = yhat[year==max(year, na.rm = T)]), 
    by = c('quantile', 'ecogroup')
  ) %>%
  filter( par == 'year', quantile %in% c('25th', '75th') )  %>% 
  rename( 'year' = year1)

label_df2 <- 
  label_df %>% 
  left_join( 
    yhats %>% 
      group_by(ecogroup, quantile) %>% 
      summarise( year1 = median(year, na.rm = T), 
                 value = yhat[ year == 2010]), 
    by = c('quantile', 'ecogroup')
  ) %>%
  filter( par == 'year', quantile %in% c('50th', '90th') ) %>% 
  rename( 'year' = year1)


label_df <- label_df1 %>% rbind(label_df2)

old_dat <- tree_model@frame 
year_att <- attributes(old_dat$year2)
value2_att <- attributes( old_dat$value2)

old_dat <- old_dat %>% 
  distinct(year2, ecogroup) %>% 
  mutate(yhat2 = predict(tree_model, newdata = ., re.form = NA))%>%
  mutate( year = back_transform( year2, year_att, log = F), 
          yhat = back_transform( yhat2, value2_att, log = T))


tree_dat2 %>%   
  ggplot( aes( x = year, y = value )) + 
  geom_point(alpha = 0.1) + 
  geom_line(data = yhats, aes( x = year, y = yhat, 
                               color = quantile, 
                               group = quantile), size = 1) + 
  geom_line( data = old_dat, aes( x = year , y = yhat ), linetype = 5, 
             color = 'white', size = 0.5) +
    ggrepel::geom_label_repel( data= label_df, 
                               aes(x = year, label = label, color = quantile), 
                             size = 2,
                               parse = T, 
                             nudge_y = 0.1,
                             box.padding = 0.05, 
                             min.segment.length = 2, 
                             show.legend = F) + 
  scale_color_brewer(type = 'seq', palette = 'Spectral', guide  = guide_legend(reverse = T)  ) + 
  facet_wrap( ~ ecogroup, nrow = 4, ncol = 2, scales = 'free_y' )  + 
  scale_x_continuous(breaks = c(1995, 2005, 2015)) + 
  ylab('Tree Cover %') + 
  theme( panel.grid = element_blank()) + 
  ggsave(filename = 'output/figures/Fig_S3_Supp_TREE_quantreg.png', 
         width = 8, height = 8, units ='in', dpi = 600)


