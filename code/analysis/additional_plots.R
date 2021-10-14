rm(list = ls())

library(tidyverse)
library(dbplyr)
library(ggridges)
library(lmerTest)
library(plotly)
library(gridExtra)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

source('code/analysis/plot_tools.R')

load('output/cover.rda')
cover <- do.call(rbind, cover ) 

ecogroup_colors <- ecogroup_colors[ order( names( ecogroup_colors )) ] 
names(ecogroup_colors ) <- ecogroup_labels

# 

cover <- 
  cover %>% 
  filter( type != 'HERB') %>% 
  mutate( type = factor( type , labels = names(my_colors))) %>% 
  mutate( ecogroup = factor( ecogroup, labels = str_squish(ecogroup_labels)))

plot_combos <- expand.grid( type = names(my_colors), 
                            ecogroup = str_squish(ecogroup_labels)) %>% 
  arrange( type )

pdf( file = 'output/figures/ecogroup_cover_detail_plots.pdf', 
     paper = 'letter', width = 7, height = 9.5)

for( i in seq( 1, nrow(plot_combos), by = 2)) { 

  p1 <- ecogroup_detail_plot(cover, 
                             sel_type = plot_combos$type[i], 
                             sel_ecogroup = plot_combos$ecogroup[i]) + 
    theme(strip.text = element_text(size = 4), 
          axis.text.x = element_text( size = 5)) 
  
  p2 <- ecogroup_detail_plot(cover, 
                             sel_type = plot_combos$type[i+1], 
                             sel_ecogroup = plot_combos$ecogroup[i+1]) + 
    theme(strip.text = element_text(size = 4), 
          axis.text.x = element_text( size = 5)) 
  
  if( nrow(p2$data) == 0 ){ 
    grid.arrange(p1, ncol = 1)
  }else if( nrow( p2$data) > 0 ){ 
    grid.arrange(p1, p2, ncol = 1) 
  }
}
dev.off() 

# ----------------------------------- # 


  