rm(list = ls() )
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn )
options( na.action = na.fail )

source('code/analysis/plot_tools.R')
load('output/cover.rda')

attach(cover)
model_names <-str_remove_all( dir(path = 'output/', pattern = '*cover_trend_model.rds'), 
                pattern = '\\.rds')

model_names <- str_extract( model_names, '[A-Za-z]+')
model_paths <- dir(path = 'output/', pattern = '*cover_trend_model.rds', full.names = T)
n_models <- length(model_paths)

out <- list()
for( i in 1:2){ 
  
  #sub_dat <- cover[[i]] %>% filter( year > 2010, ecogroup %in% c('Warm Deserts', 'AZ/NM Highlands'))
  #sub_dat <- sub_dat[ complete.cases( sub_dat ) , ] 
  
  temp <- read_rds( model_paths[i])
  my_clust <- parallel::makeCluster(2, type = 'FORK')
  
  temp_table <- pdredge(temp, 
                        fixed = ~ ecogroup + year2, 
                        trace = 2, 
                        cluster = my_clust, REML = F)
  
  parallel::stopCluster(my_clust)
  temp_table
  
  write_rds( temp_table, file = file.path( 'output/tables', paste0( model_names, '_cover_model_evaluation.rds')))
} 
  


