rm(list = ls())
library(lme4)
library(tidyverse)
library(papeR)
library(knitr)
library(kableExtra)

model_files <- dir('output', pattern = 'cover_trend_model.rds', full.names = T)
model_type <- str_extract( basename(model_files), '[A-Za-z]+')

for( i in 1:length(model_files) ) { 
  m <- read_rds( model_files[i] )
  
  mtab <- m %>% broom.mixed::tidy() %>% 
    mutate( term = str_replace( term, pattern = 'year2', replacement = 'year')) %>% 
    mutate( group = str_replace_all( group, pattern = c('uname' = 'Allotment', 'office_label' = 'Office')))  %>% 
    dplyr::select( group, term, estimate, std.error ) 
  
  group_n <- summary(m) %>% .$ngrps 
  num_obs <- summary(m) %>% .$residuals %>% length()
  group_n <- c(total.obs = num_obs, group_n )
  
  
  model_title <- paste( paste0( LETTERS[i], ')'), model_type[i], 'Cover Model')
  
  table_output <- list(   mtab %>%
                            filter( is.na(group )) %>%
                            dplyr::select( - group ) %>%
                            kbl(caption = model_title, digits = c(3)) , 
                          
                          mtab %>% filter( !is.na(group )) %>% 
                            dplyr::select( - std.error) %>%
                            kbl( caption = "Random Effects", digits = 3) , 
                          
                          group_n %>% data.frame(N = . )  %>% 
                            mutate( group =  row.names(.) ) %>% 
                            mutate( group = str_replace_all( group , 
                                                             pattern = c('uname' = 'Allotment', 'office_label' = 'Office'))) %>%
                            pivot_wider( names_from = group, values_from = N) %>%
                            kbl(caption = 'Groups')
                          )

  file_name <- file.path('output/tables', paste0( model_type[i], '_cover.html')) 
  
  table_output <- lapply( table_output , function(x) x %>%kable_classic_2(html_font = 'arial', font_size = 10))  

  table_output %>%  
    save_kable(file = file_name )
}

# Print out Biomass models:
model_files <- dir('output', pattern = 'agb_trend_model.rds', full.names = T)
model_type <- str_extract( basename(model_files), '[A-Za-z]+')

for( i in 1:length(model_files) ) { 
  m <- read_rds( model_files[i] )
  
  mtab <- m %>% broom.mixed::tidy() %>% 
    mutate( term = str_replace( term, pattern = 'year2', replacement = 'year')) %>% 
    mutate( group = str_replace_all( group, pattern = c('uname' = 'Allotment', 'office_label' = 'Office')))  %>% 
    dplyr::select( group, term, estimate, std.error ) 
  
  group_n <- summary(m) %>% .$ngrps 
  num_obs <- summary(m) %>% .$residuals %>% length()
  group_n <- c(total.obs = num_obs, group_n )
  
  model_title <- paste( paste0( LETTERS[i], ')'), model_type[i], 'Biomass Model')
  
  table_output <- list(   mtab %>%
                            filter( is.na(group )) %>%
                            dplyr::select( - group ) %>%
                            kbl(caption = model_title, digits = c(3)) , 
                          
                          mtab %>% filter( !is.na(group )) %>% 
                            dplyr::select( - std.error) %>%
                            kbl( caption = "Random Effects", digits = 3) , 
                          
                          group_n %>% data.frame(N = . )  %>% 
                            mutate( group =  row.names(.) ) %>% 
                            mutate( group = str_replace_all( group , 
                                                             pattern = c('uname' = 'Allotment', 'office_label' = 'Office'))) %>%
                            pivot_wider( names_from = group, values_from = N) %>%
                            kbl(caption = 'Groups')
  )
  
  file_name <- file.path('output/tables', paste0( model_type[i], '_biomass.html')) 
  
  table_output <- lapply( table_output , function(x) x %>%kable_classic_2(html_font = 'arial', font_size = 10))  
  
  table_output %>%  
    save_kable(file = file_name )
}

########  Do phenology models ----------------- # 
model_files <- dir('output', pattern = '*phenology_models.rda', full.names = T)
load(model_files)

model_list <- list( SOS_mer, MS_mer, EOS_mer, GSL_mer)
model_type <- c('SOS', 'MS', 'EOS', 'GSL')

for( i in 1:length(model_list)){ 
  
  m <- model_list[[i]]
  
  mtab <- m %>% broom.mixed::tidy() %>% 
    mutate( term = str_replace( term, pattern = 'year2', replacement = 'year')) %>% 
    mutate( group = str_replace_all( group, pattern = c('uname' = 'Allotment', 'office_label' = 'Office')))  %>% 
    dplyr::select( group, term, estimate, std.error ) 
  
  
  group_n <- summary(m) %>% .$ngrps 
  num_obs <- summary(m) %>% .$residuals %>% length()
  group_n <- c(total.obs = num_obs, group_n )

  model_title <- paste( paste0( LETTERS[i], ')'), model_type[i], 'phenology model')
  
  table_output <- list(   mtab %>%
                            filter( is.na(group )) %>%
                            dplyr::select( - group ) %>%
                            kbl(caption = model_title, digits = c(3)) , 
                          
                          mtab %>% filter( !is.na(group )) %>% 
                            dplyr::select( - std.error) %>%
                            kbl( caption = "Random Effects", digits = 3) , 
                          
                          group_n %>% data.frame(N = . )  %>% 
                            mutate( group =  row.names(.) ) %>% 
                            mutate( group = str_replace_all( group , 
                                                             pattern = c('uname' = 'Allotment', 'office_label' = 'Office'))) %>%
                            pivot_wider( names_from = group, values_from = N) %>%
                            kbl(caption = 'Groups')
  )
  
  file_name <- file.path('output/tables', paste0( model_type[i], '_phenology.html')) 

  table_output <- lapply( table_output , function(x) x %>%kable_classic_2(html_font = 'arial', font_size = 10))  
  
  table_output %>%  
    save_kable(file = file_name )
}
