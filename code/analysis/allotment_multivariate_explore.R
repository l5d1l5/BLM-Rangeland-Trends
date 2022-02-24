rm(list = ls())

# library(brms)
# options(buildtools.check = function(action) TRUE )

library(tidyverse)
library(lme4)
#library(climwin)
#library(spdep)
#library(glmmTMB) # For beta regression support 

allotment_data <- read_rds('data/allotment_data_long.rds')
allotment_info <- read_rds('data/basic_allotment_info.rds')

# Example allotment total cover over time 
# does not usually add up to 100%  
allotment_data %>% 
  filter( uname %in% unique(uname)[1:10]) %>% 
  filter( type %in% c('AFGC', 'BG', 'PFGC', 'LTR', 'SHR', 'TREE')) %>% 
  group_by( uname, year ) %>% 
  summarise(tcover =  sum(value )) %>% 
  mutate( uname = factor(uname)) %>%
  ggplot( aes( x = year, y = tcover, color = uname, group = uname )) + geom_line()

AFGC_data <- 
  allotment_data %>% 
  filter( type %in% c('AFGC', 'PFGC', 'BG', 'SHR', 'TREE', 'pdsi', 'tavg', 'pr') ) %>% 
  spread( type, value ) %>%
  left_join(allotment_info %>% 
              dplyr::distinct( uname, ADMIN_ST, ADM_OFC_CD , lon, lat , NA_L3NAME , elevation)) %>%
  group_by( uname ) %>% 
  mutate( pdsi = scale( pdsi), tavg = scale(tavg), pr = scale(pr))  %>% 
  mutate( pdsi_0 = lag(pdsi), tavg_0 = lag(tavg), pr_0 = lag(pr))  %>%
  mutate( AFGC_log = log( AFGC ), AFGC_log_0 = lag(AFGC_log)) %>% 
  mutate( cover_change = AFGC_log - AFGC_log_0) %>% 
  ungroup() %>%
  filter( complete.cases(.))  %>% 
  mutate( decade = factor( year %/% 10, 
                           labels = c('80s', '90s', '00s', '10s')) )

AFGC_data %>% 
  group_by( NA_L3NAME ) %>% 
  filter( n_distinct(uname) > 20 ) %>%
  filter( decade != '80s') %>% 
  ungroup() %>% 
  ggplot( aes( y =AFGC, x  = PFGC )) + 
  geom_hex(aes(fill = ..density.. )) + 
  facet_grid( decade ~ NA_L3NAME ) +
  scale_fill_gradient2(low = 'blue', mid = 'yellow', high = 'red', midpoint = 0.05) + 
  scale_y_log10() + 
  scale_x_log10()


AFGC_data %>% 
  group_by( NA_L3NAME ) %>% 
  filter( n_distinct(uname) > 20 ) %>%
  filter( decade != '80s') %>% 
  ungroup() %>% 
  ggplot( aes( y =BG, x  = PFGC )) + 
  geom_hex(aes(fill = ..ndensity.. )) + 
  facet_grid( decade ~ NA_L3NAME ) +
  scale_fill_gradient2(low = 'blue', mid = 'yellow', high = 'red', midpoint = 0.5)


AFGC_data %>% 
  group_by( NA_L3NAME ) %>% 
  filter( n_distinct(uname) > 20 ) %>%
  filter( decade != '80s') %>% 
  ungroup() %>% 
  ggplot( aes( y = TREE, x  = SHR )) + 
  geom_hex(aes(fill = ..ndensity.. )) + 
  facet_grid( decade ~ NA_L3NAME ) +
  scale_fill_gradient2(low = 'blue', mid = 'yellow', high = 'red', midpoint = 0.5)


community_comp <- 
  AFGC_data %>%
  filter( decade != '80s') %>%
  select( uname, year, decade, NA_L3NAME, ADMIN_ST, 
          ADM_OFC_CD, AFGC, BG, PFGC, TREE, SHR ) %>% 
  group_by( uname, ADMIN_ST, NA_L3NAME, ADM_OFC_CD) %>%
  summarise( AFGC = mean(AFGC), 
             BG = mean(BG), 
             PFGC = mean(PFGC), 
             TREE = mean(TREE), 
             SHR = mean(SHR)) %>%
  ungroup() %>%
  arrange( uname, ADMIN_ST, ADM_OFC_CD ) 

comp_mat <- community_comp %>% select( AFGC:SHR) %>% as.data.frame() 

library(vegan)
library(ggfortify)

comp_mat_scaled <- log( comp_mat  )
pca1 <- prcomp(comp_mat, scale = T)
summary( pca1 )
pca2 <- prcomp(comp_mat_scaled, scale = T)
env_df <- community_comp %>% select(NA_L3NAME)
efit <- envfit(pca2, env_df)
efit$factors$centroids
community_comp
pca1

efit_factors <- efit$factors$centroids %>% data.frame( NA_L3NAME = str_remove( row.names(.), 'NA_L3NAME'))

autoplot(object = pca2, 
         data = community_comp, 
         colour = "NA_L3NAME", alpha = 1) + 
  scale_color_brewer(type = 'qual')

geom_point(data = efit_factors, aes(x = PC1, y = PC2, color= NA_L3NAME))

pca1 <- prcomp(iris[, 1:4])
pca1
autoplot(pca1,data= iris, color = 'Species')
