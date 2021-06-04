rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)
library(GGally)
source('code/analysis/plot_tools.R')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

allotments <- tbl(con, 'allotments') %>% 
  select( uname, allot_name, admin_st, 
          parent_cd, parent_name, admu_name, 
          ecogroup, acres) %>% 
  mutate( district_label = str_remove( parent_name, ' District.*$')) %>% 
  mutate( 
    office_label = str_remove_all(str_squish(str_trim (admu_name) ), 
                                  pattern = c(' Field.*$'))) 


trend_files <- dir(path = 'output', pattern = 'cover_group_trends.csv', full.names = T)
types <- str_extract(basename(trend_files), '[A-Z]+')

res <- lapply( trend_files, read_csv) 
names(res) <- types 
res <- mapply( x = res, y = types, function(x, y) data.frame(x, type = y), SIMPLIFY = F)

all_res <- do.call(rbind, res )

full_pairs_plot <- all_res %>% 
  select( full_trend, uname, ecogroup, type ) %>% 
  pivot_wider(names_from = type, values_from = full_trend) %>% 
  ggpairs(columns = c('AFG', 'BG', 'PFG', 'SHR', 'TREE') )

allotment_pairs_plot <- 
  all_res %>% 
  select( allot_trend, uname, ecogroup, type ) %>% 
  pivot_wider(names_from = type, values_from = allot_trend) %>% 
  ggpairs(columns = c('AFG', 'BG', 'PFG', 'SHR', 'TREE') )

office_pairs_plot <- 
  all_res %>% 
  distinct( office_trend, office_label, ecogroup, type ) %>% 
  pivot_wider(names_from = type, values_from = office_trend) %>% 
  ggpairs(columns = c('AFG', 'BG', 'PFG', 'SHR', 'TREE') )

district_pairs_plot <- 
  all_res %>% 
  distinct( district_trend, district_label, ecogroup, type ) %>% 
  pivot_wider(names_from = type, values_from = district_trend) %>%
  ggpairs(columns = c('AFG', 'BG', 'PFG', 'SHR', 'TREE') )

ecogroup_pairs_plot <- 
  all_res %>% 
  distinct( ecogroup_trend, ecogroup, type ) %>% 
  pivot_wider(names_from = type, values_from = ecogroup_trend) %>%
  ggpairs(columns = c('AFG', 'BG', 'PFG', 'SHR', 'TREE') )

ggsave(plot = full_pairs_plot, 
       filename = 'output/figures/full_trend_cover_pairsplot.png', 
         width = 10, height = 7, dpi = 'print') 

ggsave(plot = allotment_pairs_plot, 
       filename = 'output/figures/allotment_trend_cover_pairsplot.png', 
       width = 10, height = 7, dpi = 'print') 

ggsave(plot = office_pairs_plot, 
       filename = 'output/figures/office_trend_cover_pairsplot.png', 
       width = 10, height = 7, dpi = 'print') 

ggsave(plot = district_pairs_plot, 
       filename = 'output/figures/district_trend_cover_pairsplot.png', 
       width = 10, height = 7, dpi = 'print') 

ggsave(plot = ecogroup_pairs_plot, 
       filename = 'output/figures/ecogroup_trend_cover_pairsplot.png', 
       width = 10, height = 7, dpi = 'print') 

# 
AFG_BG_correlations <- 
  all_res %>% 
  filter(type %in% c('AFG', 'BG')) %>% 
  select(ecogroup, district_label, office_label, uname, type, allot_trend, 
         office_trend, district_trend, ecogroup_trend, full_trend )

scale_comparison <- 
  AFG_BG_correlations %>% 
  distinct(type, uname, allot_trend) %>%
  pivot_wider(names_from = type, values_from = allot_trend ) %>% 
  mutate( scale = 'Allotment') %>% 
  rename( label = uname) %>% 
  mutate( label = as.character(label)) %>% 
  bind_rows(
    AFG_BG_correlations %>% 
      distinct(type, office_label, office_trend) %>%
      pivot_wider(names_from = type, values_from = office_trend ) %>% 
      mutate( scale = 'Field Office') %>%
      rename( label = office_label)
  ) %>% 
  bind_rows(
    AFG_BG_correlations %>% 
      distinct(type, district_label, district_trend) %>%
      pivot_wider(names_from = type, values_from = district_trend ) %>% 
      mutate( scale = 'BLM District') %>% 
      rename( label = district_label)
  ) %>% 
  bind_rows(
    AFG_BG_correlations %>% 
      distinct(type, ecogroup, ecogroup_trend) %>%
      pivot_wider(names_from = type, values_from = ecogroup_trend ) %>% 
      mutate( scale = 'Ecoregion') %>% 
      rename( label = ecogroup)
  ) %>% 
  bind_rows(
    AFG_BG_correlations %>% 
      distinct(uname, full_trend, type ) %>%
      pivot_wider(names_from = type, values_from = full_trend ) %>% 
      mutate( scale = 'Total') %>% 
      rename( label = uname) %>% 
      mutate( label = as.character( label )) 
  ) 

scale_rhos <- 
  scale_comparison %>% 
  filter( scale != 'Total') %>% 
  group_by( scale ) %>% 
  summarise( rho = list(cor.test(AFG, BG))) %>% 
  rowwise() %>%
  mutate( label  = paste0('r==', sprintf('%.2f', rho$estimate)))

scale_comparison %>% 
  filter( scale != 'Total' )  %>% 
  ggplot( aes( x = AFG, y = BG )  ) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = F, size = 0.5) + 
  geom_text( data = scale_rhos, 
             aes( x = Inf, y = Inf, label = label), vjust = 2, hjust = 1, parse = T) +  
  facet_wrap( ~ factor( scale, levels = c('Ecoregion', 'BLM District', 'Field Office', 'Allotment'), ordered = T), 
              scale = 'free') + 
  ylab( 'Bare Ground Cover Trend') + 
  xlab( 'Annual Forb/Grass Cover Trend') + 
  theme_bw() + 
  ggsave( filename = 'output/figures/BG_AFG_trend_correlations.png', 
          width = 5, height = 5, dpi = 'print')

# PFG to BG comparison 
PFG_BG_correlations <- 
  all_res %>% 
  filter(type %in% c('PFG', 'BG')) %>% 
  select(ecogroup, district_label, office_label, uname, type, allot_trend, 
         office_trend, district_trend, ecogroup_trend, full_trend )

scale_comparison <- 
  PFG_BG_correlations %>% 
  distinct(type, uname, allot_trend) %>%
  pivot_wider(names_from = type, values_from = allot_trend ) %>% 
  mutate( scale = 'Allotment') %>% 
  rename( label = uname) %>% 
  mutate( label = as.character(label)) %>% 
  bind_rows(
    PFG_BG_correlations %>% 
      distinct(type, office_label, office_trend) %>%
      pivot_wider(names_from = type, values_from = office_trend ) %>% 
      mutate( scale = 'Field Office') %>%
      rename( label = office_label)
  ) %>% 
  bind_rows(
    PFG_BG_correlations %>% 
      distinct(type, district_label, district_trend) %>%
      pivot_wider(names_from = type, values_from = district_trend ) %>% 
      mutate( scale = 'BLM District') %>% 
      rename( label = district_label)
  ) %>% 
  bind_rows(
    PFG_BG_correlations %>% 
      distinct(type, ecogroup, ecogroup_trend) %>%
      pivot_wider(names_from = type, values_from = ecogroup_trend ) %>% 
      mutate( scale = 'Ecoregion') %>% 
      rename( label = ecogroup)
  ) %>% 
  bind_rows(
    PFG_BG_correlations %>% 
      distinct(uname, full_trend, type ) %>%
      pivot_wider(names_from = type, values_from = full_trend ) %>% 
      mutate( scale = 'Total') %>% 
      rename( label = uname) %>% 
      mutate( label = as.character( label )) 
  ) 

scale_rhos <- 
  scale_comparison %>% 
  filter( scale != 'Total') %>% 
  group_by( scale ) %>% 
  summarise( rho = list(cor.test(PFG, BG))) %>% 
  rowwise() %>%
  mutate( label  = paste0('r==', sprintf('%.2f', rho$estimate)))

scale_comparison %>% 
  filter( scale != 'Total' )  %>% 
  ggplot( aes( x = PFG, y = BG )  ) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = F, size = 0.5) + 
  geom_text( data = scale_rhos, 
             aes( x = Inf, y = Inf, label = label), vjust = 2, hjust = 1, parse = T) +  
  facet_wrap( ~ factor( scale, levels = c('Ecoregion', 'BLM District', 'Field Office', 'Allotment'), ordered = T), 
              scale = 'free') + 
  ylab( 'Bare Ground Cover Trend') + 
  xlab( 'Perennial Forb/Grass Cover Trend') + 
  theme_bw() + 
  ggsave( filename = 'output/figures/BG_PFG_trend_correlations.png', 
          width = 5, height = 5, dpi = 'print')


# AFG to PFG trend correlations 
PFG_AFG_correlations <- 
  all_res %>% 
  filter(type %in% c('AFG', 'PFG')) %>% 
  select(ecogroup, district_label, office_label, uname, type, 
         allot_trend, office_trend, district_trend, ecogroup_trend, full_trend )

scale_comparison <- 
  PFG_AFG_correlations %>% 
  distinct(type, uname, allot_trend) %>%
  pivot_wider(names_from = type, values_from = allot_trend ) %>% 
  mutate( scale = 'Allotment') %>% 
  rename( label = uname) %>% 
  mutate( label = as.character(label)) %>% 
  bind_rows(
    PFG_AFG_correlations %>% 
      distinct(type, office_label, office_trend) %>%
      pivot_wider(names_from = type, values_from = office_trend ) %>% 
      mutate( scale = 'Field Office') %>%
      rename( label = office_label)
  ) %>% 
  bind_rows(
    PFG_AFG_correlations %>% 
      distinct(type, district_label, district_trend) %>%
      pivot_wider(names_from = type, values_from = district_trend ) %>% 
      mutate( scale = 'BLM District') %>% 
      rename( label = district_label)
  ) %>% 
  bind_rows(
    PFG_AFG_correlations %>% 
      distinct(type, ecogroup, ecogroup_trend) %>%
      pivot_wider(names_from = type, values_from = ecogroup_trend ) %>% 
      mutate( scale = 'Ecoregion') %>% 
      rename( label = ecogroup)
  ) %>% 
  bind_rows(
    PFG_AFG_correlations %>% 
      distinct(uname, full_trend, type ) %>%
      pivot_wider(names_from = type, values_from = full_trend ) %>% 
      mutate( scale = 'Total') %>% 
      rename( label = uname) %>% 
      mutate( label = as.character( label )) 
  ) 

scale_rhos <- 
  scale_comparison %>% 
  filter( scale != 'Total') %>% 
  group_by( scale ) %>% 
  summarise( rho = list(cor.test(PFG, AFG))) %>% 
  rowwise() %>%
  mutate( label  = paste0('r==', sprintf('%.2f', rho$estimate)))

scale_comparison %>% 
  filter( scale != 'Total' )  %>% 
  ggplot( aes( x = PFG, y = AFG )  ) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = F, size = 0.5) + 
  geom_text( data = scale_rhos, 
             aes( x = Inf, y = Inf, label = label), vjust = 2, hjust = 1, parse = T) +  
  facet_wrap( ~ factor( scale, levels = c('Ecoregion', 'BLM District', 'Field Office', 'Allotment'), ordered = T), 
              scale = 'free') + 
  ylab( 'Bare Ground Cover Trend') + 
  xlab( 'Perennial Forb/Grass Cover Trend') + 
  theme_bw() + 
  ggsave( filename = 'output/figures/AFG_PFG_trend_correlations.png', 
          width = 5, height = 5, dpi = 'print')


# 
bare_m <- read_rds('output/BG_cover_trend_model.rds')
BG_trends<- read_csv( file = 'output/BG_cover_group_trends.csv' ) 
AFGC_trends <- read_csv(file = 'output/AFG_cover_group_trends.csv')

BG_trends %>% 
  left_join(AFGC_trends %>% select( - ecogroup, -district_label, -office_label ), 
            by = 'uname') %>% 
  ggplot( aes( x = full_trend.y, y = full_trend.x )) + 
  geom_point() + 
  geom_smooth(method = 'lm', aes( group = district_label), se = F) + 
  facet_wrap( ~ ecogroup ) 

BG_trends %>%
  select( uname, allot_trend, ecogroup ) %>% 
  rename( BG_trend = allot_trend ) %>% 
  left_join(
    AFGC_trends %>% 
      select( uname, allot_trend) %>% 
      rename( AFGC_trend = allot_trend) 
  ) %>% 
  ggplot( aes( x = AFGC_trend, y = BG_trend )) + 
  geom_point() + 
  geom_smooth( method = 'lm', se = F) + 
  facet_wrap( ~ ecogroup )

BG_trends %>%
  distinct( ecogroup_trend,  ecogroup ) %>% 
  rename( BG_trend = ecogroup_trend ) %>% 
  left_join(
    AFGC_trends %>% 
      distinct( ecogroup_trend, ecogroup) %>% 
      rename( AFGC_trend = ecogroup_trend) 
  ) %>% 
  ggplot( aes( x = AFGC_trend, y = BG_trend )) + 
  geom_point() + 
  geom_smooth( method = 'lm', se = F)

BG_trends %>%
  distinct( office_trend, office_label, ecogroup ) %>% 
  rename( BG_trend = office_trend ) %>% 
  left_join(
    AFGC_trends %>% 
      distinct( office_trend, office_label, ecogroup) %>% 
      rename( AFGC_trend = office_trend) 
  ) %>% 
  ggplot( aes( x = AFGC_trend, y = BG_trend )) + 
  geom_point() + 
  geom_smooth( method = 'lm', se = F) + 
  facet_wrap( ~ ecogroup )


BG_trends %>%
  distinct( district_trend, district_label ) %>% 
  rename( BG_trend = district_trend ) %>% 
  left_join(
    AFGC_trends %>% 
      distinct( district_trend, district_label, ecogroup) %>% 
      rename( AFGC_trend = district_trend) 
  ) %>% 
  ggplot( aes( x = AFGC_trend, y = BG_trend )) + 
  geom_point() + 
  geom_smooth( method = 'lm', se = F) + 
  facet_wrap( ~ ecogroup )

# 
# first_ten_avg <- 
#   tbl(con, 'annual_data') %>% 
#   filter( year < 1995 ) %>% 
#   group_by( uname, type) %>%
#   summarise( avg = mean ( value ) ) %>% 
#   pivot_wider(id_cols = uname, names_from = type, values_from = avg) %>% 
#   left_join(allotments) %>% 
#   collect()
# 
# first_ten_avg %>%
#   left_join(AFGC_trends) %>% 
#   ggplot( aes( x = BG, y = full_trend )) + 
#   geom_point() + 
#   geom_smooth(method = 'lm') + 
#   facet_wrap( ~ ecogroup )
# 
# first_ten_avg %>%
#   left_join(AFGC_trends) %>% 
#   ggplot( aes( x = PFGC, y = full_trend )) + 
#   geom_point() + 
#   geom_smooth(method = 'lm') + 
#   facet_wrap( ~ ecogroup )

