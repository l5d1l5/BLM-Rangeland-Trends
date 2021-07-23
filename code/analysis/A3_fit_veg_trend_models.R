rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(optimx)
library(dfoptim)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

load('data/temp/cover.rda')
attach(cover)

# Fit annual cover data 
m_afgc <- lmer(data = AFGC, basic_form, control = my_control)
ecogroup_effects <- get_ecogroup_trends( m_afgc) 
random_effects <- get_blm_random_effects( m_afgc)
AFGC_trends <- blm_trend_summary( AFGC , ecogroup_effects, random_effects) 

# Fit perennial cover data 
m_pfgc <- lmer(data = PFGC, basic_form, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_pfgc ) 
random_effects <- get_blm_random_effects( m_pfgc)
PFGC_trends <- blm_trend_summary( PFGC, ecogroup_effects , random_effects)

# Fit Bare ground cover --------
m_bare <- lmer(data = BG, basic_form, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_bare ) 
random_effects <- get_blm_random_effects( m_bare)
bare_trends <- blm_trend_summary( BG, ecogroup_effects , random_effects)

# Fit Bare ground cover --------
m_tree <- lmer(data = TREE, basic_form, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_tree ) 
random_effects <- get_blm_random_effects( m_tree)
tree_trends <- blm_trend_summary( TREE, ecogroup_effects , random_effects)

# Fit Woody ground cover --------
m_shrub <- lmer(data = SHR, basic_form, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_shrub ) 
random_effects <- get_blm_random_effects( m_shrub)
shrub_trends <- blm_trend_summary( SHR, ecogroup_effects , random_effects)

# Fit total herbaceous ---------- 

m_herb <- lmer(data = HERB, basic_form, control = control_lmer)
# note total herbaceous cover is NOT log transformed 
ecogroup_effects <- get_ecogroup_trends(m_herb) 
random_effects <- get_blm_random_effects( m_herb)
herb_trends <- blm_trend_summary(HERB, ecogroup_effects , random_effects)

# ---------- Output 
saveRDS(m_afgc, file = 'output/AFG_cover_trend_model.rds')
saveRDS(m_pfgc, file = 'output/PFG_cover_trend_model.rds')
saveRDS(m_bare, file = 'output/BG_cover_trend_model.rds')
saveRDS(m_tree, file = 'output/TREE_cover_trend_model.rds')
saveRDS(m_shrub, file = 'output/SHR_cover_trend_model.rds')
saveRDS(m_herb, file = 'output/HERB_cover_trend_model.rds')

write_csv(AFGC_trends, file = 'output/AFG_cover_group_trends.csv')
write_csv(PFGC_trends, file = 'output/PFG_cover_group_trends.csv')
write_csv(bare_trends, file = 'output/BG_cover_group_trends.csv')
write_csv(tree_trends, file = 'output/TREE_cover_group_trends.csv')
write_csv(shrub_trends, file = 'output/SHR_cover_group_trends.csv')
write_csv(herb_trends, file = 'output/HERB_cover_group_trends.csv')

detach(cover )
rm( cover ) 

load('data/temp/agb.rda')
attach(agb)
afg_attributes <- attributes( afgAGB$value2)
pfg_attributes <- attributes( pfgAGB$value2)
herb_attributes <- attributes(herb_agb$value2)

# test back-transformation 
stopifnot( 
  all.equal( back_transform(afgAGB$value2[1:10], afg_attributes), 
             afgAGB$value[1:10] ) ) 

# AFG TRENDS
# Basic analysis formula for finding long-term annual trend in the data 

# sample_uname <- pfgAGB %>% 
#   select( ecogroup, office_label, uname ) %>% 
#   arrange( ecogroup,  office_label, uname) %>%
#   distinct() %>%
#   group_by(ecogroup, office_label) %>% 
#   filter( row_number() < 10 ) %>% 
#   pull(uname)
# 
# # 
# sample_dat <- pfgAGB %>% filter( uname %in% sample_uname )
# 
# m_pfg_agb <- lmer(formula = basic_form, 
#                   data = sample_dat)
# 
# # check singularity 
# tt <- getME(m_pfg_agb,"theta")
# ll <- getME(m_pfg_agb,"lower")
# min(tt[ll==0])
# 
# m_pfg_agb@optinfo
# summary(m_pfg_agb)
# 
# meth_tab <- data.frame( allFit(show.meth.tab=TRUE)  ) %>% 
#   bind_rows( data.frame( optimizer = 'optimx', method = c('nlminb' , 'bobyqa', 'BFGS')))
# 
# ncores <- parallel::detectCores()
# 
# diff_optims <- allFit(m_pfg_agb, 
#                       meth.tab = meth_tab, 
#                       maxfun = 1e5, 
#                       parallel = 'multicore', ncpus = ncores)
# 
# summary(diff_optims)

# AFG AGB TRENDS
m_afg_agb <- lmer(formula = basic_form, 
                  data = afgAGB, 
                  control = control_lmer)
afg_fixed <- get_ecogroup_trends(m_afg_agb)
afg_random <- get_blm_random_effects(m_afg_agb)
afg_trends <- blm_trend_summary( afgAGB, afg_fixed, afg_random)

stopifnot( all(complete.cases(afg_trends)))


# PFG AGB Trends 
m_pfg_agb <- lmer(formula = basic_form, 
                  data = pfgAGB, 
                  control = control_lmer)

pfg_fixed <- get_ecogroup_trends(m_pfg_agb)
pfg_random <- get_blm_random_effects(m_pfg_agb)
pfg_trends <- blm_trend_summary( pfgAGB, pfg_fixed, pfg_random)


# Herbaceous: AFG + PFG = Total Herbaceous 
m_herb_agb <- lmer(data = herb_agb, 
                   basic_form, 
                   control = control_lmer)
herb_fixed <- get_ecogroup_trends(m_herb_agb)
herb_random <- get_blm_random_effects(m_herb_agb)
herb_trends <- blm_trend_summary(herb_agb, herb_fixed, herb_random)


# Check convergence with different methods, could take a while ---------------------------- # 
# diff_optims <- allFit(m_pfg_agb, 
#                       meth.tab = meth_tab, 
#                       maxfun = 1e6, 
#                       parallel = 'multicore', 
#                       ncpus = ncores)

# ------------- # 

# SAVE AGB models and output 
# Herbaceous AGB Trends AFG + PFG = Total Herbaceous 

saveRDS(m_afg_agb, file = 'output/AFG_agb_trend_model.rds')
saveRDS(m_pfg_agb, file = 'output/PFG_agb_trend_model.rds')
saveRDS(m_herb_agb, file = 'output/HERB_agb_trend_model.rds')

write_csv(afg_trends, file = 'output/AFG_agb_group_trends.csv')
write_csv(pfg_trends, file = 'output/PFG_agb_group_trends.csv')
write_csv(herb_trends, file = 'output/HERB_agb_group_trends.csv')

detach(agb)
rm( agb, m_afg_agb, m_pfg_agb, m_herb_agb)
