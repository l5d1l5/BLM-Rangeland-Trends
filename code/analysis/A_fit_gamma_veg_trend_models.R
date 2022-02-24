rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(optimx)
library(dfoptim)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

load('data/analysis_data/cover.rda')
attach(cover)

control_lmer$optCtrl$eval.max <- 1e8
control_lmer$optCtrl$iter.max <- 1e8

trend_formula <- formula(value2 ~ year2 * ecogroup +
    (year2 | ecogroup:office_label ) + 
    (year2 | uname) 
)

trend_formula2 <- update(trend_formula, . ~ . - year2:ecogroup)
trend_formula3 <- update(trend_formula2, . ~ . - ecogroup)
trend_formula4 <- update(trend_formula2, . ~ . - year2)

# Now run with all data 
m_afgc_ML <- lmer(data = AFGC, 
               trend_formula, 
               control = control_lmer, REML = F)

m_afgc2 <- lmer(data = AFGC, trend_formula2, control_lmer, REML = F)
m_afgc3 <- lmer(data = AFGC, trend_formula3, control_lmer, REML = F)
m_afgc4 <- lmer(data = AFGC, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_afgc_ML, m_afgc2, m_afgc3, m_afgc4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/afgc_model_comparison_table.md')

m_afgc <- lmer(data = AFGC, 
               trend_formula, 
               control = control_lmer)

ecogroup_effects <- get_ecogroup_trends( m_afgc) 
random_effects <- get_blm_random_effects( m_afgc)

AFGC_trends <- blm_trend_summary( AFGC, ecogroup_effects, random_effects) 


# Fit perennial cover data 
m_pfgc_ML <- lmer(data = PFGC, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_pfgc2 <- lmer(data = PFGC, trend_formula2, control_lmer, REML = F)
m_pfgc3 <- lmer(data = PFGC, trend_formula3, control_lmer, REML = F)
m_pfgc4 <- lmer(data = PFGC, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_pfgc_ML, m_pfgc2, m_pfgc3, m_pfgc4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/pfgc_model_comparison_table.md')



m_pfgc <- lmer(data = PFGC, trend_formula, control = control_lmer)


ecogroup_effects <- get_ecogroup_trends(m_pfgc ) 
random_effects <- get_blm_random_effects( m_pfgc)
PFGC_trends <- blm_trend_summary( PFGC, ecogroup_effects , random_effects)

# Fit Bare Ground cover --------
m_bare_ML <- lmer(data = BG, 
                  trend_formula, 
                  control = control_lmer, REML = F)



m_bare2 <- lmer(data = BG, trend_formula2, control_lmer, REML = F)
m_bare3 <- lmer(data = BG, trend_formula3, control_lmer, REML = F)
m_bare4 <- lmer(data = BG, trend_formula4, control_lmer, REML = F)


table <- MuMIn::model.sel(m_bare_ML, m_bare2, m_bare3, m_bare4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')

kableExtra::save_kable(table, file = 'output/tables/bare_model_comparison_table.md')




m_bare <- lmer(data = BG, trend_formula, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_bare ) 
random_effects <- get_blm_random_effects( m_bare)
bare_trends <- blm_trend_summary( BG, ecogroup_effects , random_effects)

# Fit Shrub  cover --------
m_shrub_ML <- lmer(data = SHR, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_shrub2 <- lmer(data = SHR, trend_formula2, control_lmer, REML = F)
m_shrub3 <- lmer(data = SHR, trend_formula3, control_lmer, REML = F)
m_shrub4 <- lmer(data = SHR, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_shrub_ML, m_shrub2, m_shrub3, m_shrub4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/shrub_model_comparison_table.md')


m_shrub <- lmer(data = SHR, trend_formula, control = control_lmer)
ecogroup_effects <- get_ecogroup_trends(m_shrub ) 
random_effects <- get_blm_random_effects( m_shrub)

shrub_trends <- blm_trend_summary( SHR, ecogroup_effects , random_effects)

# Fit Tree  cover --------
m_tree_ML <- lmer(data = TREE, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_tree2 <- lmer(data = TREE, trend_formula2, control_lmer, REML = F)
m_tree3 <- lmer(data = TREE, trend_formula3, control_lmer, REML = F)
m_tree4 <- lmer(data = TREE, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_tree_ML, m_tree2, m_tree3, m_tree4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/tree_model_comparison_table.md')



m_tree <- lmer(data = TREE, trend_formula, control = control_lmer)


ecogroup_effects <- get_ecogroup_trends(m_tree ) 
random_effects <- get_blm_random_effects( m_tree)
tree_trends <- blm_trend_summary( TREE, ecogroup_effects , random_effects)

# Fit Woody Cover ------------------------------------------------ # 
m_woody_ML <- lmer(data = WOODY, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_woody2 <- lmer(data = WOODY, trend_formula2, control_lmer, REML = F)
m_woody3 <- lmer(data = WOODY, trend_formula3, control_lmer, REML = F)
m_woody4 <- lmer(data = WOODY, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_woody_ML, m_woody2, m_woody3, m_woody4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/woody_model_comparison_table.md')

m_woody <- lmer(data = WOODY, trend_formula, control = control_lmer)

ecogroup_effects <- get_ecogroup_trends(m_woody ) 
random_effects <- get_blm_random_effects( m_woody)
woody_trends <- blm_trend_summary( WOODY, ecogroup_effects , random_effects)

# ---------- Output 
saveRDS(m_afgc, file = 'output/AFG_cover_trend_model.rds')
saveRDS(m_pfgc, file = 'output/PFG_cover_trend_model.rds')
saveRDS(m_bare, file = 'output/BG_cover_trend_model.rds')
saveRDS(m_tree, file = 'output/TREE_cover_trend_model.rds')
saveRDS(m_shrub, file = 'output/SHR_cover_trend_model.rds')
saveRDS(m_woody, file = 'output/woody_cover_trend_model.rds')

save(m_afgc_ML, m_afgc2, m_afgc3, m_afgc4, file = 'output/all_annual_cover_models.rda')
save(m_pfgc_ML, m_pfgc2, m_pfgc3, m_pfgc4, file = 'output/all_perennial_cover_models.rda')
save(m_shrub_ML, m_shrub2, m_shrub3, m_shrub4, file = 'output/all_shrub_cover_models.rda')
save(m_bare_ML, m_bare2, m_bare3, m_bare4, file = 'output/all_bare_cover_models.rda')
save(m_tree_ML, m_tree2, m_tree3, m_tree4, file = 'output/all_tree_cover_models.rda')
saveRDS(m_woody_ML, m_woody2, m_woody3, my_woody4, file = 'output/all_woody_cover_models.rda')

write_csv(AFGC_trends, file = 'output/AFG_cover_group_trends.csv')
write_csv(PFGC_trends, file = 'output/PFG_cover_group_trends.csv')
write_csv(bare_trends, file = 'output/BG_cover_group_trends.csv')
write_csv(tree_trends, file = 'output/TREE_cover_group_trends.csv')
write_csv(shrub_trends, file = 'output/SHR_cover_group_trends.csv')
write_csv(woody_trends, file = 'output/WOODY_cover_group_trends.csv')

detach(cover )
rm( cover ) 

load('data/analysis_data/agb.rda')

attach(agb)
afg_attributes <- attributes( afgAGB$value2)
pfg_attributes <- attributes( pfgAGB$value2)
herb_attributes <- attributes(herbaceousAGB$value2)

unique( agb$afgAGB$ecogroup )
# test back-transformation 
stopifnot( 
  all.equal( back_transform(afgAGB$value2[1:10], afg_attributes), 
             afgAGB$value[1:10] ) ) 

# AFG TRENDS ------------------------------------------- # 
# AFG AGB TRENDS
m_afg_agb_ML <- lmer(data = afgAGB, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_afg_agb2 <- lmer(data = afgAGB, trend_formula2, control_lmer, REML = F)
m_afg_agb3 <- lmer(data = afgAGB, trend_formula3, control_lmer, REML = F)
m_afg_agb4 <- lmer(data = afgAGB, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_afg_agb_ML, m_afg_agb2, m_afg_agb3, m_afg_agb4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/afg_agb_model_comparison_table.md')


m_afg_agb <- lmer(formula = trend_formula, 
                  data = afgAGB, 
                  control = control_lmer)

afg_fixed <- get_ecogroup_trends(m_afg_agb)
afg_random <- get_blm_random_effects(m_afg_agb)
afg_trends <- blm_trend_summary( afgAGB, afg_fixed, afg_random)

stopifnot( all(complete.cases(afg_trends)))


# PFG AGB Trends 
m_pfg_agb_ML <- lmer(data = pfgAGB, 
                     trend_formula, 
                     control = control_lmer, REML = F)

m_pfg_agb2 <- lmer(data = pfgAGB, trend_formula2, control_lmer, REML = F)
m_pfg_agb3 <- lmer(data = pfgAGB, trend_formula3, control_lmer, REML = F)
m_pfg_agb4 <- lmer(data = pfgAGB, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_pfg_agb_ML, m_pfg_agb2, m_pfg_agb3, m_pfg_agb4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/pfg_agb_model_comparison_table.md')

#
m_pfg_agb <- lmer(formula = trend_formula, 
                  data = pfgAGB, 
                  control = control_lmer)

pfg_fixed <- get_ecogroup_trends(m_pfg_agb)
pfg_random <- get_blm_random_effects(m_pfg_agb)
pfg_trends <- blm_trend_summary( pfgAGB, pfg_fixed, pfg_random)


# Herbaceous: AFG + PFG = Total Herbaceous 
# m_herb_agb <- lmer(data = herbaceousAGB, 
#                    trend_formula, 
#                    control = control_lmer)
# 
# herb_fixed <- get_ecogroup_trends(m_herb_agb)
# herb_random <- get_blm_random_effects(m_herb_agb)
# herb_trends <- blm_trend_summary(herb_agb, herb_fixed, herb_random)


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
# saveRDS(m_herb_agb, file = 'output/HERB_agb_trend_model.rds')

write_csv(afg_trends, file = 'output/AFG_agb_group_trends.csv')
write_csv(pfg_trends, file = 'output/PFG_agb_group_trends.csv')
# write_csv(herb_trends, file = 'output/HERB_agb_group_trends.csv')

save(m_afg_agb_ML, m_afg_agb2, m_afg_agb3, m_afg_agb4, file = 'output/all_annual_prod_models.rda')
save(m_pfg_agb_ML, m_pfg_agb2, m_pfg_agb3, m_pfg_agb4, file = 'output/all_perennial_prod_models.rda')


detach(agb)
rm( agb, m_afg_agb, m_pfg_agb)
