rm(list = ls())
library(MuMIn)
library(tidyverse)
library(lme4)
source('code/analysis/parameters.R')

load('data/analysis_data/cover.rda')
attach(cover)

control_lmer$optCtrl$eval.max <- 1e8
control_lmer$optCtrl$iter.max <- 1e8

m_afgc <- read_rds('output/AFG_cover_trend_model.rds')

m_afgc <- update(m_afgc, REML = F )

m_afgc2 <- update( m_afgc, . ~ . - ecogroup:year2)
m_afgc3 <- update( m_afgc2, . ~ . - ecogroup)
m_afgc4 <- update( m_afgc3, . ~ . - year2)
m_afgc5 <- update( m_afgc4, . ~ . - (year2|ecogroup:office_label))
m_afgc6 <- update( m_afgc5, . ~ . - (year2|uname))

r.squaredGLMM(m_afgc )
r.squaredGLMM(m_afgc2 )
r.squaredGLMM(m_afgc3 )
r.squaredGLMM(m_afgc4 )
r.squaredGLMM(m_afgc5 )
r.squaredGLMM(m_afgc6 )

MuMIn::model.sel(m_afgc, m_afgc2, m_afgc3, m_afgc4)
