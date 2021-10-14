rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)
library(merTools)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'blm', 
  user = 'andy', 
  port = '5432'
)

source('code/analysis/plot_tools.R')

n_bootsims <- 10 # number of simulations for bootstrapping 

m <- read_rds('output/AFG_cover_trend_model.rds')
newdat <- m@frame %>% distinct(year2, ecogroup)

test <- bootMer(m, FUN = function(x) predict(x, newdata = newdat, re.form = NA), 
                nsim = 20, re.form = NA)

testCI <- apply( test$t, 2, quantile, c(0.025, 0.5, 0.975)) %>% 
  t() %>% 
  data.frame()

dat <- m@frame
dat$CI <- data.frame(testCI)

g <- dat %>%
  ggplot(aes( x = year2, y = value2))  + 
  geom_point() + 
  facet_wrap( ~ ecogroup) 

g + 
  geom_ribbon( 
    data = dat %>% distinct(year2, ecogroup, CI), 
    aes( x = year2, ymin = CI$X2.5., ymax = CI$X97.5., y = CI$X50.)) 
  
