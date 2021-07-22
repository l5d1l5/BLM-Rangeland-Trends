# Graphical parameters and other settings shared across scripts.

require(tidyverse)
require(lme4)


my_colors0 <-
  c('sienna1', 'black', '8dd3c7', '#fb8072', '#80b1d3', 'magenta')
names(my_colors0) <-
  c("Annual", "Bare", "Perennial", "Shrub", "Tree", "Litter")

# lbs /acre to kg/ha
kgHa <- 1.12085

my_colors <- c(
  'Annual' = '#b2df8a',
  'Bare'  = 'darkgray',
  'Perennial' = '#1f78b4',
  'Shrub' = '#fb9a99',
  'Tree' = '#33a02c'
)

WESTERN_STATES <- c('AZ',
                    'CA',
                    'CO',
                    'ID',
                    'NE',
                    'NV',
                    'NM',
                    'ND',
                    'OR',
                    'MT',
                    'SD',
                    'UT',
                    'WA',
                    'WY')

ecogroup_colors <- RColorBrewer::brewer.pal(n = 7, 'Set2')
names(ecogroup_colors) <- c(
  'W Cold Deserts',
  'E Cold Deserts',
  'Great Plains',
  'Mediterranean California',
  'Forested Mts',
  'AZ/NM Highlands',
  'Warm Deserts'
)

# For relabeling figures with shorter labels
ecogroup_labels = c(
  'AZ/NM\nHighlands',
  'E Cold Deserts',
  'Great Plains',
  'Mediterranean\nCalifornia',
  'Forested Mts',
  'W Cold Deserts',
  'Warm Deserts'
)

group_variance_colors <-
  c('#f1b6da', 'cornsilk4', '#b8e186', '#4dac26', 'cadetblue3')
names(group_variance_colors) <-
  c('District', 'Field Office', 'Allotment', 'Year', 'Residual')

# Model Formula

basic_form <- formula(
  value2 ~ year2 * ecogroup +
    (year2 |
       office_label) + (year2 | uname) + (1 | year:climate_region)
)



# GLMER/LMER optimization options
control_lmer = lmerControl(
  optimizer = "optimx",
  calc.derivs = FALSE,
  optCtrl = list(
    method = "nlminb",
    starttests = FALSE,
    kkt = FALSE
  )
)

my_control =
  lmerControl(
    optimizer = 'optimx',
    optCtrl = list(
      method = 'nlminb',
      eval.max = 1e3,
      iter.max = 1e3
    )
  )