# Graphical parameters and other settings shared across scripts.

require(tidyverse)
require(lme4)

error_scale_colors <- RColorBrewer::brewer.pal(3, 'Set2')
names(error_scale_colors) <- c('Office', 'Allotment', 'Pixel')

my_colors0 <-
  c('sienna1', 'black', '8dd3c7', '#fb8072', '#80b1d3', 'magenta')
names(my_colors0) <-
  c("Annual", "Bare", "Perennial", "Shrub", "Tree", "Litter")

type_labels <- c('AFG' = 'Annual', 
                 'PFG' = 'Perennial', 
                 'BG' = 'Bare Ground',
                 'HERB' = 'Herbaceous', 
                 'TREE' = 'Tree', 
                 'SHR' = 'Shrub', 
                 'LTR' = 'Litter')

unit_labels  <- c('agb' = 'production', 
                  'cover' = 'cover')

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

ecogroup_colors <- RColorBrewer::brewer.pal(n = 8, 'Set2')

ecogroup_colors

names(ecogroup_colors) <- c(
  'W Cold Deserts', 
  'E Cold Deserts',
  'Forested Mts', 
  'Mediterranean California',
  'N Great Plains',
  'S Great Plains',
  'AZ/NM Highlands',
  'Warm Deserts'
)
#colors()
ecogroup_colors['Mediterranean California']  <- 'firebrick2'
ecogroup_colors['Forested Mts']  <- 'cornflowerblue'
ecogroup_colors['Warm Deserts']  <- 'slategray'

# For relabeling figures with shorter labels
ecogroup_labels = c(
  'AZ/NM\nHighlands',
  'E Cold Deserts',
  'Forested Mts',
  'Mediterranean\nCalifornia',
  'N Great Plains', 
  'S Great Plains',
  'W Cold Deserts',
  'Warm Deserts'
)

group_variance_colors <-
  c('#f1b6da', 'cornsilk4', '#b8e186', '#4dac26', 'cadetblue3')
names(group_variance_colors) <-
  c('District', 'Field Office', 'Allotment', 'Year', 'Residual')


# LMER optimization options
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