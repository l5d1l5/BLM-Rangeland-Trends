# The purpose of this analysis is to perform trend analysis while preserving 
# individual pixel level data in each allotment. 
# Analyze trends with random effects for pixel, allotment, office, district, etc. 
rm(list = ls())
library(tidyverse)
library(lme4)

allotments <- read_csv('data/temp/allotment_info.csv')
veg <- read_csv('~/Downloads/RAP_allotment_pixel_samples (1).csv')
pts <- read_csv('~/Downloads/RAP_sample_points.csv')

pts <- pts %>%
  separate(`system:index`, c('id', 'sample'),sep =  '_' ) %>%
  rename( uname = first ) %>% 
  left_join(allotments)

veg <- veg %>% 
  separate( `system:index`, c('year', 'id', 'sample'), sep = '_') %>%
  left_join(pts, by = c('id', 'sample')) 


# veg %>% 
#   filter( uname < 100 ) %>%
#   ggplot( aes( x = year, y = AFGC, color = uname, group = sample )) + 
#   geom_point() + 
#   geom_line() + 
#   #geom_smooth(se = F, method = 'lm') + 
#   facet_wrap( ~ uname)
#             
veg <- veg %>%
  mutate( year = as.numeric(str_extract(year, '\\d+'))) %>%
  mutate( pixel = paste( uname, sample , sep = '_'))  %>% 
  mutate( year2 = scale(year))

veg %>% 
  ggplot() + 
  geom_histogram(aes( x = AFGC))

m1 <- glmer( AFGC ~ year2 + (year2|uname/sample) + (1|uname:sample:year),
             data = veg %>% filter( uname < 200 ), 
             family = 'poisson', control = glmerControl(optimizer = "nlminbwrap"))

m2 <- glmer( PFGC ~ year2 + (year2|uname/sample), 
             data = veg %>% filter( uname < 200), 
             family = 'poisson', control = glmerControl(optimizer = "nlminbwrap"))

summary(m1)
summary(m2)

plot(m1)
plot(m2)
hist(abs( resid(m1)))
hist(abs( resid(m2)))

