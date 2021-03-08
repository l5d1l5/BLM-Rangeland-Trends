rm(list = ls())

library(tidyverse)
library(googledrive)

googledrive::drive_download(file = 'RAP_EE_exports/BLM_allotment_cover_median.csv', 
                            path = '~/Dropbox/projects/BLM_rangeland_trends/data/cover_median.csv', overwrite = T)

googledrive::drive_download(file = 'RAP_EE_exports/MT_BLM_cover_stdDev.csv', 
                            path = '~/Dropbox/projects/BLM_rangeland_trends/data/cover_stdDev.csv', overwrite = T)

googledrive::drive_download(file = 'RAP_EE_exports/MT_BLM_cover_hist.csv', 
                            path = '~/Dropbox/projects/BLM_rangeland_trends/data/cover_hist.csv', overwrite = T)

cover_hist <- read_csv('~/Downloads/BLM_allotments.csv') %>% head

# cover <- read_csv('~/Dropbox/projects/BLM_rangeland_trends/data/cover_mean.csv')
# cover_sd <- read_csv('~/Dropbox/projects/BLM_rangeland_trends/data/cover_stdDev.csv')
# cover_hist <- read_csv('~/Dropbox/projects/BLM_rangeland_trends/data/cover_hist.csv')


cover_hist <- 
  cover_hist %>%
  pivot_longer(matches('[0-9]{4}')) %>% 
  separate( col = name, into = c('var', 'year'), sep = '_')  %>% 
  mutate( year = as.numeric(year))  


parse_histogram_list <- function(x){
      eval(parse(text = paste0(
        'list( ',
          str_replace_all(
            x, 
            c('\\n' = '', '\\ '='', '\\{'= '', '\\}'='', '\\[' = 'c(', '\\]' = ')')
          ),')')))
}

test <- cover_hist %>% 
  rowwise() %>% 
  mutate( y = list( parse_histogram_list( value)) ) %>% 
  ungroup() %>%
  filter( row_number() == 1 ) %>% 
  pull(y)

barplot(height = test[[1]]$histogram, names.arg = test[[1]]$bucketMeans)
test[[1]]$bucketMeans

test
cover_hist[1, ]


cover %>% 
  pivot_longer(matches('[0-9]{4}')) %>% 
  separate( col = name, into = c('var', 'year'), sep = '_')  %>% 
  mutate( year = as.numeric(year)) %>% 
  ggplot( aes( x = year, y = value, color = var)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ ALLOT_NA) + 
  scale_x_continuous() + 
  scale_y_continuous('average cover')


cover_sd %>% 
  pivot_longer(matches('[0-9]{4}')) %>% 
  separate( col = name, into = c('var', 'year'), sep = '_')  %>% 
  mutate( year = as.numeric(year)) %>% 
  ggplot( aes( x = year, y = value, color = var, shape = var)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ ALLOT_NA) + 
  scale_x_continuous() + 
  scale_y_continuous('Cover Standard Dev.')


