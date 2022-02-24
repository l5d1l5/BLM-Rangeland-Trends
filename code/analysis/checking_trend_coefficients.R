rm(list = ls())
library(sf)
library(tidyverse)
library(lme4)

load('app/data/vegdata.rda')
load( 'app/data/trenddata.rda')
load( 'app/data/mapdata.rda')

mMod <- read_rds('output/AFG_cover_trend_model.rds')

rEff <- ranef(mMod)
fEff <- fixef(mMod)
mData <- mMod@frame

rEff$uname

mData$value2
mData$year2

test_cases <- veg %>% 
  filter( type == 'AFGC') %>% 
  left_join(allotment_shps %>% st_drop_geometry()) %>% 
  filter( State == "CO") %>% 
  filter( `Field Office` == 'Little Snake') %>%
  filter( Name %in% c('CEDAR SPRINGS DRAW', 
                      'W WAPITI PEAK', 
                      'GROUNDS', 'N DECEPTION CREEK'))  
  
test_cases %>% 
  filter( year > 1990) %>% 
  ggplot( aes( x = year, y = value, color = Name, group = Name)) +
  geom_line() + 
  geom_smooth(se = F, method = 'lm')


mData %>% 
  filter( uname %in% test_cases$uname) %>% 
  left_join( test_cases %>% distinct(uname, Name)) %>% 
  ggplot( aes( x = year2, y = value2, color = Name, group = Name)) +
  geom_line() + 
  geom_smooth(se = F, method = 'lm')

all_trends %>% 
  filter( uname == 2125)

veg %>% 
  filter( year > 1989) %>%
  filter( uname == 2125) %>% 
  filter( type == 'AFGC') %>%
  ggplot( aes( x = year, y= value )) + 
  geom_line() + 
  geom_smooth(se = F, method = 'lm')


test_fo <- allotment_shps %>%
  st_drop_geometry()

veg_trends <- veg %>% 
  filter( year > 1990 ) %>%
  filter( type == 'AFGC') %>%
  mutate( value == log(value)) %>% 
  filter( is.finite(value)) %>% 
  left_join( test_fo %>%
      select(uname, Name))  %>%
  group_by( uname , Name) %>% 
  summarise( m =  list( lm( value ~ year ))) 

coefs <- data.frame( do.call( rbind, lapply( veg_trends$m, coefficients ) ) )
names( coefs ) <- c('intercept', 'trend')


mismatches <- cbind( veg_trends, coefs)  %>% 
  left_join( allotment_shps ) %>%
    select( Annual_cover, uname, Name, `Field Office`, Ecoregion, trend ) %>% 
    mutate( misMatch = as.numeric( sign( trend) != sign( Annual_cover) ))

mismatches %>% 
  group_by( `Field Office` ) %>%
  summarise( mismatch_fraction = mean(misMatch, na.rm = T)) %>%
  view 
