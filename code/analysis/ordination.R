rm(list = ls())
library(tidyverse)
library(dbplyr)
require( RPostgres  )
library(sf)
library(lme4)
library(emmeans)

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

decadal_data <- 
  tbl(con, 'annual_data') %>% 
  filter( year  > 1990 ) %>%
  left_join(allotments, by = 'uname') %>% 
  filter( ecogroup != "Coastal Forests") %>% 
  ungroup() %>% 
  mutate( decade = as.integer( floor( year/10)*10 )) %>%
  group_by( uname, type, decade ) %>%
  summarise( avg = mean(value, na.rm = T ), 
             nyears = count(!is.na(value)) ) %>% 
  mutate( decgroup = paste0( as.character( decade), 's' ))  %>%
  left_join( allotments, by = 'uname')


sample_allots <- decadal_data %>% 
  filter( type %in% c('AFGC', 'PFGC', 'SHR', 'TREE' , 'BG')) %>% 
  ungroup() %>%
  pivot_wider(names_from = type, values_from = avg) %>%
  group_by( ecogroup, decgroup ) %>%
  slice_sample(n = 50) %>% 
  collect()


sample_allots <- sample_allots %>% 
  select( uname, decgroup, district_label, office_label, ecogroup , AFGC:BG) %>% 
  arrange( ecogroup, decgroup, uname ) 

library(vegan)

comdat <- sample_allots %>% ungroup() %>% select( AFGC:BG)
princomp(comdat)
ord <- metaMDS(comm = comdat, k = 2, trymax = 100)

env <- sample_allots[, c('decgroup' , 'ecogroup')]
spp <- sample_allots[, c('AFGC', 'TREE', 'PFGC', 'SHR', 'BG')]

envfit <- envfit(ord, env, permutations = 999) # this fits environmental vectors
spp.fit <- envfit(ord, spp , permutations = 999) # this fits species vectors

site.scrs <- as.data.frame(scores(ord, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, env)
site.scrs

spp.scrs <- as.data.frame(scores(spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

env.scores <- as.data.frame(scores(envfit, display = "factors")) #extracts relevant scores from envifit
env.scores <- cbind(env.scores, env.variables = rownames(env.scores)) #and then gives them their names

site.scrs <- site.scrs %>%
  filter( decgroup != '2020s') %>%
  mutate( ecogroup = ifelse(ecogroup == 'NW Forested Mts', 'Forested Mts', ecogroup)) %>%
  mutate( decgroup = factor( decgroup, levels = c('1990s', '2000s', '2010s'), ordered = T))
  
decade_averages <- site.scrs %>%
  group_by( ecogroup , decgroup ) %>%
  summarise( NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2)) %>% 
  pivot_longer(c(NMDS1, NMDS2), names_to = 'axis', values_to = 'vals') %>% 
  unite('group',  c('axis', 'decgroup') ) %>% 
  pivot_wider(names_from = group, values_from = vals)


site.scrs %>%
  ggplot( aes( x = NMDS1, y= NMDS2 , color = ecogroup)) + 
  geom_point(alpha = 0.2) + 
  geom_segment(data = decade_averages , aes( x = NMDS1_1990s, xend = NMDS1_2000s, 
                    y = NMDS2_1990s, yend = NMDS2_2000s), 
               show.legend = F) + 
  geom_segment(data = decade_averages, aes( x = NMDS1_2000s, xend = NMDS1_2010s, 
                    y = NMDS2_2000s, yend = NMDS2_2010s), arrow = arrow(), 
               show.legend = F) + 
  scale_color_manual(values = ecogroup_colors) + 
  theme_bw() + 
  geom_segment(data = sig.spp.scrs, 
               aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), 
               colour = "grey10", lwd=0.3) + 
  ggrepel::geom_text_repel(data = sig.spp.scrs, 
                           aes(x=NMDS1, y=NMDS2, label = Species), color = 1, 
                           cex = 3, direction = "both", segment.size = 0.25)

nmds.plot.dune <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$ecogroup), shape = factor(site.scrs$decgroup)), size = 2) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Ecogroup", shape = "Decade") +
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10))

nmds.plot.dune
nmds.plot.dune + labs(title = "Basic ordination plot") #displays plot