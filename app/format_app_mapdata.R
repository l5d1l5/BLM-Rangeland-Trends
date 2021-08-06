# Get data for app 
rm(list= ls())
library(tidyverse)
library(sf)

source('app/parameters.R')
load('app/data/trenddata.rda')
allotments <- read_csv(file = 'output/export_data/allotment_table.csv')
shapes <- read_rds(file = 'output/export_data/BLM_allotments_sf.rds')

centers <- shapes %>% 
  select(uname) %>% 
  st_centroid() %>% 
  st_simplify(preserveTopology = T, dTolerance = 200) %>% 
  st_transform(crs = 'epsg:4326') 

shapes <- shapes %>% 
  select(uname) %>% 
  st_simplify(preserveTopology = T, 
              dTolerance = 200) %>% 
  st_transform(crs = 'epsg:4326')

allotments <- allotments %>% 
  rename( "Field Office" = admu_name, 
          "District" = parent_name, 
          "Name" = allot_name, 
          "BLM ID" = allot_no, 
          "State" = admin_st, 
          "Ecoregion" = ecogroup ) %>% 
    mutate( `Field Office` = str_remove(string = `Field Office`, 
                                            pattern = " FIELD OFFICE")) %>% 
    mutate( District = str_remove( District,  pattern = " DISTRICT OFFICE")) %>%
    mutate( `Field Office` = str_to_title(`Field Office`), 
            District = str_to_title( District ), 
            Ecoregion = str_to_title(Ecoregion)) %>% 
  left_join(all_trends , by = 'uname')

allotment_ctrs <- centers %>% left_join(allotments, by = 'uname')
allotment_shps <- shapes %>% left_join(allotments, by = 'uname')

##############
# Map Data 
label_df <-
  allotment_ctrs %>%
  st_drop_geometry() 

labels <- sprintf(
  table_html,
  label_df$Name, 
  label_df$`BLM ID`, 
  as.character(label_df$District), 
  as.character(label_df$`Field Office`), 
  as.character(label_df$Ecoregion), 
  as.numeric(label_df$acres)) %>% 
  lapply(htmltools::HTML)


# allotment_shps <- allotment_shps %>% st_cast('POLYGON') 
# allotment_ctrs <- allotment_ctrs %>% st_cast('POINT') 
# 
# allotment_shps <- allotment_shps %>% head(1000)
# allotment_ctrs <- allotment_ctrs %>% head(1000)
# 
# labels <- labels[1:1000]

save(allotment_ctrs, allotment_shps, labels,
     file = 'app/data/mapdata.rda')
