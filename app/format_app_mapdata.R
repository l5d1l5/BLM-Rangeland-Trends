# Get data for app 
rm(list= ls())
library(tidyverse)
library(sf)

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
            Ecoregion = str_to_title(Ecoregion))


allotment_ctrs <- centers %>% left_join(allotments, by = 'uname')
allotment_shps <- shapes %>% left_join(allotments, by = 'uname')

##############
##############
table_html <-
  '<table style="width:100%%">
<tr>
<th><span style="float:left"> %s </span><br/></th>
</tr>
<tr>
<td><span style="float:left"> BLM ID </span><br/></td>
<td><span style="float:right"> %s </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> District </span><br/></td>
<td><span style="float:right"> %s </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> Field Office </span><br/></td>
<td><span style="float:right"> %s </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> Ecoregion </span><br/></td>
<td><span style="float:right"> %s </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> Acres </span><br/></td>
<td><span style="float:right"> %.0f </span><br/></td>
</tr>
</table>'

# veg <- veg %>% 
#   filter( type %in% 
#             c('AFGC','PFGC', 'BG', 'SHR', 'TREE', 'LTR')) %>% 
#   spread( type , value )
# 

# Map Data 
map_data <- 
  allotment_ctrs %>% 
  # filter( ADM_OFC_CD %in% input$ADM_OFC_CD ) %>% 
  select( uname, Name, `BLM ID`,  District, `Field Office`, Ecoregion, acres ) 

label_df <-
  map_data %>%
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

save(allotment_shps, labels, map_data,
     file = 'app/data/mapdata.rda')
