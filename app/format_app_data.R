# Get data for app 
rm(list= ls())
library(tidyverse)
library(sf)

allotments <- read_csv(file = 'output/export_data/allotment_table.csv')

veg <- read_csv(file = 'output/export_data/annual_veg_data.csv')

shapes <- read_rds(file = 'output/export_data/BLM_allotments_sf.rds')

centers <- shapes %>% 
  select(uname) %>% 
  st_centroid() %>% 
  st_transform(crs = 'epsg:4326')

shapes <- shapes %>% 
  select(uname) %>% 
  st_simplify(preserveTopology = T, 
              dTolerance = 200) %>% 
  st_transform(crs = 'epsg:4326')

allotments <- allotments %>% 
  rename( "Field Office" = admu_name, 
          "Distrinct" = parent_name, 
          "Name" = allot_name, 
          "BLM ID" = allot_no, 
          "State" = admin_st, 
          "Ecoregion" = ecogroup )

allotment_ctrs <- centers %>% left_join(allotments, by = 'uname')
allotment_shps <- shapes %>% left_join(allotments, by = 'uname')

# Settings and Parameters 

##############
##############
table_html <-
  '<table style="width:100%%">
<tr>
<th><span style="float:left"> %s (%i) </span><br/></th>
</tr>
<tr>
<td><span style="float:left"> Type </span><br/></td>
<td><span style="float:right"> Cover </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> AFGC </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> PFGC </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> SHR </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> TREE </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> LTR </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
<tr>
<td><span style="float:left"> BG </span><br/></td>
<td><span style="float:right"> %.2f </span><br/></td>
</tr>
</table>'

veg <- veg %>% 
  filter( type %in% 
            c('AFGC','PFGC', 'BG', 'SHR', 'TREE', 'LTR')) %>% 
  spread( type , value )

last_year <- max(veg$year)
first_year <- min(veg$year)

# Create Choices List for Admin Office 
office_list <- unique( allotment_ctrs$`Field Office`) 
names(office_list) <- office_list
district_list <- unique( allotment_ctrs$Distrinct)
names(district_list) <- district_list

# Map Data 
map_data <- 
  allotment_ctrs %>% 
  # filter( ADM_OFC_CD %in% input$ADM_OFC_CD ) %>% 
  select( uname, Name, Distrinct, `Field Office`, acres ) 

label_df <-
  map_data %>%
  st_drop_geometry() %>% 
  left_join( veg %>% filter( year == last_year ), 
             by = 'uname') 

labels <- sprintf(
  table_html,
  label_df$Name, 
  as.numeric(label_df$year),
  as.numeric(label_df$AFGC), 
  as.numeric(label_df$PFGC), 
  as.numeric(label_df$SHR), 
  as.numeric(label_df$TREE), 
  as.numeric(label_df$LTR), 
  as.numeric(label_df$BG)) %>% 
  lapply(htmltools::HTML)

veg <- 
  veg %>% 
  pivot_longer( cols = c(AFGC, PFGC, BG, LTR, SHR, TREE), 
                names_to = 'type', 
                values_to = 'values' )

save(allotment_shps, veg, labels, map_data, last_year, first_year, file = 'app/data/mapdata.rda')
