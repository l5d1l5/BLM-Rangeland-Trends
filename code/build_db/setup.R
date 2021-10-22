# setup 

rm(list = ls() )
library(tidyverse)
library(sf)
library(lubridate)

# Download source data 

# Check for data 
stopifnot( file.exists('data/spatial/BLM_National_Grazing_Allotments/gra.gdb/gdb'))
stopifnot( file.exists('data/spatial/BLM_National_Administrative_Units/admu.gdb/gdb'))
stopifnot( file.exists('data/spatial/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp'))

source('code/build_db/1_clean_allotment_shapes.R')
source('code/build_db/2_join_BLM_districts.R')

stopifnot(file.exists('data/temp/BLM_allotments_cleaned/allotments.shp')) # import into GEE
# complete ownership analysis in GEE: 
# https://code.earthengine.google.com/1468504e7449193cdd198b2f3c9564f5
readline(prompt="Complete ownership analysis in GEE. Upload cleaned allotment shapes to GEE and then run ownership script. \n
          'https://code.earthengine.google.com/1468504e7449193cdd198b2f3c9564f5'
         \n When finished, press [enter] to proceed with setup")

stopifnot(file.exists('data/RAP_EE_exports/ownership_area_by_allotment.csv'))

source('code/build_db/3_allotment_info.R')
source('code/build_db/4_join_ecoregions.R') 
# complete RAP data extraction by allotment in GEE: 
# export climate: https://code.earthengine.google.com/75ac58530a476272231cf285227eaf13
# export cover: https://code.earthengine.google.com/7559d77e44b408184e03df154472a4d8
# export production: https://code.earthengine.google.com/ef15186654ea61125a9d91fd6830620f

readline(prompt="Complete allotment-level data extraction in GEE. 
Upload cleaned allotment shapes to GEE and then run annual data script. \n
  When finished, press [enter] to proceed with setup")

stopifnot( file.exists('data/RAP_EE_exports/allotment_cover_by_year.csv'))
stopifnot( file.exists('data/RAP_EE_exports/allotment_production_by_year.csv'))
stopifnot( file.exists('data/RAP_EE_exports/allotment_elevation.csv'))

source('code/build_db/5_process_annual_data.R')
