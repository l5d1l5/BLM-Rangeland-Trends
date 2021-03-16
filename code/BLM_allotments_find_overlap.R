rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)
library(parallel)

m2_per_ACRE <- 4046.8564224

find_intersection_area <- function(x, y, m2_per_ACRE) {
  # Function for finding intersecting area of two shapes
  O <- try(expr = st_intersection(x, y), silent = T)
  if (class(O)[1] == 'try-error') {
    x <- x %>% st_make_valid()
    y <- y %>% st_make_valid()
    O <- st_intersection(x, y)
  }
  A_o <- st_area(O) %>% as.numeric / m2_per_ACRE
  
  return(A_o)
}

# Pull allotment geometries and find intersections
BLM <-
  readRDS('data/BLM_sf.rds') %>%
  #filter( ADMIN_ST = 'MT') %>%
  st_sf() %>%
  st_transform(5070) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>% 
  st_simplify(preserveTopology = T, dTolerance = 30) %>% 
  st_make_valid()

BLM_u <-
  BLM %>%
  group_by(uname, LAST_DATE) %>%
  summarise(SHAPE = SHAPE %>%
              st_union %>%
              st_make_valid) %>%
  ungroup() %>%
  mutate(area = st_area(SHAPE) %>% as.numeric) %>%
  mutate(acres = area / m2_per_ACRE) %>%
  mutate(rid = row_number())

overlaps <-
  BLM_u %>%
  st_join(BLM_u, join = st_overlaps, left = T) %>%
  rowwise() %>%
  mutate(pair_id = paste(sort(c(uname.x, uname.y)), collapse = '-')) %>%
  filter(!is.na(rid.y)) %>%
  ungroup() %>%
  select(
    pair_id,
    uname.x,
    uname.y,
    LAST_DATE.x,
    LAST_DATE.y,
    rid.x,
    rid.y,
    area.x,
    area.y,
    acres.x,
    acres.y,
    SHAPE
  ) %>%
  arrange(pair_id)

overlaps2 <-
  BLM_u[overlaps$rid.y,]  # order second shape by intersection with first

overlaps$overlap_acres <-
  mcmapply(overlaps$SHAPE,
           overlaps2$SHAPE,
           FUN = find_intersection_area,
           m2_per_ACRE = m2_per_ACRE) # calculate acreage of overlap

overlaps$prop_overlap <- overlaps$overlap_acres / overlaps$acres.x

overlaps <-
  overlaps %>%
  group_by(pair_id) %>%
  mutate(n = n()) %>%
  mutate(max_overlap = max(prop_overlap)) %>%
  mutate(min_overlap = min(prop_overlap)) %>%
  arrange(desc(max_overlap), pair_id) %>%
  mutate(drop = F, drop.y = F) %>%
  mutate(drop = ifelse(# if overlap is > 0.9 for both then drop oldest
    (
      min_overlap > 0.8 & LAST_DATE.y > LAST_DATE.x
    ), T, drop)) %>%
  ungroup() %>%
  mutate(drop.y = ifelse(rid.y %in% unique(rid.x[drop]), T, drop.y)) %>%
  group_by(pair_id) %>%
  mutate(drop =  # if y has not been dropped then drop x where area_x < area_y
           ifelse(
             (max_overlap > 0.9 & !drop.y &
                acres.x < acres.y),
             yes = T,
             no = drop
           )) %>%
  select(pair_id, drop, drop.y , uname.x:min_overlap)

drop_index <-
  overlaps %>%
  st_drop_geometry() %>%
  group_by(rid.x) %>%
  summarise(
    drop = max(drop) ,
    drop_concensus = sum(drop),
    n_overlaps = n()
  )  %>%
  arrange(desc(n_overlaps), desc(drop_concensus)) %>%
  ungroup()

allot_names <-
  BLM %>%
  st_drop_geometry() %>%
  distinct(uname, ADMIN_ST, ADM_OFC_CD, ALLOT_NO, ALLOT_NAME)

BLM_out <-
  BLM_u  %>%
  left_join(drop_index, by = c('rid' = 'rid.x')) %>%
  mutate(drop = replace_na(drop, 0),
         n_overlaps = replace_na(n_overlaps, 0)) %>%
  ungroup() %>%
  left_join(allot_names, by = 'uname')

BLM_clean <- 
  BLM_out  %>% 
  filter( ! drop ) %>%  # filter out dropped polygons and regroup by uName
  st_cast('MULTIPOLYGON') %>%
  group_by( uname, ALLOT_NAME, ADMIN_ST, ADM_OFC_CD ) %>%
  summarise( SHAPE = st_union(SHAPE)) %>% 
  st_make_valid()  %>% 
  ungroup() %>%
  mutate( acres = st_area(SHAPE)/m2_per_ACRE)

unlink('output/BLM_allotments_dropped', recursive = T)
dir.create('output/BLM_allotments_dropped')

BLM_out %>%
  select(
    uname,
    rid,
    drop,
    ADMIN_ST,
    ADM_OFC_CD,
    ALLOT_NO,
    ALLOT_NAME,
    LAST_DATE,
    acres,
    n_overlaps
  ) %>%
  st_cast('MULTIPOLYGON') %>%
  st_make_valid() %>%
  st_write('output/BLM_allotments_dropped/allotments_dropped.shp',
           append = F)


# Save as sf object RDS 
BLM_clean %>%
  select(
    uname,
    ADMIN_ST,
    ADM_OFC_CD,
    ALLOT_NAME,
    acres
  ) %>%
  saveRDS(file = 'output/BLM_cleaned_shape_sf.RDS')
