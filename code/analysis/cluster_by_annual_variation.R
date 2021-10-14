load('data/analysis_data/cover.rda')
attach(cover)
load('data/analysis_data/allotments.rda')

sel_uname <- 
  AFGC %>%
  filter( ecogroup != 'Costal Forests') %>% 
  group_by( uname) %>% 
  filter( min(value) > 0.25 ) %>% 
  ungroup() %>% 
  group_by(office_label) %>% 
  sample_n(100, replace =T )   %>% 
  distinct(uname) %>% 
  pull(uname)

sample_data <- AFGC %>% filter( uname %in% sel_uname) %>% 
  group_by( uname ) %>% 
  select( uname, year, value2 ) %>% 
  pivot_wider( names_from = uname, values_from = value2 ) 

year <- sample_data$year
sel_uname <- names(sample_data)[-1]

uname_labels <- AFGC %>% 
  filter( uname %in% sel_uname ) %>% 
  distinct( uname, climate_region, ecogroup )

cormat <- sample_data %>% 
  select( -year ) %>% 
  as.data.frame() %>% 
  cor( use = 'pairwise.complete.obs')

dist <- as.dist(1 - cormat)
tree <- hclust(dist, method="complete")

library(dendextend)

dendrogram <- as.dendrogram(tree)

nleaves(dendrogram)
nnodes(dendrogram)

plot(branches_color(dendrogram, k = 6), leaflab = 'none')

clusters <- cutree( dendrogram, k = 6)
clusters <- as.data.frame( clusters  ) 
clusters$uname <- as.numeric( row.names(clusters) )


allotments %>% 
  left_join(clusters, by = 'uname') %>% 
  filter( !is.na(clusters)) %>% 
  ggplot(aes( x = climate_region, fill = climate_region)) + 
  geom_bar() + 
  facet_wrap( ~ clusters )

library(cluster)
kmed <- pam(dist, k = 6)

kmed <- data.frame( kmed = kmed$clustering  ) 
kmed$uname <- as.numeric( row.names(kmed) )


allotments %>% 
  left_join(kmed, by = 'uname') %>% 
  filter( !is.na(kmed)) %>% 
  ggplot(aes( x = climate_region, fill = climate_region)) + 
  geom_bar() + 
  facet_wrap( ~ kmed )

allotment_shps <- read_rds('data/temp/BLM_allotments_sf.rds' )

library(sf)
allotment_pts <- allotment_shps %>% 
  left_join(kmed, by = 'uname') %>% 
  left_join(clusters , by = 'uname') %>% 
  filter( !is.na(kmed) , !is.na(clusters )) %>% 
  mutate( POINT = st_centroid(SHAPE)) %>% 
  st_drop_geometry()  %>% 
  mutate( X  = st_coordinates(POINT)[, 1], Y = st_coordinates(POINT)[, 2]) %>% 
  select( - POINT)

allotment_pts <- st_as_sf(allotment_pts, coords = c('X', 'Y'), crs = st_crs(allotment_shps)) 
allotment_pts <- allotment_pts %>% st_transform(crs = st_crs(allotment_shps))

pal <- RColorBrewer::brewer.pal(6, 'Accent')

allotment_pts %>% 
  mutate( clusters = factor(clusters)) %>%
  ggplot( ) + 
  geom_sf(aes( color = clusters ))  
