rm(list = ls()) 
load('data/analysis_data/allotments.rda')

allotment_geo <- read_rds('output/export_data/BLM_allotments_sf.rds')

x_y <-
  allotment_geo %>% 
  sf::st_centroid() %>%
  distinct(uname)  %>% 
  sf::st_coordinates() %>% 
  data.frame() 

centroids <- kmeans_res <- list() 

for( i in 1:10 ) { 
  kmeans_res[[i]] <- kmeans(x_y, i , iter.max = 100, nstart = 200)
  centroids[[i]] <- data.frame(uname = allotment_centroids$uname, cluster = kmeans_res[[i]]$cluster )
}


allotment_geo %>% 
  sf::st_centroid( ) %>% 
  left_join(centroids[[10]], by = 'uname') %>% 
  mutate( cluster = factor(cluster))  %>% 
  ggplot(aes( color = cluster) ) + 
  geom_sf(alpha = 0.5)

# Try another approach 

d <- dist( x_y )
hc <- hclust(d)

groups <- cutree(hc, k = 20)

allotment_geo %>% 
  sf::st_centroid() %>%
  mutate( group = factor( groups )) %>% 
  ggplot() + 
  geom_sf( aes( color = group))

allotment_geo %>% 
  sf::st_centroid() %>%
  mutate( group = factor( groups )) %>% 
  ggplot() + 
  geom_sf( aes( color = ADM_OFC_CD))

kmeans_res[[1]]$tot.withinss
kmeans_res[[2]]$tot.withinss
kmeans_res[[10]]$tot.withinss

tot.withinss <- unlist( lapply( kmeans_res, function(x) x$tot.withinss )  )
plot(tot.withinss, type = 'l')







allotment_info <- read_csv('output/export_data/allotment_info.csv')


allotment_info <- allotment_info %>% left_join(allotments)  %>% 
  distinct()

allotment_info %>% 
  ggplot( aes( x = lon, y = lat)) + 
  geom_point()

allotment_info %>% 