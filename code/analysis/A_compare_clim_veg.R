
fls <- dir('output', pattern = '_group_trends.csv', full.names = T)
type <- str_extract( basename(fls), '^[A-Za-z]+')
unit <- str_extract( basename(fls), 'cover|agb')

grp_trends <- 
  mapply( x = fls, y = type, z = unit , 
          FUN =  function( x, y, z ){ 
            read_csv( x) %>% 
              mutate( type = y, unit = z )
            }, SIMPLIFY = F)

trend_compare <- 
  grp_trends %>% 
  bind_rows() %>% 
  select( uname, ecogroup, full_trend, type, unit ) %>% 
  unite( col = 'var',  c(type:unit)) %>% 
  pivot_wider(id_cols = c(uname, ecogroup), names_from = var, values_from = full_trend ) 

# tavg 

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = tavg_NA, y = AFG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup )

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = tavg_NA, y = BG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup )

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = tavg_NA, y = PFG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup )


trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = tavg_NA, y = TREE_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup )


trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = tavg_NA, y = SHR_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup )

# pr 
trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pr_NA, y = AFG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup , scales =  'free')

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pr_NA, y = BG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup , scales = 'free')

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pr_NA, y = PFG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup , scales = 'free')

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pr_NA, y = TREE_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup, scales = 'free' )


trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pr_NA, y = SHR_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup , scales = 'free')


# pdsi
trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pdsi_NA, y = AFG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup , scales =  'free')

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pdsi_NA, y = BG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup , scales = 'free')

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pdsi_NA, y = PFG_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup , scales = 'free')

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pdsi_NA, y = TREE_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup, scales = 'free' )

trend_compare %>% 
  filter( !is.na(ecogroup)) %>%
  ggplot( aes( x = pdsi_NA, y = SHR_cover)) + 
  geom_point(aes( color = ecogroup)) + 
  scale_color_manual(values = ecogroup_colors) + 
  facet_wrap( ~ ecogroup , scales = 'free')


# 
veg_trends <- trend_compare %>% 
  filter( !is.na(ecogroup ))   %>% 
  select( uname, AFG_cover, BG_cover, PFG_cover, SHR_cover)  %>% 
  arrange( uname) %>% 
  filter( complete.cases(.))  
  
clim_trends <- trend_compare %>% 
  filter( !is.na(ecogroup )) %>% 
  select( uname, tavg_NA, pr_NA) %>% 
  filter( uname %in% unique(veg_trends$uname)) %>%
  arrange(uname) %>% 
  filter( complete.cases(.))

veg_pca <- veg_trends %>% 
  select( - uname ) %>% 
  princomp()

clim_pca <- clim_trends %>% 
  select( - uname ) %>% 
  princomp()

efit <- vegan::envfit(veg_pca, clim_trends %>% select( -uname))
biplot(veg_pca)
plot(efit)
