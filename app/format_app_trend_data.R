# Load model trends 
source('code/analysis/parameters.R')


trend_files <- dir('output', pattern = 'trends.csv$', full.names = T)
type <- str_extract( basename(trend_files), pattern = '[A-Z]+')
unit <- str_extract( basename( trend_files), pattern = 'cover|agb')

trends <- lapply(trend_files, read_csv)
type_unit <- paste( type, unit , sep = '_')
trends <- mapply( type_unit, trends, FUN = function(x, y){ data.frame( y, type_unit = x) }, SIMPLIFY = F )

all_trends <- 
  do.call(rbind, trends) %>%
  separate(col = type_unit, into = c('type', 'unit'), sep = '_') %>%
  mutate( type = factor( type, labels = unique(type_labels[type]))) %>% 
  mutate( unit = factor( unit, labels = unique(unit_labels[unit]))) %>% 
  unite(col = 'type_unit', c(type, unit ))

all_trends <- all_trends %>%
  select( uname, full_trend, type_unit ) %>%
  pivot_wider(id_cols = uname, 
              names_from = type_unit, 
              values_from = full_trend)

save(all_trends, file = 'app/data/trenddata.rda')

