# App Parameters 

##############
##############
my_colors <- c(
  'Annual' = '#b2df8a',
  'Bare Ground'  = '#A9A9A9',
  'Perennial' = '#1f78b4',
  'Shrub' = '#fb9a99',
  'Tree' = '#33a02c'
)

my_colors_rgba <- colorspace::hex2RGB(my_colors)

my_colors_rgba <- apply( X = my_colors_rgba@coords , 1, 
                         FUN = function( x ){ paste0('rgba(', 
                                                     paste0(c(x, 0.5), collapse = ','), 
                                                     ')')})

main_title <- "Rangeland Vegetation on BLM Allotments"
map_title <- "BLM Grazing Allotments"
plotly_title <- "Selected allotment (click on map)"

hoverformat <- c(cover = "%{text}: %{y:.0f}%", 
                 production = "%{text}: %{y:0f} kg/ha")

x_title <- "Year"
y_title <- c( cover = "Cover (%)", production = 'Production (kg/ha)')

default_name <- ''

RAP_link <-
  "https://rangelands.app/"

latest_update_link <-
  "https://rangelands.app/"

about <-
  '<!DOCTYPE html>
<b>ABOUT:</b>
<br></br>
<body style="width:70%%"><P>
Grassland cover in BLM grazing allotments. Click on a grazing allotment on the 
map to see an annual timeseries of cover in that grazing allotment. Select one of 
the plant cover categories above.  Cover data is derived from the <a href = "https://rangelands.app/">Rangeland Analysis Platform</a>.
<P>
<td>
<center>#####</center><br>
</td>
</body>
</html>'

scale_labs = c('Ecoregion' = 'Ecoregion', 'District' = 'District', 
               `Field Office` = 'Field_Office')

center_ll <- c(-114.07, 42.04) 

bounds <- c( -125.0, 27.9, 
             94.8, 49.1) 

# Map hover table template 
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


