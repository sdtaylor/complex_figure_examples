library(tidyverse)
library(sf)
library(rnaturalearth)
library(cowplot)

#############################################

# These points are randomly placed in the same vicinity of the original map. Done using http://geojson.io
map_points = st_read('sullivan2020/map_points.geojson')
# Randomly assign census type to have open and closed points
map_points$plot_type = sample(c('multicensus','single_census'), size=nrow(map_points), replace = T)

# Make contient a factor with a specific order to order, the same as the barchart below, so the colors get assigned the same
map_points$continent = factor(map_points$continent, levels = c('S America','Africa','Asia','Australia'), ordered = T)

countries <- ne_countries(returnclass = "sf")

ggplot() +
  geom_sf(data = countries, fill='transparent', color='grey60', size=0.2) +
  geom_sf(data=map_points, aes(color=continent, shape=plot_type), size=3, stroke=1) + 
  coord_sf(xlim = c(-80,140), ylim = c(-20,20)) +
  scale_color_brewer(palette = 'Dark2') + 
  scale_shape_manual(values = c(1,16)) + 
  theme_minimal() +
  theme(panel.background = element_rect(color='black'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none') 

##############################################

barchart_data_starter = tribble(
  ~carbon_metric, ~continent, ~mean_value, ~sig_text1, ~sig_text2,
  'stocks',      'S America',  120,         'a',        '[a'
  'stocks',      'Africa',     160,
  'stocks',      'Asia',       190,
  'stocks',      'Australia',  200,
  'gains',       'S America',  2.1,
  'gains',       'Africa',     2.5,
  'gains',       'Asia',       3.5,
  'gains',       'Australia',  2.2,
  'time',        'S America',  50,
  'time',        'Africa',     60,
  'time',        'Asia',       60,
  'time',        'Australia',  100)

# Get some  random values to make the boxplots
barchart_data = barchart_data_starter %>%
  group_by(carbon_metric, continent) %>%
  summarise(carbon_value=rnorm(rnorm(n=100,mean=mean_value))) %>%
  ungroup()

# Make contient a factor with a specific order to order the x-axis
barchart_data$continent = factor(barchart_data$continent, levels = c('S America','Africa','Asia','Australia'), ordered = T)

#get_barchart = function(df, y_label){
  
  
  ggplot(df, aes(x=continent, y=carbon_value, fill=continent)) + 
    geom_boxplot() +
    geom_text()
    scale_fill_brewer(palette = 'Dark2') + 
    theme_bw() + 
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(size=0.5),
          axis.line.y.left = element_line(size=0.5),
          axis.text.x = element_text(angle=90, hjust=1)) +
    labs(y=y_label, x='')
#}

stock_plot = get_barchart(filter(barchart_data, carbon_metric=='stocks'), y_label = 'Carbon Stocks Mg C ha-1')

draw_t