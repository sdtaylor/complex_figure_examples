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

map = ggplot() +
  geom_sf(data = countries, fill='transparent', color='grey60', size=0.2) +
  geom_sf(data=map_points, aes(color=continent, shape=plot_type), size=1, stroke=1) + 
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
  ~carbon_metric, ~continent, ~mean_value, ~sd_value, ~text1, ~text2, ~text_y,
  'stocks',      'S America',  120,        50,        'a',    '[a]',   400,
  'stocks',      'Africa',     160,        50,        'b',    '[b]',   400,
  'stocks',      'Asia',       190,        60,        'c',    '[b]',   400,
  'stocks',      'Australia',  200,        50,        'c',    '[ab]',  400,
  'gains',       'S America',  2.1,        1,         'a',    '[a]',   6,
  'gains',       'Africa',     2.5,        1,         'b',    '[a]',   6,
  'gains',       'Asia',       3.5,        0.8,       'c',    '[b]',   6,
  'gains',       'Australia',  2.2,        1,         'ab',   '[a]',   6,
  'time',        'S America',  50,         25,        'a',    '[a]',   200,
  'time',        'Africa',     60,         25,        'b',    '[b]',   200,
  'time',        'Asia',       60,         25,        'b',    '[ab]',  200,
  'time',        'Australia',  100,        30,        'c',    '[ab]',  200
  )

set.seed(1)

# Get some  random values to make the boxplots
barchart_data = barchart_data_starter %>%
  group_by(carbon_metric, continent) %>%
  summarise(carbon_value=rnorm(n=100,mean=mean_value, sd=sd_value)) %>%
  ungroup()

# Make contient a factor with a specific order to order the x-axis
barchart_data$continent = factor(barchart_data$continent, levels = c('S America','Africa','Asia','Australia'), ordered = T)

# The style of all 3 bar plots is exactly the same, with the underlying data the difference. So here
# we use a function to return the same chart, with metric specifying which of the 3 data attributes to use.
get_barchart = function(metric, y_label){
  df_data = barchart_data %>%
    filter(carbon_metric==metric)
  
  df_text = barchart_data_starter %>%
    filter(carbon_metric==metric)
  
  ggplot(df_data, aes(x=continent, y=carbon_value, fill=continent)) + 
    stat_boxplot(geom ='errorbar', linetype='dotted') +  # This is for the 
    geom_boxplot(size=0.3, coef=0) +          # Size here dictates the width of the boxplot outline
    scale_fill_brewer(palette = 'Dark2') + 
    geom_text(data = df_text, aes(y=text_y, label=text1), size=2) +                                                     # The text needs to be done twice since they have different colors.
    geom_text(data = df_text, aes(y=text_y, label=text2), size=2, position = position_nudge(x=0.23), color='blue') +    # Note the 2nd one is nudged slightly to the right, otherwise they would be
    theme_bw(8) +                                                                                                      # drawn on top of eachother. The x-axis placement (continent) is inherited from 
    theme(legend.position = 'none',                                                                                     # the primary ggplot call.
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(size=0.5),
          axis.line.y.left = element_line(size=0.5),
          axis.text.x = element_text(angle=90, hjust=1)) +       # continent labels being rotated is done here. 
    labs(y=y_label, x='')
}

stock_plot = get_barchart(metric='stocks', y_label = 'Carbon Stocks (Mg C ha-1)')
gains_plot = get_barchart(metric='gains', y_label = 'Carbon Gains (Mg C ha-1 yr-1)')
time_plot = get_barchart(metric='time', y_label = 'Carbon residence time (years)')

box_plots = plot_grid(stock_plot, gains_plot, time_plot, nrow = 1)


final_figure = plot_grid(map, box_plots, ncol=1, rel_heights = c(0.7,1))

save_plot('sullivan2020/sullivan2020_final.png', plot = final_figure)

