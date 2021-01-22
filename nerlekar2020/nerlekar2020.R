library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)
library(cowplot)       # note cowplot is only used to attach the watermark at the end.


#--------------
# This is a replication of Figure 1 from the following paper
# Nerlekar, A.N. and Veldman, J.W., 2020. High plant diversity and slow assembly 
# of old-growth grasslands. Proceedings of the National Academy of Sciences, 117(31), pp.18550-18556. 
# https://doi.org/10.1073/pnas.1922266117

# This figure uses simulated data and is not an exact copy. 
# It's meant for educational purposes only.  
#--------------

# The data is a csv with lat,lon, map (mean annual precip), mat (mean annual temp), and the continent. 
# This converts it to a spatial object so it can be mapped with the sf package.
point_data = read_csv('nerlekar2020/point_data.csv') %>%
  st_as_sf(coords = c('lon','lat'), crs=4326)

# Make labels for the legend which include the sample size.
point_data = point_data %>%
  group_by(continent) %>%
  mutate(continent_label = paste0(continent, ' (n=',n(),')')) %>%
  ungroup()

# Note this palette is not color-blind friendly.
#                     africa,  asia,   aus,     europe,    n amer,  s amer
continent_colors = c('salmon','black','#d55e00','#e6ab02','#56b4e9','#7570b3')

# To get the affect of only land outlines as simply as possible we'll combine
# two free datasets. The country and coastlines from Natural Earth. https://www.naturalearthdata.com/
# vai the rnaturalearth package.
# The coastline data is a *line* object, therefore it's impossible to give it a fill
# color, for that we need a *polygon* object, so the country one will work. 
countries = ne_countries(scale=110, returnclass = 'sf') 
coastlines = ne_coastline(returnclass = 'sf')

# The country polygon object will have the grey fill, but the lines of each country will be set to transparent.
# the coastline line object will be black to create the desired affect. 
# Try commenting out to the 2 different lines or changing the fill/color values to see what happens. 

world_map_panel = ggplot() + 
  geom_sf(data=countries, fill='grey90', color='transparent') + 
  geom_sf(data=coastlines, color='black', size=0.25) +
  geom_sf(data=point_data, aes(color=continent_label),size=3) +
  scale_color_manual(values=continent_colors) + 
  coord_sf(ylim = c(-50, 80)) +                                     # Zoom in a bit to exclude antarctica
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(color='black', size=10),
        legend.key = element_blank(),                               # this element turns off the background of the points inside the legend.
        legend.title = element_blank(),
        legend.position = c(0.1,0.2))


mat_map_panel = ggplot(point_data, aes(x=map, y=mat, color=continent_label)) + 
  geom_point(size=4)  +
  scale_color_manual(values=continent_colors) + 
  scale_x_continuous(breaks = seq(0,2500,500)) + 
  scale_y_continuous(breaks = seq(0,25,5)) + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),               # Turn off the full border but initialize the bottom and left lines
        axis.line.x.bottom = element_line(size=0.5),
        axis.line.y.left   = element_line(size=0.5),
        axis.text = element_text(size=12, face='bold', color='black'),
        axis.title = element_text(size=13, face='bold'),
        legend.position = 'none') +
  labs(x='Mean annual precipitation (mm)', y='Mean annual temperature (°C)')

# Combining plots here is done using the patchwork library.
# See https://patchwork.data-imaginist.com/articles/guides/layout.html
final_figure = world_map_panel + mat_map_panel + 
  plot_layout(nrow = 1, widths = c(3,1.5)) +
  plot_annotation(tag_levels = 'A') &                                # This & is special for the patchwork library and applies the theme to all plots. 
  theme(plot.tag = element_text(size = 16, face = 'bold'))           # See https://patchwork.data-imaginist.com/articles/guides/annotation.html for detail


#--------------
water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.05), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.6,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
  draw_text(water_mark, x=0.05, y=0.1, size=10, hjust = 0)

save_plot('./nerlekar2020/nerlekar2020_final.png', plot=final_figure, base_height = 4, base_width = 12)
