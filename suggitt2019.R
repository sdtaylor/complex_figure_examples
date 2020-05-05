library(tidyverse)
library(sf)

# Kopen polygons from http://koeppen-geiger.vu-wien.ac.at/present.htm
kopen_climate_regions = sf::read_sf('./suggitt2019/data/Koppen_Geiger Edited and Completed/Shapefiles/world_climates_completed_koppen_geiger.shp')

# Match up the kopen classifications with the regions defined in the paper.
suggitt_regions = tribble(
  ~kopen_group, ~region, ~fill_color,
  'B', 'Arid', 'yellow',
  'A', 'Equatorial', 'green4',
  'C', 'Warm temperate', 'purple4',
  'D', 'Cold', 'grey80',
  'E', 'Polar', 'cyan2'
)
kopen_climate_regions = kopen_climate_regions %>%
  mutate(kopen_group = stringr::str_sub(climates_f,1,1)) %>%
  left_join(suggitt_regions, by='kopen_group')

# simplify polygon lines for quicker rendering
kopen_climate_regions = kopen_climate_regions %>%
  st_simplify(dTolerance = 0.25)

# Make 1 polygon entry per region. 
# See https://stackoverflow.com/questions/57625471/create-new-geometry-on-grouped-column-in-r-sf
kopen_climate_regions = kopen_climate_regions %>%
  group_by(region) %>% 
  summarise() %>%
  ungroup() %>%
  filter(!is.na(region))

# Convert the region column to a factor with the order of the suggitt_regions data.frame
# This allows the colors to line up with the scale_fill_manual line below.
# Note this also defines the ordering of the map legend.
kopen_climate_regions$region = factor(kopen_climate_regions$region, levels = suggitt_regions$region, ordered = T)

# These are the sites from Vellend et al. 2016 supp. info
# https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/ecy.1660
map_points = read_csv('suggitt2019/data/Vellend_data_updated.csv') %>%
  st_as_sf(coords = c('Longitude','Latitude'), crs=4326)

plot_A_map = ggplot() + 
  geom_sf(data=kopen_climate_regions, aes(fill=region), color='transparent') +
  scale_fill_manual(values=suggitt_regions$fill_color) +
  geom_sf(data=map_points, shape=1,stroke=1, size=4) + 
  theme_minimal() +
  theme(legend.position = c(0.15,0.35),
        legend.background = element_rect(color='black'),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.key.size = unit(5,'mm'),
        panel.grid = element_blank())


###############################################################
###############################################################
# Create some data for the charts.
# These are roughly equievlant to the actual charts from the paper,
# though I am totally eyeballing these numbers.





plot_E_data = tribble(
  ~region, ~fill_color, ~pie_A, ~pie_B
  'Arid', 'yellow',
  'Equatorial', 'green',
  'Warm temperate', 'purple',
  'Cold', 'grey70',
  'Polar', 'cyan'
)

##################################################
# Pie Charts
#

pie_chart_data = tribble(
  ~region, ~fill_color, ~pie_B, ~pie_C,
  'Arid', 'yellow', 5, 24,
  'Equatorial', 'green4', 10, 15,
  'Warm temperate', 'purple4', 45, 15,
  'Cold', 'grey80', 30, 30,
  'Polar', 'cyan2', 20,22
)

# Matching the ordering is tricky here. The ggplot fill argument wants a factor, so by 
# default it turn the region column into a factor ordered alphabetially. 
# We must make a factor first using the order.
# In the pie charts it will go 


pie_chart_data$region = fct_inorder(pie_chart_data$region)

pie_chart_B = ggplot(pie_chart_data, aes(x="", y=pie_B, fill=region)) + 
  geom_bar(stat='identity', width=1, color='black') +
  scale_fill_manual(values=pie_chart_data$fill_color) + 
  coord_polar('y',start=0, direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none',
        legend.background = element_rect(color='black'),
        legend.title = element_blank())
  
pie_chart_C = ggplot(pie_chart_data, aes(x="", y=pie_C, fill=region)) + 
  geom_bar(stat='identity', width=1, color='black') +
  scale_fill_manual(values=pie_chart_data$fill_color) + 
  coord_polar('y',start=0, direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none')

##################################################
# subplot D temp/precip change plot

plot_D_data = tribble(
  ~region, ~fill_color, ~precip_mean, ~precip_sd, ~temp_mean, ~temp_sd,
  'Arid', 'yellow',      0,            0.2,       0.102,      0.02,  
  'Equatorial', 'green4', 0.2,          0.6,       0.04,       0.02,
  'Warm temperate', 'purple4',0.18,     0.55,      0.075,      0.02,
  'Cold', 'grey80',      0.3,         0.25,       0.14,       0.02,
  'Polar', 'cyan2',       0.06,        0.2,        0.109,      0.04 
)

# There is no good way to place axis text in the middle of the plot,
# so this places it manually with geom_text
y_axis_text = data.frame(y=seq(0, 0.18, 0.02),x=-0.1)

plot_D_data$region = fct_inorder(plot_D_data$region)

# note that ordering of the diferent geoms matter here.
# The points should be last so they overlay the error bars, and x/y axis lines. 

plot_D = ggplot(plot_D_data, aes(x=precip_mean,y=temp_mean)) + 
  geom_hline(yintercept = 0, size=2) +
  geom_vline(xintercept = 0, size=2) + 
  geom_errorbar(aes(ymin = temp_mean - temp_sd, ymax=temp_mean+temp_sd), width=0) +        # height/width of 0 here gets rid of
  geom_errorbarh(aes(xmin=precip_mean-precip_sd, xmax=precip_mean+precip_sd), height=0) +  # the error bar ends
  geom_point(aes(color=region), size=5) +
  geom_point(shape=1, stroke=1, size=5, color='black') +
  scale_color_manual(values=plot_D_data$fill_color) + 
  scale_x_continuous(breaks=round(seq(-0.6,1,0.2),1)) + 
  geom_text(data=y_axis_text, aes(x=x,y=y,label=y)) + 
  labs(y='Temperature change (+ °C per decade)', x='Precipitation change (± mm per decade',
       color='') + 
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=12),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_text(),
        axis.ticks = element_blank(),
        legend.position = 'bottom')
##################################################
# subplot E temp/precip change plot

plot_E_data = tribble(
  ~region, ~fill_color,    ~y0,   ~y25,   ~y50,   ~y75,   ~y100,
  'Arid', 'yellow',       -0.5,   -0.35,  -0.2,   0.05,   0.25,
  'Equatorial', 'green4', -0.25,  -0.05,  0.01,   0.02,   0.25,
  'Warm temperate', 'purple4',-0.8,-0.3,  0,      0.25,   0.9,
  'Cold', 'grey80',       -0.5,   -0.2,   0.1,    0.4,    1.0,
  'Polar', 'cyan2',       -0.4,   -0.05,  0.2,    0.25,   0.5
)

# Order the region to what is specifed in the tribble()
# Note that its reveresed for the x axis ordering, but not for the fill order.
plot_E_data$region = fct_inorder(plot_E_data$region)

plot_E = ggplot(plot_E_data, aes(x=fct_rev(region), fill=region)) +
  geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100), stat='identity', position = 'identity', 
               width=0.8, color='black') +
  scale_fill_manual(values = plot_E_data$fill_color) + 
  scale_y_continuous(breaks = c(-1,0,1), limits=c(-1,1), position='right') + 
  geom_hline(yintercept = 0, linetype='dashed') +
  coord_flip() + # running just the above code makes vertical box plots. coord_flip makes them horizontal.
  theme_bw() + 
  labs(x='',y='', title = 'Ln (SR2/SR1) per decade') + 
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5),
        axis.text = element_text(size=12),
        legend.position = 'none')


##################################################
##################################################
# put it all together!
# Cowplot is the best option here for fine tuning the sizien/scales of the 5 subplots.
library(cowplot)

middle_legend = get_legend(pie_chart_B + theme(legend.position = c(0.5,0.4), 
                                               legend.text = element_text(size=8),
                                               legend.key.size = unit(5,'mm'),
                                               legend.direction = 'vertical'))
middle_row = plot_grid(pie_chart_B,middle_legend, pie_chart_C, nrow=1, rel_widths = c(1,0.5,1), rel_heights = c(1,0.5,1), labels = c('B','','C'))

bottom_row = plot_grid(plot_D, plot_E, rel_widths = c(0.9,0.8), labels = c('D','E'), hjust=c(0.5, -0.5))

final_plot = 5
plot_grid(plot_A_map, middle_row, bottom_row, ncol=1, rel_widths = c(1,1,1), rel_heights = c(1,0.7,1))
