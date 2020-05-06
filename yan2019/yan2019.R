library(tidyverse)
library(cowplot)

#####################################
# Generate some similar looking data for the figures
generate_random_timeseries = function(n,initial_value,error){
  ts = rep(NA, n)
  ts[1] = initial_value
  for(t in 2:n){
    ts[t] = ts[t-1] + rnorm(1, mean=0, sd=error)
  }
  return(ts)
}

generate_correlated_timeseries = function(ts, error, scale_min, scale_max){
  n=length(ts)
  new_ts = ts + rnorm(n=n, mean=0, sd=error)
  return(scales::rescale(new_ts, to=c(scale_min,scale_max)))
}


# The data consist of 3 locations, each with a GPP measurement (y axis) and 3 correlated visual indexes (Phenocam, landsat, & MODIS)
grassland = data.frame(gpp = generate_random_timeseries(n=76, initial_value = 0, error = 0.5), site='grassland')
grassland$phenocam = generate_correlated_timeseries(grassland$gpp, error=2, scale_min=-0.35,scale_max = -0.05)
grassland$landsat = generate_correlated_timeseries(grassland$gpp, error=2, scale_min=0.15, scale_max = 0.35)
grassland$modis = generate_correlated_timeseries(grassland$gpp, error=2, scale_min=0.15, scale_max = 0.4)

savanna = data.frame(gpp = generate_random_timeseries(n=42, initial_value = 0, error = 0.5), site='savanna')
savanna$phenocam = generate_correlated_timeseries(savanna$gpp, error=2, scale_min=0.35,scale_max = 0.4)
savanna$landsat = generate_correlated_timeseries(savanna$gpp, error=2, scale_min=0.3, scale_max = 0.4)
savanna$modis = generate_correlated_timeseries(savanna$gpp, error=2, scale_min=0.32, scale_max = 0.4)

shrubland = data.frame(gpp = generate_random_timeseries(n=73, initial_value = 0, error = 0.5), site='shrubland')
shrubland$phenocam = generate_correlated_timeseries(shrubland$gpp, error=2, scale_min=-0.45,scale_max = -0.2)
shrubland$landsat = generate_correlated_timeseries(shrubland$gpp, error=2, scale_min=0.1, scale_max = 0.28)
shrubland$modis = generate_correlated_timeseries(shrubland$gpp, error=2, scale_min=0.12, scale_max = 0.3)

scatter_plot_data = grassland %>%
  bind_rows(savanna) %>%
  bind_rows(shrubland) %>%
  pivot_longer(cols=c(phenocam, landsat, modis), names_to = 'sensor', values_to = 'sensor_value')


#### Now eyeball the data for the barplots
barplot_data = tribble(
  ~site, ~sensor, ~landcover, ~percent,
  'grassland','PhenoCam','Mesquite',10,
  'grassland','PhenoCam','Grass',80,
  'grassland','PhenoCam','Soil',10,
  'grassland','Landsat','Mesquite',0,
  'grassland','Landsat','Grass',75,
  'grassland','Landsat','Soil',25,
  'grassland','MODIS','Mesquite',4,
  'grassland','MODIS','Grass',48,
  'grassland','MODIS','Soil',48,
  'grassland','Footprint','Mesquite',5,
  'grassland','Footprint','Grass',45,
  'grassland','Footprint','Soil',50,
  
  'savanna','PhenoCam','Mesquite',52,
  'savanna','PhenoCam','Grass',43,
  'savanna','PhenoCam','Soil',5,
  'savanna','Landsat','Mesquite',25,
  'savanna','Landsat','Grass',60,
  'savanna','Landsat','Soil',15,
  'savanna','MODIS','Mesquite',30,
  'savanna','MODIS','Grass',50,
  'savanna','MODIS','Soil',20,
  'savanna','Footprint','Mesquite',30,
  'savanna','Footprint','Grass',30,
  'savanna','Footprint','Soil',40,
  
  'shrubland','PhenoCam','Shrub',70,
  'shrubland','PhenoCam','Soil',30,
  'shrubland','Landsat','Shrub',38,
  'shrubland','Landsat','Soil',62,
  'shrubland','MODIS','Shrub',45,
  'shrubland','MODIS','Soil',55,
  'shrubland','Footprint','Shrub',60,
  'shrubland','Footprint','Soil',40
)


#############################################################3
# 1st the scatter plot
# 

sensor_values = c('phenocam','landsat','modis')
sensor_labels = c('PhenoCam','Landsat','MODIS')
site_values = c('grassland','savanna','shrubland')
site_labels = c('WKG-Grassland','SRM-Savanna','WHS-Shrubland')

scatter_plot_data$sensor = factor(scatter_plot_data$sensor, levels = sensor_values, labels = sensor_labels, ordered = T)
scatter_plot_data$site = factor(scatter_plot_data$site, levels = site_values, labels = site_labels, ordered = T)

# Here the best option for the 3x3 grid of plots is facet_wrap, since it handles all the alignment
# it is too usefull not to use. The labelling of the rows and columns as in the original figure is
# not possible with facet_wrap though. So here we'll essentially "turn off" the default labels 
# by setting strip.background and strip.text to element_blank(), and then adding the 6 row/column
# labels manually.

ggplot(scatter_plot_data, aes(x=sensor_value, y=gpp)) + 
  geom_point() + 
  facet_wrap(site~sensor, scales='free', strip.position = 'left') +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) 


##################################################
# The barplots

barplot_sensor_order = c('PhenoCam','Landsat','MODIS','Footprint')
barplot_data$sensor = factor(barplot_data$sensor, levels = barplot_sensor_order, ordered = TRUE)

barplot_landcover_details = tribble(
  ~landcover, ~fill_color,
  'Soil',     'grey70',
  'Grass',    'darkorange2',
  'Mesquite', 'purple4',
  'Shrub',    'tan4'
)

barplot_data$landcover = factor(barplot_data$landcover, levels=barplot_landcover_details$landcover, ordered = TRUE)

# This is what I believe is a better version of the barplot using facet_grid
# which does not need to replicate each of the 3 plots.
# It's here for reference. 
a_better_barplot = 5
ggplot(barplot_data, aes(x=sensor, y=percent, fill=landcover)) + 
  geom_col(width=0.4) +
  scale_fill_manual(values = barplot_landcover_details$fill_color) + 
  scale_y_continuous(breaks=c(0,20,40,60,80), labels = function(x){paste0(x,'%')}, expand=c(0,0)) + 
  facet_grid(site~.) + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face='bold', hjust=0.5),
        axis.text.x = element_text(face = 'bold', size=10),
        axis.text.y = element_text(face = 'bold', angle = 90, hjust=0.5),
        legend.key.height  = unit(20,'mm'),
        ) +
  labs(x='',y='',title='Land cover proportions') +
  guides(fill = guide_legend(direction = 'vertical', label.position = 'right', title='',
                             keyheight = unit(20,'mm'), keywidth = unit(5,'mm'),
                             label.theme = element_text(angle=90, hjust=0.5, face='bold')))

