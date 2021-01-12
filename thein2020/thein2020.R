library(tidyverse)
library(patchwork)
library(cowplot)





figure_data = read_csv('thein2020/simulated_data.csv')

# Here we make 4 unique plots for the 4 variables.
# They share everything in column except the data, so a common
# base_figure can be specified with everything except the geoms.
# 
# Note how the x and color values here get passed on to geom_point() and 
# geom_smooth() for all plots.
base_figure = ggplot(figure_data, aes(x=elev, color=season)) + 
  scale_color_manual(values = c('darkorange2','dodgerblue')) +
  scale_x_continuous(breaks=c(0.5, 1, 1.5, 2, 2.5, 3)) + 
  theme_bw() + 
  theme(legend.position = 'none') +           # Turn off the legend for all plots. This can be turned back on in the plot where it's needed. 
  labs(x='Elevation (km)')

point_size = 3

panel_a = base_figure + 
  geom_smooth(aes(y=depot_encounter),method='lm', formula = y~poly(x,2))  +             # Fit a 2nd order polynomial smoothing line, this needs to be repeated for each 
  geom_point(aes(y=depot_encounter), size=point_size) +                                 # panel using the respective variable.
  labs(y='Seed depot encounter (%)')

panel_b = base_figure + 
  geom_smooth(aes(y=removal_from_depot),method='lm', formula = y~poly(x,2))  +             
  geom_point(aes(y=removal_from_depot), size=point_size) +
  labs(y='Seed removal from\n encountered depots (%)')                                   # Note the \n to get the text to wrap

panel_c = base_figure + 
  geom_smooth(aes(y=total_removal),method='lm', formula = y~poly(x,2))  +             
  geom_point(aes(y=total_removal), size=point_size) +
  theme(legend.title = element_blank(),                                                  # Add the legend to the (c) panel. Position is relative to this panel only.
        legend.position = c(0.5,0.91)) +
  labs(y='Total seed removal (%)')

panel_d = base_figure + 
  geom_smooth(aes(y=species_removed),method='lm', formula = y~poly(x,2))  +            
  geom_point(aes(y=species_removed), size=point_size) +
  labs(y='Number of species removed')


# put them together with patchwork
primary_figure = panel_a + panel_b + panel_c + panel_d + 
  plot_layout(ncol=2) + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &             # This & is special for the patchwork library and applies the theme to all plots.    
  theme(plot.tag.position = c(0.2,0.95),                                              # See https://patchwork.data-imaginist.com/articles/guides/annotation.html for detail
        plot.tag = element_text(face = 'bold'),
        panel.grid = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=14))
 

#--------------
# Alternative method using facets
# This doesn't quit work as the facet labels end up being on top, but it's still makes for a nice figure.
#
# This can also be done using a facet instead of 4 separate plots
# 1st need to convert the data to a long format, where there is a new column for the variable of interest
figure_data_long = figure_data %>%
  pivot_longer(c(-elev, -season), names_to='variable', values_to='variable_value')


# Convert the variable to a factor column, where the factor labels are the ultimate title on each panel
variables = c('depot_encounter', 
              'removal_from_depot', 
              'total_removal', 
              'species_removed')
nice_variable_names = c('Seed depot encounter (%)',
                        'Seed removal from\n encountered depots (%)',
                        'Total seed removal (%)',
                        'Number of species removed')

figure_data_long$variable = factor(figure_data_long$variable, levels = variables, labels = nice_variable_names, ordered = TRUE)


ggplot(figure_data_long, aes(x=elev, y=variable_value, color=season)) + 
  geom_point(size=3) + 
  geom_smooth(method='lm', formula = y~poly(x,2)) +
  scale_color_manual(values = c('darkorange2','dodgerblue')) +
  scale_x_continuous(breaks=c(0.5, 1, 1.5, 2, 2.5, 3)) + 
  facet_wrap(~variable, scales='free', strip.position = 'left') +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = c(0.75,0.425),
        legend.title = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=14),
        strip.text = element_text(size=18),
        strip.background = element_blank(),
        strip.placement = 'outside') +
  labs(x='Elevation (km)', y='')

