library(tidyverse)
library(patchwork)
library(cowplot)

#--------------
# This is a replication of Figure 1 from the following paper
# Thein, M. M., Wu, L. M., Corlett, R. T., Quan, R. C., & Wang, B. (2020). Changes in seed predation 
# along a 2300‐m elevational gradient on a tropical mountain in Myanmar: a standardized test 
# with 32 non‐native plant species. Ecography. https://doi.org/10.1111/ecog.05385

# This figure uses simulated data and is not an exact copy. 
# It's meant for educational purposes only.  
#--------------
# The reproduced plot is made creating 4 different panels and combining them using the patchwork library.
# An alternative method is at the end to create the same figure using facets instead. 
#--------------


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
  geom_smooth(aes(y=total_removal),method='lm', formula = y~poly(x,2), show.legend = FALSE)  +             
  geom_point(aes(y=total_removal), size=point_size) +
  theme(legend.title = element_blank(),                                                  # Add the legend to the (c) panel. Position is relative to this panel only.
        legend.background = element_blank(),                                             # show.legend = FALSE in the geom_smooth for this panel turns off the line inside the legend
        legend.position = c(0.5,0.91)) +
  labs(y='Total seed removal (%)') +
  guides(color = guide_legend(override.aes = list(size=4.5)))                            # Make the points in the legend slightly larger to distinquish from surrounding points.

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
water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(primary_figure) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.05), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.6,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
  draw_text(water_mark, x=0.05, y=0.1, size=10, hjust = 0)

save_plot('./thein2020/thein2020_final.png', plot=final_figure, base_height = 8, base_width = 8)



#--------------
# Alternative method using facets
# This uses the tagger package to label a-c on the panels.
# https://github.com/eliocamp/tagger
#--------------
#
# This can also be done using a facet instead of 4 separate plots
# 1st need to convert the data to a long format, where there is a new column for the variable of interest
figure_data_long = figure_data %>%
  pivot_longer(c(-elev, -season), names_to='variable', values_to='variable_value')

# Convert the variable to a factor column, where the factor labels are the ultimate title on each panel
# The order here also defines the panel order (left -> right, top -> bottom)
variables = c('depot_encounter', 
              'removal_from_depot', 
              'total_removal', 
              'species_removed')
nice_variable_names = c('Seed depot encounter (%)',
                        'Seed removal from\n encountered depots (%)',
                        'Total seed removal (%)',
                        'Number of species removed')

figure_data_long$variable = factor(figure_data_long$variable, levels = variables, labels = nice_variable_names, ordered = TRUE)


alternate_method_figure = ggplot(figure_data_long, aes(x=elev, y=variable_value, color=season)) + 
  geom_point(size=3) + 
  geom_smooth(method='lm', formula = y~poly(x,2), show.legend = FALSE) +
  scale_color_manual(values = c('darkorange2','dodgerblue')) +
  scale_x_continuous(breaks=c(0.5, 1, 1.5, 2, 2.5, 3)) + 
  facet_wrap(~variable, scales='free', strip.position = 'left') +           # set te "strip" to the left side
  tagger::tag_facets(tag_prefix = '(', tag_suffix = ')') + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = c(0.75,0.425),
        legend.title = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=14),
        strip.text = element_text(size=18),
        strip.background = element_blank(),                                # Turn off the grey strip background, and 
        strip.placement = 'outside') +                                     # put it outside the axis numbers.
  labs(x='Elevation (km)', y='') +
  guides(color = guide_legend(override.aes = list(size=4.5)))              # Make the points in the legend slightly larger to distinquish from surrounding points.


