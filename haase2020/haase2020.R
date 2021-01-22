library(tidyverse)
library(cowplot)

#--------------
# This is a replication of Figure 2 from the following paper
# Haase, Catherine G., et al. 2020. "Body mass and hibernation microclimate may 
# predict bat susceptibility to white‐nose syndrome." Ecology and Evolution. 
# https://doi.org/10.1002/ece3.7070

# This figure uses simulated data and is not an exact copy. 
# It's meant for educational purposes only.  
#--------------

figure_data = tribble(
  ~species,                  ~survival, ~survival_sd,
  'Eptesicus fuscus',        105,       50,
  'Corynorhinus townsendii', 60,        110,
  'Myotis velifer',          60,        100,
  'Perimyotis subflavus',    -10,       25,
  'Myotis thysanodes',       -50,       100,
  'Myotis volans',           -55,       90,
  'Myotis lucifugus',        -60,       90,
  'Myotis evotis',           -70,       80,
  'Myotis ciliolabrum',      -90,       25
)

# Calculate low and high values for the error bars
figure_data = figure_data %>%
  mutate(survival_low  = survival - survival_sd,
         survival_high = survival + survival_sd)

# Make the y-axis a factor with order according to the survival (lowest to highest)
# note here the first position will be at the bottom (y = 0).
# If this is not done then the y-axis will be ordered alphabetically
figure_data$species = fct_reorder(figure_data$species, figure_data$survival)


final_figure = ggplot(figure_data, aes(y=species, x=survival)) + 
  geom_errorbarh(aes(xmin = survival_low, xmax=survival_high), height=0.2) +      # Put error bars first so the points get drawn on top.
  geom_point(size=4, shape=21, fill='#1b9e77', color='black', stroke=0.4) +       # the "21" shape is a point with an outer stroke. With this the fill arg becomes
  geom_vline(xintercept = 0, linetype='dashed') +                                 # the main color, and the color arg becomes the outer line color, and stroke the outer line width.
  theme_bw() +                                                                                
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(size=12, color='black'),
        axis.text.x = element_text(size=12, color='black'),
        axis.text.y = element_text(size=14, color='black', face = 'italic')) +
  labs(x='Difference between Predicted Survival and Winter Duration (days)', y='')


#--------------
water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.05), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.6,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
  draw_text(water_mark, x=0.05, y=0.1, size=10, hjust = 0)

save_plot('./haase2020/haase2020_final.png', plot=final_figure, base_height = 5, base_width = 8)
