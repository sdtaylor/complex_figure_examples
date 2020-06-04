library(tidyverse)
library(cowplot) # cowplot is used here just for the disclaimer watermark at the end. Its not needed for the primary figure. 

##################################
# This is a replication of Figure 5 from the following paper
# Craven, D., Knight, T.M., Barton, K.E., Bialic-Murphy, L. and Chase, J.M., 2019. 
# Dissecting macroecological and macroevolutionary patterns of forest biodiversity across the Hawaiian archipelago. 
# Proceedings of the National Academy of Sciences, 116(33), pp.16436-16441. https://doi.org/10.1073/pnas.1901954116

# Data used here are simulated, and not meant to replicate the original figure exactly.
##################################
set.seed(1)

data_starter = tribble(
  ~island, ~species, ~beta_s_mean,
  "Hawai'i",'All species', 8,
  "Hawai'i",'Native species', 7,
  "Maui Nui",'All species', 7,
  "Maui Nui",'Native species', 6.5,
  "O'ahu",'All species', 9,
  "O'ahu",'Native species', 11,
  "Kaua'i",'All species', 10,
  "Kaua'i",'Native species', 17,
)

# Make the order as the same specified above. This affects the left-right ordering on the x-axis.
data_starter$island = fct_inorder(data_starter$island)

figure_data = data_starter %>%
  group_by(island, species) %>%
  summarise(beta_s = rnorm(n=100, mean=beta_s_mean, sd=2)) %>%
  ungroup()

# The fading effect around the points of this plot are points of individual observations. 
# They have a low alpha value, so when a lot are stacked together it becomes darker. 
# Its done here with geom_point() and the alpha argument.

# How does position_dodge() know to split them up by species?? It references the group argument in the first aes() call,
# which is not set explicitly but gets set automatically when color is set. Try replacing color with group to see what happens. 

final_figure = ggplot(figure_data, aes(x=island, y=beta_s, color=species)) +
  geom_point(position = position_dodge(width = 0.5),size=3, alpha=0.1) +
  stat_summary(geom='point', fun = 'mean',                 # This places the mean point at the center.
               size=7,
               position = position_dodge(width=0.5)) +
  scale_color_manual(values = c("purple4","darkorange")) +
  theme_bw(25) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        panel.grid = element_blank(),
        panel.background = element_rect(size = 2, color='black'),
        axis.text = element_text(face = 'bold',color='black'),
        legend.text = element_text(face='bold'),
        axis.title = element_text(face='bold')) +
  labs(x='', y='βs', color='')   +             # Note the beta symbol is a unicode character. Easiest method is copy pasting from wikipedia directly into R.
 guides(color=guide_legend(override.aes = list(size=10))) # With guides the legend symbols can be made bigger than whats in the plot.

#############################################
water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.15), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.5,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
  draw_text(water_mark, x=0.05, y=0.2, size=10, hjust = 0)

save_plot('./craven2019/craven2019_final.png',plot = final_figure, base_height = 6.5, base_width = 10)
