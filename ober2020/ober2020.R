library(tidyverse)
library(cowplot) # cowplot is used here just for the disclaimer watermark at the end. Its not needed for the primary figure. 

##################################
# This is a replication of Figure 2 from the following paper
# Ober, H.K., Jones, G.M., Gottlieb, I.G., Johnson, S.A., Smith, L., Brosi, B. and Fletcher, R.J., Jr. (2020), 
# Bat community response to intensification of biomass production 
# for bioenergy across the southeastern USA. Ecol Appl
# https://doi.org/10.1002/eap.2155

# Data used here are simulated, and not meant to replicate the original figure exactly.
##################################
bats = c('Big brown*','Eastern red*','Hoary','Southeastern myotis',
         'Little brown','Evening','Tri-colored','Brazillian free-tailed')

habitats = c('Corn','Residue removed','Residue left','Unthinned','Thinned','Young','Mature','Reference')

all_data = expand_grid(bat = bats, habitat = habitats)

# Simulate some data to get the mean, and upper/lower 95 confidence intervals in the  0-1 bounds
all_data$occurance_prob_mean = runif(nrow(all_data), min=0.01,max=0.95)
all_data$occurance_prob_upper   = pmin(all_data$occurance_prob_mean + rbeta(nrow(all_data), 2,5),1)
all_data$occurance_prob_lower   = pmax(all_data$occurance_prob_mean - rbeta(nrow(all_data), 2,5),0)

##########################################
# Create factors in the data data.frame with the explicit order of the vectors specified above.
# This affects the ordering of the facets (bats) and y-axis (habitat) of the plot.
all_data$bat = factor(all_data$bat, levels = bats, ordered = TRUE)
all_data$habitat = factor(all_data$habitat, levels = habitats, ordered = TRUE)

##########################################
# These lines are all that is needed for the basic figure outline. 
# Everything else is just theme elements. 
primary_figure = ggplot(all_data, aes(y=fct_rev(habitat), x=occurance_prob_mean)) + 
  geom_errorbarh(aes(xmax = occurance_prob_upper, xmin = occurance_prob_lower),size=1, height=0) + # Height here  turns off vertical ends on error bars
  geom_point(size=4, color='black') +    # You could used the shape=1 argument here to get a circle instead of a solid point. But the error bar
  geom_point(size=2, color='white') +  # would be visible through it. Instead draw a small white point on top of a slightly larger black point.
  scale_x_continuous(breaks = c(0.2,0.5,0.8)) + 
  facet_wrap(~bat, nrow=2) +
  labs(x='Occurrence probability (95% CRI)', y='')


final_figure = primary_figure +
  theme_bw(10) +
  theme(strip.background = element_blank(), # Turn off strip background so the bat names "float"
        panel.grid       = element_blank(), # turn off x/y grid lines
        panel.background = element_rect(size=2, color='black'),  # make the border on each subplot bigger
        axis.ticks = element_line(size = 1),
        axis.text = element_text(size=10, face = 'bold', color='black'),
        axis.title = element_text(size=10, face = 'bold', color='black'),
        strip.text = element_text(size=10, face = 'bold', color='black'))
  
#############################################

water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.05), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.5,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
  draw_text(water_mark, x=0.05, y=0.1, size=10, hjust = 0)

save_plot('./ober2020/ober2020_final.png',plot = final_figure, base_height = 6.5, base_width = 10)
