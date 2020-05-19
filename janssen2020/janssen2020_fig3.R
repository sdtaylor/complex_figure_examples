library(tidyverse)
library(cowplot) # cowplot is used here just for the disclaimer watermark at the end. Its not needed for the primary figure. 

##################################
# This is a replication of Figure 2 from the following paper
# Janssen, T., Fleischer, K., Luyssaert, S., Naudts, K. and Dolman, H., 2020. Drought resistance increases from the individual to the ecosystem level in highly diverse neotropical rain forest: a meta-analysis of leaf, tree and ecosystem responses to drought. Biogeosciences. https://doi.org/10.5194/bg-17-2621-2020

# Data used here are simulated, and not meant to replicate the original figure exactly.

##############################
# Specify each y-axis label
leaf_measurements = c('Midday stomatal conductance',
                      'Midday photosynthesis',
                      'Midday intrinsic water use efficiency',
                      'Predawn water leaf potental',
                      'Midday leaf water potential')

tree_measurements = c('Midday water potential gradient',
                      'Midday soil-leaf hydraulic conductance',
                      'Midday crown conduncance',
                      'Daily transpiration',
                      'Stem diameter growth',
                      'Leaf flushing',
                      'Litterfall')

ecosystem_measurements = c('Evapotranspiration',
                           'Net ecosystem productivity',
                           'Net primary producivity',
                           'Above-ground net primary productivity',
                           'Gross ecosystem productivity',
                           'Ecosystem respiration',
                           'Ecosystem water use efficiency')

drought_types = c('seasonal','eposodic')

# Setup a tidy data frame so each row represents a single point/line
leaf_entries = expand_grid(measurement_level='leaf', measurement = leaf_measurements, drought_type = drought_types)
tree_entries = expand_grid(measurement_level='tree', measurement = tree_measurements, drought_type = drought_types)
ecosystem_entries = expand_grid(measurement_level='ecosystem', measurement = ecosystem_measurements, drought_type = drought_types)

all_data = leaf_entries %>%
  bind_rows(tree_entries) %>%
  bind_rows(ecosystem_entries)

# Simulate some data to get the mean, and upper/lower 95 confidence intervals in the  0-1 bounds
set.seed(2)
all_data$change_mean = runif(nrow(all_data), min=-1,max=0.25)
all_data$change_upper   = pmin(all_data$change_mean + rbeta(nrow(all_data), 2,5),1)
all_data$change_lower   = pmax(all_data$change_mean - rbeta(nrow(all_data), 2,5),-1)

# Simulate random significance tests
all_data$sig_factor = sample(c('n.s.','*','**','***'), size=nrow(all_data), replace = TRUE)
all_data$sample_size = round(runif(n=nrow(all_data), min=5,max=50),0)

# Build a text label to show significance test results and sample size adjacent to each line.
all_data = all_data %>%
  mutate(measurement_text = paste0(sig_factor,'(',sample_size,')'))

# The small offset between the red/blue values can be done by making a small nudge value
# based on the type. y_nudge is then used inside position_nudge in ggplot
all_data = all_data %>%
  mutate(y_line_nudge = case_when(
           drought_type == 'seasonal' ~ 0.15,
           drought_type == 'eposodic' ~ -0.15),
         y_text_nudge = case_when(
           drought_type == 'seasonal' ~ 0.5,
           drought_type == 'eposodic' ~ -0.5))

# Setup a factor to put the measurement level in the top->down order we want in the facet_wrap
# Also use it to assign a nice label with A-B labelling
measurement_levels = c('leaf','tree','ecosystem')
measurement_level_labels = c('(a) Leaf','(b) Tree','(c) Ecosystem')
all_data$measurement_level = factor(all_data$measurement_level, levels = measurement_levels, labels = measurement_level_labels, ordered = TRUE)

# Specify the ordering of the y-axis measurments. 
# This will be ordered in the order specified for each above. 
# Note ggplot will start the first entry at 0 and go up. Thus this uses fct_rev() in the first ggplot line to reverse that.
all_data$measurement = factor(all_data$measurement, levels = c(leaf_measurements, tree_measurements, ecosystem_measurements), ordered = TRUE)

final_figure = ggplot(all_data, aes(y=fct_rev(measurement), x=change_mean, color=drought_type)) +
  geom_vline(xintercept = 0) + 
  geom_point(position = position_dodge(width = 0.5), size=2) + 
  geom_errorbarh(aes(xmin=change_lower, xmax=change_upper), position = position_dodge(width=0.5),
                 height=0, size=1) + 
  geom_text(aes(label = measurement_text), position = position_dodge(width = 2), # Note the dodge with for the text is higher to push them above/below the lines
            size=3.5) +
  scale_color_manual(values=c( "#0072B2", "#D55E00")) +
  scale_x_continuous(breaks=c(-1,-0.5,0,0.5), labels = function(x){round(x*100,0)}) + ## Use a function in the x_axis labels to convert the  percentage to a whole number
  facet_wrap(~measurement_level, ncol=1, scales='free_y') +   ## The scales='free_y' argument here makes it so each plot only has
  theme_bw(15) +                                              # the respective measurements for each level, and not empty values for others.
  theme(legend.position = 'none',                             # remove it to see what happens
        strip.background = element_blank(),
        strip.text = element_text(hjust=0, size=18, face = 'bold')) +
  labs(x='',y='')

#############################################

water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'
 
final_figure = ggdraw(final_figure) +
 geom_rect(data=data.frame(xmin=0.05,ymin=0.05), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.5,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
 draw_text(water_mark, x=0.05, y=0.1, size=10, hjust = 0)

save_plot('./janssen2020/janssen2020-fig3_final.png',plot = final_figure, base_height = 10, base_width = 10)
