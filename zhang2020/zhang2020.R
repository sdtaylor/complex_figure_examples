library(tidyverse)
library(cowplot)

##################################
# This is a replication of Figure 3 from the following paper
# Zhang, Qiang, et al. "Trait–environment relationships differ between mixed‐species flocking and non‐flocking bird assemblages." 
# 2020. Ecology. https://doi.org/10.1002/ecy.3124

# This figure uses simulated data and is not an exact copy. 
# It's meant for educational purposes only.  
#############################################

predictors = c('Elevation','NDVI','Arbor','Shrub','Herb','DBHmean','Heightmax','IVdomi','PC1','PC2')
traits = c('Diet.FRU','Diet.INS','Diet.OMN','Subs.CAN','Subs.GRU','Subs.MID','Subs.UND','Breeding','Nest',
              'Clutch','Body','Bill','Tarsus','Disperse','Habitat','Vertical')

# A plot like this requires a data.frame row for every row x columnn combination in the figure. expand_grid does this
# to make data for this example, but your data may need reshaping.

plot_a_data = expand_grid(trait = traits, predictor = predictors)
plot_b_data = expand_grid(trait = traits, predictor = predictors)

# Mark some random locations for color coding by grey/red/blue, indicating no/positive/negative associationes.
set.seed(1)
plot_a_data$significance = 'none'
plot_a_data$significance[sample.int(160, 3)] = 'negative' # 3 random entires choses out of a possible 160
plot_a_data$significance[sample.int(160, 2)] = 'positive' # 

plot_b_data$significance = 'none'
plot_b_data$significance[sample.int(160, 12)] = 'negative' # 3 random entires choses out of a possible 160
plot_b_data$significance[sample.int(160, 20)] = 'positive' # 

#######################
# Correctly order everything using factors. 
# For trait and predictor this dictactes the order of the axis labels. For significance this dictates
# the order the colors are assigned, which allows us to easily match the correct color using scale_fill_manual()

# The ordered=TRUE arguments sets the order as the one specified in the levels argument. For predictor and trait this
# ends up being the order specified in the first variables above.
# Note that in the ggplot call trait is wrapped with fct_rev. Since the traits are ordered top->bottom, but ggplot 
# orders from bottom->top (ie. starting at 0)
plot_a_data$significance = factor(plot_a_data$significance, levels=c('none','positive','negative'))
plot_a_data$predictor    = factor(plot_a_data$predictor, levels=predictors, ordered = TRUE)
plot_a_data$trait        = factor(plot_a_data$trait, levels=traits, ordered = TRUE)

plot_b_data$significance = factor(plot_b_data$significance, levels=c('none','positive','negative'))
plot_b_data$predictor    = factor(plot_b_data$predictor, levels=predictors, ordered = TRUE)
plot_b_data$trait        = factor(plot_b_data$trait, levels=traits, ordered = TRUE)
#######################

# Both x and y axis are discrete here. So to draw lines intercepting them we can
# place geom_vline and geom_hline at the appropriate 0.5 intervals.
# Note there are probably better visuals to distinguish the different trait groups...
trait_grey_lines = c(10.5,11.5,12.5,14.5,15.5)
trait_black_lines = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,13.5,16.5)

plot_a = ggplot(plot_a_data, aes(x=predictor, y=fct_rev(trait), fill=significance)) + 
  geom_tile() +
  scale_fill_manual(values=c('grey80','deepskyblue','red2')) + 
  geom_hline(yintercept = trait_black_lines, color='black', size=1) +
  geom_hline(yintercept = trait_grey_lines, color='grey95', size=1) + 
  geom_vline(xintercept = seq(0.5,11.5,1), color='black', size=1) + # predictor lines are at every interval
  scale_x_discrete(position = 'top', expand=c(0,0)) + # put the x axis text on top. expand = c(0,0) drops the buffer at the edges of the plot
  scale_y_discrete(expand=c(0,0)) + 
  theme(axis.title = element_blank(),
        axis.text = element_text(color='black',size=18),
        axis.text.x = element_text(angle=90, hjust=0),
        axis.ticks = element_blank(),
        legend.position = 'none')

plot_b = ggplot(plot_b_data, aes(x=predictor, y=fct_rev(trait), fill=significance)) + 
  geom_tile() +
  scale_fill_manual(values=c('grey80','deepskyblue','red2')) + 
  geom_hline(yintercept = trait_black_lines, color='black', size=1) +
  geom_hline(yintercept = trait_grey_lines, color='grey95', size=1) + 
  geom_vline(xintercept = seq(0.5,11.5,1), color='black', size=1) + # predictor lines are at every interval
  scale_x_discrete(position = 'top', expand=c(0,0)) + # put the x axis text on top. expand = c(0,0) drops the buffer at the edges of the plot
  scale_y_discrete(expand=c(0,0)) + 
  theme(axis.title = element_blank(),
        axis.text = element_text(color='black',size=18),
        axis.text.x = element_text(angle=90, hjust=0),
        axis.ticks = element_blank(),
        legend.position = 'none')

final_figure = plot_grid(plot_a, plot_b, nrow=1, labels=c('(a)','(b)'), label_size = 22)

#################################################
water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.05), aes(xmin=xmin,ymin=ymin, xmax=xmin+2.4,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') +
  draw_text(water_mark, x=0.05, y=0.1, size=20, hjust = 0)

save_plot('./zhang2020/zhang2020_final.png', plot=final_figure, base_height = 9, base_width = 12, dpi=50)
