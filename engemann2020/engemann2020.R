library(tidyverse)
library(cowplot)  # cowplot is used here just for the disclaimer watermark at the end. Its not needed for the primary figure. 

##################################
# This is a replication of Figure 1 from the following paper
# Engemann et al. 2020. Associations between growing up in natural 
# environments and subsequent psychiatric disorders in Denmark. 
# Environmental Research. https://doi.org/10.1016/j.envres.2020.109788.

# Data used here are simulated, and not meant to replicate the original figure exactly.
##################################

hazard_data = tribble(
  ~space_type, ~space_subtype,                        ~hazard_ratio, ~hazard_sd,
  'Agriculture',  'Agriculture basic',                 0.88,          0.01,
  'Agriculture',  'Agriculture adjusted',              0.89,          0.01,
  'Green space',  'Near-natural green space basic',    0.78,          0.05,
  'Green space',  'Near-natural green space adjusted', 0.85,          0.05,
  'Blue space',   'Blue space basic',                  0.84,          0.025,
  'Blue space',   'Blue space adjusted',               0.86,          0.025,
  'Urban',        'Urban',                             1.0,           0)

# Use fct_inorder to set the factor order specified in the table above.
# Otherwise the order will be alphabetical.
hazard_data$space_subtype = fct_inorder(hazard_data$space_subtype)
hazard_data$space_type = fct_inorder(hazard_data$space_type)

final_figure = ggplot(hazard_data, aes(x=space_type, y=hazard_ratio, color=space_subtype)) + 
  geom_hline(yintercept = 1, linetype='dotted', size=1) + # put the horizontal line first so points are drawn on top of it
  geom_point(position = position_dodge(width=0.2), size=4) +
  scale_color_manual(values = c('gold','goldenrod4','darkolivegreen3','green4','skyblue2','blue4','grey80')) + 
  geom_errorbar(aes(ymax = hazard_ratio + hazard_sd, ymin = hazard_ratio - hazard_sd), width=0.05,
                position = position_dodge(width = 0.2)) + 
  theme_bw(20) + 
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),               # Turn off the full border but initialize the bottom and left lines
        axis.line.x.bottom = element_line(size=0.5),
        axis.line.y.left   = element_line(size=0.5),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.text = element_text(size=12),
        legend.title = element_blank()) +
  labs(x='',y='Hazard ratio (95% CI)')
  
#############################################
water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.1,ymin=0.25), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.5,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
  draw_text(water_mark, x=0.12, y=0.3, size=8, hjust = 0)


save_plot('engemann2020/engemann2020_final.png', plot = final_figure, base_width = 10, base_height = 5)
