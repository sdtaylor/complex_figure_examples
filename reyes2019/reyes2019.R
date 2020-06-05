library(tidyverse)
library(cowplot) # cowplot is used here just for the disclaimer watermark at the end. Its not needed for the primary figure. 


cause_of_loss = c('Wind/Excess Wind','Hot Wind','Heat','Hail','Freeze','Flood','Failure Irrig Supply',
                  'Excess Moisture/Precip/Rain','Drought','Cold Wet Weather')
time_periods  = c('Annual','JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')

all_data = expand_grid(cause = cause_of_loss, time = time_periods)

# Order them in the order specified above. Note for the cause loss the  order will be reversed
# in ggplot call because I made the order top->bottom, but ggplot orders it bottom->top (ie. starting at 0 on the y-axis)
all_data$cause = factor(all_data$cause, levels = cause_of_loss, ordered = TRUE)
all_data$time =  factor(all_data$time,  levels = time_periods, ordered = TRUE)

set.seed(1)
# Make some random significance values
all_data$tau = rnorm(n=nrow(all_data), mean=0, sd=0.3)
all_data$sig = ifelse(abs(all_data$tau) > 0.6 , 'yes','no')


final_figure = ggplot(all_data, aes(x=time, y=fct_rev(cause), color=tau, shape=sig)) + 
  geom_point(size=14, stroke=5) +
  scale_color_distiller(palette = 'RdBu', limits=c(-1,1)) +
  scale_shape_manual(values=c(1,4)) +
  theme_bw(28) +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        axis.title = element_blank(),
        legend.direction = 'horizontal') +
  guides(color=guide_colorbar(title.position = 'top', title = 'tau', barwidth = unit(8,'cm'), barheight = unit(1.5,'cm')),
         shape=guide_legend(title.position = 'top', title = 'Significance', override.aes = list(size=8,stroke=3)))

#############################################
water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.15), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.5,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
  draw_text(water_mark, x=0.05, y=0.2, size=15, hjust = 0)

ggsave(plot = final_figure, filename = 'reyes2019/reyes2019_final.png', width=45, height = 25, unit='cm', dpi=50)
