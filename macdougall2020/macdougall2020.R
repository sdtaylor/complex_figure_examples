library(tidyverse)
library(cowplot)

##################################
# This is a replication of Figure 7 from the following paper
# MacDougall, Andrew H., et al. "Is there warming in the pipeline? A multi-model analysis of the zero emission commitment from CO2." 
# Biogeosciences Discussions (2020): 1-45. https://doi.org/10.5194/bg-17-2987-2020

# Data used here are mostly simulated, and not meant to replicate the original figure exactly.

#############################################

# Here I judged by eye the value of everything. Note there are newline characters (\n) in the model 
# names for the ones to wrap to 2 lines. Alternatilly you could use stringr::str_wrap(), but that would 
# not give as much control in exactly how they look. 
flux_data = tribble(
  ~model,         ~delta_n, ~f_ocean, ~f_land, ~zec, 
  'PLASIM-\nGENIE',  0.9,      -0.7,     -0.2,   -0.38,
  'GFDL\nESM2M',    0.6,      -0.4,     -0.2,   -0.23,
  'MPI-ESMI1.2',   0.6,      -0.4,     -0.4,   -0.3, 
  'CanESM5',       0.85,     -0.25,    -0.5,   -0.15,
  'MIROC-\nlite',    0.5,      -0.5,     -0.25,  -0.05,
  'MIROC-\nES2L',    0.65,     -0.4,     -0.3,   -0.05,
  'LOVE\nCLIM 1.2', 1.2,      -0.35,    -0.5,   -0.04,
  'UVic\nESCM 2.10',0.6,      -0.55,    -0.1,   -0.04,
  'Bern3D\n-LPX',    0.65,     -0.4,     -0.3,    0.02,
  'ACCESS-\nESM1.5', 0.55,     -0.35,    -0.1,    0.02,
  'MESM',          0.62,     -0.75,    -0.22,   0.03,
  'CNRM-ESM2',     0.88,     -0.25,    -0.5,    0.05,
  'DCESS',         0.85,     -0.4,     -0.25,   0.05,
  'UKESM1',        1.00,     -0.43,    -0.22,   0.25,
  'IAPRAS',        1.4,      -0.8,     -0.1,    0.251 
)

# The left to right ordering of models is small->large, based on the ZEC value of the lower bar plot. 
# Thats specified using this function in the forecats library, where the factor order is matched to some numeric value.
flux_data$model = fct_reorder(flux_data$model, flux_data$zec)

# The 3 variables in the a figure (delta_n, f_ocean, and f_land) need to be in a tidy format
# where they share a common column for the flux amount, and a new column is used to identify
# the flux type. The zec variable is excluded here as its used in the b figure. 
flux_data_long = flux_data %>%
  select(-zec) %>%
  pivot_longer(cols=c('delta_n','f_ocean','f_land'), names_to = 'flux_var', values_to = 'flux_value')

# Generate random values for the error bars
set.seed(2)
flux_data_long$flux_value_sd = runif(n=nrow(flux_data_long), 0.1, 0.3)
flux_data_long$flux_value_sd = with(flux_data_long, ifelse(flux_var=='f_ocean',NA,flux_value_sd))

# The lower error bars for f_land are tricky here. The bars are specified with position_stack, but there is no 
# corresponding way to "stack" the error bars. See https://stackoverflow.com/a/30873811
# The solution is to manually specify the error bars. So here the upper one is the sd around only the delta_n,
# while the lower one is the sd around the sum(f_ocean,f_land). Each will get there own geom_errorbar call.
upper_error_bars = flux_data_long %>%
  filter(flux_var == 'delta_n') %>%
  group_by(model) %>%
  summarise(ymax = flux_value + flux_value_sd,
            ymin = flux_value - flux_value_sd) %>%
  ungroup()

lower_error_bars = flux_data_long %>%
  filter(flux_var %in% c('f_ocean','f_land')) %>%
  group_by(model) %>%
  summarise(ymax = sum(flux_value) + flux_value_sd,
            ymin = sum(flux_value) - flux_value_sd) %>%
  ungroup()


# Specify the desired order of the 3 variables and assign the legend labels. 
# This affects the stacking and coloring order. I'm not sure exactly how this interacts with the negative values.
# If you have have a similar problem I recommend adjusting the ordering here until you get a solution you like.
# This can also potentially be adjusted by using reverse=TRUE insdie the position_stack() call for geom_col().
# Note also by default the legend will go, top to bottom, f_land, f_ocean, delta_n. Thats reversed in the guide() call, which 
# does not affect the stacking/color order. 
# Δ symbol was copy pasted from wikipedia. It can go straight into the label without any special calls. 
flux_data_long$flux_var = factor(flux_data_long$flux_var, levels = c('f_land','f_ocean','delta_n'), labels = c('F land','F ocean','-Δ N'), ordered = T)

# The flux amount column is then used for the y-axis, while they are assigned a color fill with
# the flux_var column.
barplot_a = ggplot(flux_data_long,aes(x=model, y=flux_value, fill=flux_var)) +
  geom_col(width=0.5, position = position_stack()) +
  geom_errorbar(data = upper_error_bars, aes(x=model,ymin = ymin, ymax = ymax), width=0, inherit.aes = FALSE) +  # Note these have to not inherit the aes, the error bar data.frames
  geom_errorbar(data = lower_error_bars, aes(x=model,ymin = ymin, ymax = ymax), width=0, inherit.aes = FALSE) +  # are expected to have a flux_value and flux_var columns.
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c('darkgreen','dodgerblue2','deepskyblue4')) + 
  scale_y_continuous(breaks=c(-1,-0.5,0,0.5,1.0,1.5), limits = c(-1.2,1.6)) + 
  theme_bw(5) + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(color='black'),
        axis.text.y= element_text(size=6),
        legend.title = element_blank(),
        legend.text = element_text(size=6),
        legend.position = c(0.07,0.91)) + 
  guides(fill=guide_legend(reverse = TRUE, keyheight = unit(2,'mm'))) + 
  labs(x='', y=bquote('Flux'~(Wm^-2)))  # The superscript in the axis label is done via bquote. Some tutorials also use expression()


barplot_b = ggplot(flux_data, aes(x=model, y=zec)) + 
  geom_col(width=0.5, fill='black') +
  geom_hline(yintercept = 0) +
  ylim(-0.4,0.4) + 
  theme_bw(5) + 
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),               # Turn off the full border but initialize the bottom and left lines
        axis.line.x.bottom = element_line(size=0.3),
        axis.line.y.left   = element_line(size=0.3),
        axis.text = element_text(color='black'),
        axis.text.y= element_text(size=6),
        legend.title = element_blank()) +
  labs(x='', y='ZEC (°C)')

final_figure = plot_grid(barplot_a, barplot_b, ncol=1, rel_heights = c(1,0.5), align = 'h', axis='lr',
                         labels = c('(a)','(b)'), label_size=7, label_x = 0.96, label_y=0.99, label_fontface = 'plain')

#############################################
water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.1,ymin=0.25), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.5,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') + 
  draw_text(water_mark, x=0.12, y=0.3, size=6, hjust = 0)

save_plot('macdougall2020/macdougall2020_final.png', plot = final_figure)


