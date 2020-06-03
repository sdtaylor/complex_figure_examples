library(tidyverse)
library(cowplot)
library(ggnewscale)
library(png)

##################################
# This is a replication of Figure 1 from the following paper
# Cottrell, R.S., Blanchard, J.L., Halpern, B.S. et al. Global adoption of novel 
# aquaculture feeds could substantially reduce forage fish demand by 2030. 
# Nature Food 1, 301–308 (2020). https://doi.org/10.1038/s43016-020-0078-x

# This figure uses simulated data and is not an exact copy. 
# It's meant for educational purposes only.  
#############################################
# First generate random data to use in the figure
generate_random_timeseries = function(n,initial_value, trend,error){
  ts = 1:n
  ts[1] = initial_value
  for(t in 2:n){
    ts[t] = ts[t-1] + trend + rnorm(1, mean=0, sd=error)
  }
  return(ts)
}

set.seed(2)

years=1960:2013
timeseries_data = tibble(aquaculture = generate_random_timeseries(n=length(years),initial_value = 0, trend=4e4, error = 2e6))
timeseries_data$total_animal_feed = timeseries_data$aquaculture + 8e6 + rnorm(nrow(timeseries_data), mean=0,sd=2e6)
timeseries_data$historic_supply   = timeseries_data$aquaculture + 20e6 + rnorm(nrow(timeseries_data), mean=0,sd=2e6)
timeseries_data$year = years

# Make the timeseries data tidy for use in ggplot
timeseries_data_long = timeseries_data %>%
  pivot_longer(cols =c(aquaculture, total_animal_feed, historic_supply), names_to = 'demand_type', values_to = 'tonnes')

# It's common to have values with _ and stuff in them so they're easier to use column names and stuff,
# but those  usually make for bad figure text. Use a factor to assign nice labels, and at the same time
# specify the order of those labels. Which specified the order they get colored and the order in the legend.
demand_type_values = c('historic_supply','total_animal_feed','aquaculture')
demand_type_labels = c('Historical Supply','Total animal feed use','Aquaculture use')

timeseries_data_long$demand_type = factor(timeseries_data_long$demand_type, levels=demand_type_values, 
                                          labels = demand_type_labels, ordered = TRUE)
######################################
avg_value_labels = tribble(
  ~label_y, ~avg_label,
  32e6, 'Average Supply\n1980-2013',
  24e6, 'Proposed\nEBFM limit', 
  18e6, 'Average feed use\n1980-2013'
)

timeseries_plot = ggplot(timeseries_data_long,aes(x=year, y=tonnes, color=demand_type)) +
  geom_line(size=2) +
  scale_color_manual(values=c('steelblue1','black','grey50')) + 
  ggnewscale::new_scale_color() + 
  geom_segment(x=2013,xend=2035,y=30e6,yend=30e6, color='black', linetype='solid', size=1.5) +  # avg supply '80-'13            # These 3 lines and labels off to the  right are easiest to do 1 at a time. 
  geom_segment(x=2013,xend=2035,y=22e6,yend=22e6, color='black',linetype='dashed', size=1.5) + # proposed limit                # Its possible to make a data.frame defining each one as was done with the text, but
  geom_segment(x=2013,xend=2035,y=20e6,yend=20e6, color='grey50',linetype='solid', size=1.5) +  # avg feed use                  # it gets  complicated because of the different colors also used in the mains lines and forming a single legend. 
  geom_text(data=avg_value_labels,aes(x=2014,y=label_y,label=avg_label),size=8,hjust=0,inherit.aes = F) +   # inherit.aes=FALSE here because the label data.frame does not have demand type, specified in the main ggplot call
  geom_vline(xintercept = 2013, color='grey50',linetype='dashed') +                                            # y, and x values are different than whats in the main ggplot() call.
  scale_x_continuous(breaks=c(1970,1990,2010,2030), limits = c(1960,2035)) +
  #scale_y_continuous(labels =  scales::label_math(.x, format=function(l){paste(l/10e5,'x 10^6')})) +
  #scale_y_continuous(labels = function(l){paste(l/10e5,'x 10^6')}) +
  scale_y_continuous(limits = c(0,4.1e7),breaks = c(0,1e7,2e7,3e7,4e7), expand = c(0,0),    # Expand here takes the buffer off the top and bottom, putting the 0 values directly on the bottom axis line.
                     labels=parse(text=c('0','10 %*% 10^6','20 %*% 10^6','30 %*% 10^6','40 %*% 10^6'))) + # scales::label_math or scales::label_scientific are probably able to make these labels automatically,
  theme_classic(35) +                                                                                     # but I could  not figure out how to get this exact format. 
  theme(legend.position = c(0.18,0.98),
        legend.text = element_text(size=25),
        legend.key.width = unit(25,'mm'),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.line = element_line(color='grey80'),
        axis.text = element_text(color='black'))  +
  labs(y='Forage fish demand (tonnes)',x='Year')

########################################################
# The bar plot

# eyeball the mean values
barplot_data = tribble(
  ~scenario,~demand,~demand_sd,
  'BAU',    22e6,   2e6,
  'Rap.Gr.',26e6,   2e6,
  'Cons.Shft',37e6, 2e6
)

# Make scenario and put them in the same order specified in the tribble
# to specify the left-> right order on the x-axis. 
barplot_data$scenario = fct_inorder(barplot_data$scenario)

barplot = ggplot(barplot_data, aes(x=scenario, y=demand, fill=scenario)) + 
  geom_col(width=0.5) + 
  scale_fill_manual(values=c('midnightblue','palegreen3','deeppink3')) + # interesting color choices here...
  geom_errorbar(aes(ymax = demand+demand_sd, ymin=demand-demand_sd), width=0.1, size=1) + 
  geom_hline(yintercept = 30e6, color='black', linetype='solid',size=1.5) +  # avg supply '80-'13            
  geom_hline(yintercept=22e6, color='black',linetype='dashed',size=1.5) + # proposed limit                
  geom_hline(yintercept=20e6, color='grey50',linetype='solid',size=1.5) +   # avg feed use   
  scale_y_continuous(limits = c(0,4.1e7),breaks = c(0,1e7,2e7,3e7,4e7), expand = c(0,0),
                     labels=parse(text=c('0','10 %*% 10^6','20 %*% 10^6','30 %*% 10^6','40 %*% 10^6'))) + 
  theme_bw(35) +
  theme(legend.position = 'none',
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color='grey80'),
        axis.text = element_text(color='black')) +
  labs(y='Forage fish demand (tonnes)', x='Aquaculuture 2030 scenario')

#######################################################
fish = png::readPNG('cottrell2020/fish.png')

final_figure = plot_grid(timeseries_plot, barplot, nrow = 1, 
                         rel_heights = c(0.9,0.9), rel_widths = c(1,0.7), 
                         labels = c('a','b'), label_size = 40) +
  draw_image(fish,x=0.44, y=0.82, width=0.12, height=0.12)

water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(final_figure) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.05), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.4,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') +
  draw_text(water_mark, x=0.05, y=0.1, size=20, hjust = 0)

save_plot('./cottrell2020/cottrell2020_final.png', plot=final_figure, 
          base_height = 30, base_width = 70, units='cm', dpi=50)

