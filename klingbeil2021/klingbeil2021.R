library(tidyverse)
library(cowplot)
library(png)


#-------------------------------
# This is a replication of Figure 2 from the following paper
# Klingbeil, B.T., Cohen, J.B., Correll, M.D. et al. High uncertainty over the future of 
# tidal marsh birds under current sea-level rise projections. Biodivers Conserv (2021). 
# https://doi.org/10.1007/s10531-020-02098-z

# It's meant for educational purposes only.  
#-------------------------------

# Data estimated from the original plot.

figure_data = tribble(
  ~bird_type,  ~model_type,  ~marsh_area_percent,
  'AREA',      'dynamic',    0.12,
  'AREA',      'static',     0.10,
  'CLRA',      'dynamic',    0.10,
  'CLRA',      'static',     0.05,
  'NESP',      'dynamic',    0.20,
  'NESP',      'static',     0.25,
  'SALS',      'dynamic',    0.18,
  'SALS',      'static',     0.10,
  'SESP',      'dynamic',    0.10,
  'SESP',      'static',     0.05,
  'WILL',      'dynamic',    0.11,
  'WILL',      'static',     0.07
)


# Make the bird_type a factor with ordering as the order seen in the data.frame. This determines the x-axis order.
# This isn't strictly necccesary, as the order seen in the original plot is alphabetical,
# and ggplot will order it that way by default.
figure_data$bird_type = forcats::fct_inorder(figure_data$bird_type)

#-------------------------------
# load images
#-------------------------------
bird1 = png::readPNG('klingbeil2021/animals/PhyloPic.3baf6f47.Ferran-Sayol.Mesitornis_Mesitornis-unicolor_Mesitornithidae.png')
bird2 = png::readPNG('klingbeil2021/animals/PhyloPic.ad11bfb7.Ferran-Sayol.Threskiornis_Threskiornis-aethiopicus_Threskiornithidae_Threskiornithinae.png')
bird3 = png::readPNG('klingbeil2021/animals/PhyloPic.ca1082e0.George-Edward-Lodge-modified-by-T-Michael-Keesey.Aves.png')
grass = png::readPNG('klingbeil2021/animals/PhyloPic.822f98c6.Mason-McNair.Paspalum_Paspalum-vaginatum.png')

#-------------------------------

main_figure = ggplot(figure_data, aes(x=bird_type, y=marsh_area_percent, fill=model_type)) +
  geom_col(aes(y=1), position = position_dodge(width = 0.9),width=0.8, alpha=0.2) +     # This first geom_col is for the faint colored bars
  geom_col(position = position_dodge(width=0.9), width = 0.8) +                         # This geom_col draws the primary colored bars
                                                                                        # The width inside position_dodge() controls distance between grey/green bars.
                                                                                        # Width for geom_col controls distance between paired bars (ie. between the bird types)
  scale_fill_manual(values=c('darkgreen','grey20')) + 
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1.0), labels = function(x){paste0(ceiling(x*100),'%')}) +     # y axis is 0-1 but the label function converts to %
  theme_bw() + 
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        panel.border = element_blank(),               # Turn off the full border but initialize the bottom and left lines
        axis.line.x.bottom = element_line(size=0.5),
        axis.line.y.left   = element_line(size=0.5),
        axis.title = element_text(size=15, face = 'bold'),
        axis.text = element_text(color='black', size=10, face='bold'),
        axis.ticks.x = element_blank()) +
  labs(y='Percent of 2010 Estimate', x='SLR Response')

#---------------------------
# Add the silhouettes. The position and size of these takes a few minutes of
# trial and error.

figure_with_animals = ggdraw() + 
  draw_plot(main_figure, height=0.9) +
  draw_image(grass, x=0.11, y=0.87, width=0.1, height=0.1) +  # AREA
  draw_image(bird1, x=0.28, y=0.86, width=0.1, height=0.1) +  # CLRA
  draw_image(bird2, x=0.40, y=0.86, width=0.1, height=0.1) +  # NESP
  draw_image(bird3, x=0.56, y=0.86, width=0.1, height=0.1) +  # SALS
  draw_image(bird1, x=0.70, y=0.86, width=0.1, height=0.1) +  # SESP
  draw_image(bird2, x=0.85, y=0.86, width=0.1, height=0.1)    # WILL

#---------------------------
# Add the watermark

water_mark = 'Example figure for educational purposes only. Not made with real data.\n See github.com/sdtaylor/complex_figure_examples'

final_figure = ggdraw(figure_with_animals) +
  geom_rect(data=data.frame(xmin=0.05,ymin=0.01), aes(xmin=xmin,ymin=ymin, xmax=xmin+0.6,ymax=ymin+0.1),alpha=0.9, fill='grey90', color='black') +
  draw_text(water_mark, x=0.05, y=0.05, size=10, hjust = 0)

save_plot('./klingbeil2021/klingbeil2021_final.png', plot=final_figure, base_height = 6, base_width = 10)
