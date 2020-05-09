library(tidyverse)
library(janitor)
library(cowplot)
library(png)

buffalo = png::readPNG('guyton2020/animals/buffalo.png')

animal_data = read_csv('guyton2020/data/Mimosa_RRA_FOO.csv') %>%
  janitor::clean_names() %>%
  filter(season=='early dry')

# Add in NA values for missing years of some animals. 
# This will ensure the spacing of the bar graph columns are correct.
animal_data = animal_data %>%
  complete(year, species)

animal_order = c('Warthog','Waterbuck','Reedbuck','Impala','Oribi','Buffalo')
animal_data$species = factor(animal_data$species, levels = animal_order, ordered = TRUE)

#               2013,     2015,     2016,       2017,  2018
year_colors = c('yellow2','orange2','palegreen4','tan3','steelblue4')
animal_data$year = as.factor(animal_data$year)


barplot_a = ggplot(animal_data, aes(x=species, y=mean_mimosa_rra, fill=as.factor(year))) + 
  geom_col(position = position_dodge(width=1), color='black') +
  geom_text(aes(y=mean_mimosa_rra + sem_mimosa_rra + 0.05, label=number_of_samples),
            position = position_dodge(width=1)) + # note sure why geom_text needs the width set in position dodge while geom_col and geom_errobar do not
  geom_errorbar(aes(ymin = mean_mimosa_rra - sem_mimosa_rra, ymax=mean_mimosa_rra + sem_mimosa_rra),
                width=0.5,position = position_dodge(width=1)) +
  scale_fill_manual(values=year_colors) +
  ylim(0, 0.9) + 
  labs(x='LMH',y='M. pigra relative sequence read abundance per sample') + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x.bottom = element_line(size=0.5),
        axis.line.y.left = element_line(size=0.5),
        axis.text = element_text(face='bold'),
        panel.background  = element_blank(),
        panel.border = element_blank(),
        legend.position = c(0.08,0.75)) +
  guides(fill = guide_legend(title = 'Year'))

barplot_a_with_animals = ggdraw() + 
  draw_plot(barplot_a, height=0.95) + 
  draw_image(buffalo, x=0.82, y=0.65, width=0.08, height=0.08) + # buffalo
  draw_image(buffalo, x=0.71, y=0.65, width=0.08, height=0.08) + # oribi
  draw_image(buffalo, x=0.55, y=0.8, width=0.08, height=0.08) + # impala
  draw_image(buffalo, x=0.45, y=0.72, width=0.08, height=0.08) + # reedbuck
  draw_image(buffalo, x=0.25, y=0.62, width=0.08, height=0.08) + # waterbuck
  draw_image(buffalo, x=0.12, y=0.25, width=0.08, height=0.08)   # warthog


barplot_b = ggplot(animal_data, aes(x=species, y=mimosa_foo, fill=as.factor(year))) + 
  geom_col(position = position_dodge(), color='black') +
  scale_fill_manual(values=year_colors) +
  ylim(0, 1) + 
  labs(x='LMH',y='Proportional occurrence of M. pigra accross samples') + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x.bottom = element_line(size=0.5),
        axis.line.y.left = element_line(size=0.5),
        axis.text = element_text(face='bold'),
        panel.background  = element_blank(),
        panel.border = element_blank(),
        legend.position = 'none')
 
barplot_b_with_animals =ggdraw() +
  draw_plot(barplot_b, height=0.95) + 
  draw_image(buffalo, x=0.82, y=0.88, width=0.08, height=0.08) + # buffalo
  draw_image(buffalo, x=0.71, y=0.9, width=0.08, height=0.08) + # oribi
  draw_image(buffalo, x=0.58, y=0.9, width=0.08, height=0.08) + # impala
  draw_image(buffalo, x=0.42, y=0.85, width=0.08, height=0.08) + # reedbuck
  draw_image(buffalo, x=0.23, y=0.88, width=0.08, height=0.08) + # waterbuck
  draw_image(buffalo, x=0.1, y=0.5, width=0.08, height=0.08)   # warthog


plot_grid(barplot_a_with_animals, barplot_b_with_animals)
