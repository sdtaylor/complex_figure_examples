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

ggplot(animal_data, aes(x=species, y=mean_mimosa_rra, fill=as.factor(year))) + 
  geom_col(position = position_dodge(), color='black') +
  geom_errorbar(aes(ymin = mean_mimosa_rra - sem_mimosa_rra, ymax=mean_mimosa_rra + sem_mimosa_rra),
                position = position_dodge()) +
  scale_fill_manual(values=year_colors) +
  labs(x='LMH',y='M. pigra relative sequence read abundance per sample') + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x.bottom = element_line(size=0.5),
        axis.line.y.left = element_line(size=0.5),
        panel.background  = element_blank(),
        panel.border = element_blank(),
        legend.position = c(0.15,0.85)) +
  guides(fill = guide_legend(title = 'Year'))
