library(tidyverse)
library(sf)
library(cowplot)

# The data is supplied as a raster. Which is converted to an sf polygon object
# by way of a SpatialPolygonDataframe.
# The reason for this is the sf package has great support for plotting maps in different
# projections. 
# The phenoregion raster is from the paper. downloaded from https://data.mendeley.com/datasets/k35ry274gv/1
phenoregion_raster = raster::raster('./dannenberg2020/data/world_phenoregions_L1.tif')
phenoregion_polygon = raster::rasterToPolygons(phenoregion_raster)
phenoregions = st_as_sf(phenoregion_polygon) %>%
  rename(phenoregion = world_phenoregions_L1) %>%
  group_by(phenoregion) %>%
  summarise() %>%
  ungroup()

# The colors here are done with 4 base colors and 3 levels of "adding" more white to them.
# This can be done by adjusting the transparency in ggplot thru the alpha argument.
# This method of representing 2 variables is well researched see:
# Kaye et al. 2012. Mapping the climate: Guidance on appropriate techniques to map climate variables and their uncertainty. 
# Geoscientific Model Development, 5(1), 245–256. https://doi.org/10.5194/gmd-5-245-2012

# #483248 purple
# #33b69b bluish
# #e29601 gold
# #eb6b51 red
phenoregion_colors = tribble(
  ~phenoregion, ~color,   ~alpha, ~seasonality, ~productivity,
  1,            '#eb6b51', 0.5,  3,            1,
  2,            '#e29601', 0.5,  3,            2,
  3,            '#33b69b', 0.5,  3,            3,
  4,            '#483248', 0.5,  3,            4,
  5,            '#eb6b51', 0.75,   2,            1,
  6,            '#e29601', 0.75,   2,            2,
  7,            '#33b69b', 0.75,   2,            3,
  8,            '#483248', 0.75,   2,            4,
  9,            '#eb6b51', 1.0,   1,            1,
 10,            '#e29601', 1.0,   1,            2,
 11,            '#33b69b', 1.0,   1,            3,
 12,            '#483248', 1.0,   1,            4
)

#################################################################################
# Setup the legend.
# The style of legend can't be done automatically in ggplot. But it can be done
# manually be creating a 2nd smaller plot which is a 3x4 grid.

base_legend = ggplot(phenoregion_colors, aes(x=productivity, y=seasonality, 
                               fill=as.factor(phenoregion), alpha=as.factor(phenoregion))) +
  geom_tile(colour='black', size=1) +
  geom_text(aes(label=phenoregion), size=5, alpha=1, color='black') + 
  scale_fill_manual(values = phenoregion_colors$color) + 
  scale_alpha_manual(values = phenoregion_colors$alpha) +
  theme_nothing() +
  labs(title = 'Phenoregion')+
  theme(plot.title = element_text(hjust=0.5, vjust=0, face = 'bold', size=15))

full_legend= ggdraw() + 
  draw_plot(base_legend, scale=0.6, hjust = -0.1, vjust=-0.1) +
  draw_text('Productivity', x = 0.5, y=0.1, size=10) + 
  draw_line(x=c(0.4, 0.8), y=c(0.25,0.25), size=1, arrow = arrow()) +
  draw_text('Seasonality',  x = 0.12, y=0.35, size=10) +
  draw_line(x=c(0.27, 0.27), y=c(0.35,0.75), size=1, arrow = arrow())

#countries <- rnaturalearth::ne_countries(returnclass = "sf")

############################################
# Pie chart
# Getting the order and the labels on this pie chart was a huge pain.
# Pie charts also have well studied problems. See https://stats.stackexchange.com/questions/8974/problems-with-pie-charts/
# If you're thinking of doing something similar please consider just a barchart.

# The pie chart by default goes clockwise from numbers 1->12
# The original pie chart in the paper is not exactly that so it must be coerced manually.
pie_chart_order = data.frame(phenoregion = c(12,8,4,11,7,3,10,6,2,9,5,1))
pie_chart_order$pie_order = 1:nrow(pie_chart_order)

phenoregions$area = st_area(phenoregions)

pie_chart_data = phenoregions %>%
  as_tibble() %>%
  select(phenoregion, area) %>%
  mutate(area = as.numeric(round(area/sum(area),2))) %>%
  mutate(chart_label = paste0(round(area*100,0),'%')) %>%
  left_join(phenoregion_colors, by='phenoregion') %>%
  left_join(pie_chart_order, by='phenoregion') %>%
  arrange(pie_order)

# The phenoregion ordering with in the factor is now coerced to the order specified above in pie_chart_order
pie_chart_data$phenoregion = fct_inorder(factor(pie_chart_data$phenoregion))

# The labels around the pie chart can be done by labelling specific spots
# on the y axis, which is then wrapped around. The exact locations of 
# each labely are confusingly calculated here...
pie_chart_data$label_y_pos = NA
pie_chart_data$label_y_pos[1] = pie_chart_data$area[1]/2
for(phenoregion_i in 2:nrow(pie_chart_data)){
  pie_chart_data$label_y_pos[phenoregion_i] = sum(pie_chart_data$area[1:(phenoregion_i-1)]) + (pie_chart_data$area[phenoregion_i]/2)
}
pie_chart_data$label_y_pos = 1-pie_chart_data$label_y_pos


pie_chart = ggplot(pie_chart_data, aes(x="", y=area, fill=phenoregion,alpha=phenoregion)) + 
  geom_bar(stat='identity', width=1, color='black') +
  scale_fill_manual(values=pie_chart_data$color) + 
  scale_alpha_manual(values = pie_chart_data$alpha) +
  scale_y_continuous(breaks = pie_chart_data$label_y_pos, labels=pie_chart_data$chart_label)  +
  #geom_text(aes(label=area), position = position_stack(vjust=0.5), hjust=1) +   
  coord_polar('y',start=0, direction=-1) +
  theme_nothing() + 
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size=6, hjust = 2),
        legend.position = 'none',
        legend.background = element_rect(color='black'),
        legend.title = element_blank())

#############################################
# The map

map = ggplot(phenoregions) +
  geom_sf(aes(fill=as.factor(phenoregion), alpha=as.factor(phenoregion)), color='transparent') +
  scale_fill_manual(values = phenoregion_colors$color) + 
  scale_alpha_manual(values = phenoregion_colors$alpha) +
  coord_sf(crs = "+proj=natearth +wktext") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.position = 'none')

#############################################
# Put it together.

ggdraw() +
  draw_plot(map, x=0,y=0, scale=0.8, hjust = 0.1) +
  draw_plot(pie_chart, scale=0.15, x=0.37, y=0.1) +
  draw_plot(full_legend, scale=0.25, x=0.37, y=-0.1)

