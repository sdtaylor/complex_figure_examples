## Paper
Guyton, J.A., Pansu, J., Hutchinson, M.C., Kartzinel, T.R., Potter, A.B., Coverdale, T.C., Daskin, J.H., da Conceição, A.G., Peel, M.J., Stalmans, M.E. and Pringle, R.M. (2020). Trophic rewilding revives biotic resistance to shrub invasion.  Nature Ecology & Evolution, 1-13. https://doi.org/10.1038/s41559-019-1068-y

Figure 3

## Notes

An important aspect here was filling in missing values for each animal so that spacing on the x-axis is done correctly. Placement of animal silhouettes required fine tuning of each one using x,y arguments in `cowplot::draw_image()`. Also note how the primary bar graphs have their `height` in `draw_plot()` adjusted down slightly to make room for the silhouettes. The animal silhouettes are not exact matches for each species.

## Reproduced Figure
![](https://raw.githubusercontent.com/sdtaylor/complex_figure_examples/master/guyton2020/guyton2020_final.png)
