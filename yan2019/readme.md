## Paper
Yan, D., Scott, R. L., Moore, D. J. P., Biederman, J. A., & Smith, W. K. (2019). Understanding the relationship between vegetation greenness and productivity across dryland ecosystems through the integration of PhenoCam, satellite, and eddy covariance data. Remote Sensing of Environment, 223, 50–62. https://doi.org/10.1016/j.rse.2018.12.029

Figure 5

## Notes

This uses two base figures. One for the scatterplot and one for the barcharts. The scatter plot uses facet_wrap, but all the row, column, and axis titles are inserted manually using `cowplot::draw_plot() + geom_text()`. facet_wrap does not have a way to insert text like this outside the normal facet_wrap strip labels.

## Reproduced Figure
![](https://raw.githubusercontent.com/sdtaylor/complex_figure_examples/master/yan2019/yan2019_final.png)
