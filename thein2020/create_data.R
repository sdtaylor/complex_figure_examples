library(tidyverse)


#---------------------------------------
# Here we create a fake dataset similar to the Thein 2020 figure.
# It will have the following form:
#      elev       season depot_encounter removal_from_depot total_removal species_removed
# 0.7965260   Wet Season        19.15570           37.21124      19.50298        19.24666
# 1.1163717   Wet Season        18.09308           33.13282      17.06157        16.98486
# 1.7185601   Wet Season        17.57028           36.90911      18.87783        17.99193
# .......
# .......
#---------------------------------------

set.seed(1)

upward_curve = function(x, height, noise=2){
  curve = 3*x*x - 9*x + height
  curve = curve + rnorm(length(curve), mean=0, sd=noise)   # add some noise
  curve = pmax(curve, 0)  # but keep it above 0
  return(curve)
}
downward_curve = function(x, height, noise=2){
  curve = -1.5*x*x + 5*x + height
  curve = curve + rnorm(length(curve), mean=0, sd=noise)   # add some noise
  curve = pmax(curve, 0)  # but keep it above 0
  return(curve)
}


wet_season_n = 25
dry_season_n = 25

wet_season = data.frame(elev = runif(wet_season_n, 0.5,3))
wet_season$season = 'Wet Season'
wet_season$depot_encounter = upward_curve(wet_season$elev, height=25)
wet_season$removal_from_depot = upward_curve(wet_season$elev, height=40)
wet_season$total_removal = upward_curve(wet_season$elev, height=25)
wet_season$species_removed = upward_curve(wet_season$elev, height=22)

dry_season = data.frame(elev = runif(dry_season_n, 0.5,3))
dry_season$season = 'Dry Season'
dry_season$depot_encounter = downward_curve(dry_season$elev, height=1)
dry_season$removal_from_depot = downward_curve(dry_season$elev, height=22)
dry_season$total_removal = downward_curve(dry_season$elev, height=1)
dry_season$species_removed = downward_curve(dry_season$elev, height=5)


both_seasons = wet_season %>%
  bind_rows(dry_season)

write_csv(both_seasons, 'thein2020/simulated_data.csv')

# both_seasons %>%
#   pivot_longer(c(-elev, -season), names_to='metric', values_to='metric_value') %>%
#   ggplot(aes(x=elev, y=metric_value, color=season)) + 
#   geom_point() + 
#   geom_smooth(method='lm', formula = y~poly(x,2)) + 
#   facet_wrap(~metric, ncol=2, scales='free')
