#EXPLORATORY

library(explore)
library(dplyr) 

# Get the Data
colony <- read.csv("../data/colony.csv")
stressor <- read.csv("../data/stressor.csv")

new_stress <- stressor %>% tidyr::pivot_wider(
  names_from = stressor, 
  values_from = stress_pct,
  values_fill = 0
)
data <- inner_join(colony, new_stress, by = c("year","months","state"))
#salvare data in data_merged