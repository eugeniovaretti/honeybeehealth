colony <- read.csv("colony.csv")
stressor <- read.csv("stressor.csv")
library(explore)
library(dplyr) 

new_stress <- stressor %>% tidyr::pivot_wider(
  names_from = stressor, 
  values_from = stress_pct,
  values_fill = 0
)

data <- inner_join(colony, new_stress, by = c("year","months","state"))

data %>% 
  write.csv("data_merged.csv")

explore(data)
