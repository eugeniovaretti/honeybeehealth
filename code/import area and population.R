# import data of population and area

# install.packages("usa")
library(usa)
library(readr)
library(dplyr)

res <- data.frame(area = state.area)
rownames(res) = state.name
us_data = merge(res, state.x19, by = 'row.names', all = TRUE)
us_data = us_data %>% rename(state=Row.names)
us_data = us_data %>% 
  mutate(state = tolower(state)) 


state_coords_lon_lat <- read_csv("data/state_coords_lon_lat.csv")
final = merge(us_data, state_coords_lon_lat, by = 'state', all = FALSE)
final = final %>% select(state, area, population, lon, lat)
# library(tidycensus)
# census_api_key("c5ef8b2731ebf30b0d05c627514f073cf2f01ed1")
# pop_data <- get_acs(
#   geography = "state",
#   year = 2015:2021,
#   #table = "P1",
#   variables = "B01003_001", #"P0010001",
#   #summary_var = "P0010001",
#   region = "US"
# )