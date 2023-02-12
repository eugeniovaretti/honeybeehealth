new_data <- read.csv('production_year.csv')
new_data<-new_data[,c(2,4,6,7)]
data <- read.csv('US_honey_dataset_updated.csv')
data<-data[796:1115,4]
library(tidyverse)
new_data$yield_per_colony_kg<-data
n=dim(new_data)[1]
new_data<-new_data%>%mutate(yield_per_colony_kg=(yield_per_colony_kg*2)

new_data %>% 
  write.csv("production_year.csv",row.names=F)

new_row = c(state = "Connecticut", average_price=3.20, year = 2015,
            yield_per_colony_kg=34)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "Connecticut", average_price=3.22, year = 2016,
            yield_per_colony_kg=34)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "Connecticut", average_price=3.20, year = 2017,
            yield_per_colony_kg=46)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "Connecticut", average_price=3.23, year = 2018,
            yield_per_colony_kg=46)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "Connecticut", average_price=3.23, year = 2019,
            yield_per_colony_kg=40)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "Connecticut", average_price=3.23, year = 2020,
            yield_per_colony_kg=40)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "Connecticut", average_price=3.25, year = 2021,
            yield_per_colony_kg=52)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "Connecticut", average_price=3.27, year = 2022,
            yield_per_colony_kg=52)
new_data[nrow(new_data) + 1,] <- new_row

new_row = c(state = "maryland", average_price=2.93, year = 2015,
            yield_per_colony_kg=84)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "maryland", average_price=2.93, year = 2016,
            yield_per_colony_kg=84)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "maryland", average_price=2.93, year = 2017,
            yield_per_colony_kg=92)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "maryland", average_price=2.97, year = 2018,
            yield_per_colony_kg=92)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "maryland", average_price=2.97, year = 2019,
            yield_per_colony_kg=92)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "maryland", average_price=2.97, year = 2020,
            yield_per_colony_kg=88)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "maryland", average_price=3.02, year = 2021,
            yield_per_colony_kg=88)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "maryland", average_price=3.05, year = 2022,
            yield_per_colony_kg=90)
new_data[nrow(new_data) + 1,] <- new_row


new_row = c(state = "massachusetts", average_price=3.45, year = 2015,
            yield_per_colony_kg=26)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "massachusetts", average_price=3.45, year = 2016,
            yield_per_colony_kg=30)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "massachusetts", average_price=3.45, year = 2017,
            yield_per_colony_kg=46)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "massachusetts", average_price=3.48, year = 2018,
            yield_per_colony_kg=40)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "massachusetts", average_price=3.69, year = 2019,
            yield_per_colony_kg=40)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "massachusetts", average_price=3.80, year = 2020,
            yield_per_colony_kg=45)
new_row = c(state = "massachusetts", average_price=3.80, year = 2021,
            yield_per_colony_kg=42)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "massachusetts", average_price=3.88, year = 2022,
            yield_per_colony_kg=46)
new_data[nrow(new_data) + 1,] <- new_row


new_row = c(state = "newmexico", average_price=4.40, year = 2015,
            yield_per_colony_kg=110)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "newmexico", average_price=4.40, year = 2016,
            yield_per_colony_kg=100)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "newmexico", average_price=4.42, year = 2017,
            yield_per_colony_kg=120)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "newmexico", average_price=4.45, year = 2018,
            yield_per_colony_kg=122)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "newmexico", average_price=4.48, year = 2019,
            yield_per_colony_kg=130)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "newmexico", average_price=4.56, year = 2020,
            yield_per_colony_kg=110)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "newmexico", average_price=4.56, year = 2021,
            yield_per_colony_kg=120)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "newmexico", average_price=4.59, year = 2022,
            yield_per_colony_kg=112)
new_data[nrow(new_data) + 1,] <- new_row


new_row = c(state = "oklahoma", average_price=4.56, year = 2015,
            yield_per_colony_kg=100)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "oklahoma", average_price=4.56, year = 2016,
            yield_per_colony_kg=90)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "oklahoma", average_price=4.58, year = 2017,
            yield_per_colony_kg=85)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "oklahoma", average_price=4.67, year = 2018,
            yield_per_colony_kg=98)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "oklahoma", average_price=4.67, year = 2019,
            yield_per_colony_kg=105)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "oklahoma", average_price=4.69, year = 2020,
            yield_per_colony_kg=100)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "oklahoma", average_price=4.87, year = 2021,
            yield_per_colony_kg=95)
new_data[nrow(new_data) + 1,] <- new_row
new_row = c(state = "oklahoma", average_price=4.89, year = 2022,
            yield_per_colony_kg=95)
new_data[nrow(new_data) + 1,] <- new_row

new_data <- new_data[-c(337,351),]
new_data <- new_data[order(new_data$state),]
