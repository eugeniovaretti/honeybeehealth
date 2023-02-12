
df_prices <- read.csv("data/production_year.csv")

nums <- c("average_price", "year", "yield_per_colony_kg")
df_prices[ , nums] <- as.data.frame(apply(df_prices[ , nums], 2, as.numeric))
new_row = c(state = "massachussets", average_price=3.70, year=2020, yield_per_colony_kg = 42)
df_prices = rbind(df_prices,new_row)
df_prices <- df_prices[c("year", "state", "yield_per_colony_kg", "average_price")]

df_prices$state <- tolower(df_prices$state)
df_prices <- df_prices %>% arrange(year, state)

write.csv(df_prices, "data/production_year_new.csv")
