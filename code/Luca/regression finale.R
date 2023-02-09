data <- read_csv("data/new_data/data_bystate_temp_perc.csv")

library(dplyr)

data <- data %>% mutate(
  period = case_when(
    months=='Q1' ~ paste('winter',year,sep='-'),
    months=='Q4' ~ paste('winter',year+1,sep='-'),
    months=='Q2' ~ paste('summer',year,sep='-'),
    months=='Q3' ~ paste('summer',year,sep='-')
  ))


#Analysis of new mexico
#data_sel <- data %>% filter(grepl("winter",period) )
#data_sel <- data %>% filter(state == "california")
data_sel <- data %>% filter(state == "new mexico")

# Compute cross-correlation

r = ccf(data_sel$colony_lost_pct,data_sel$Varroa.mites, lag.max = 4)
r2 = ccf(data_sel$Varroa.mites,data_sel$colony_lost_pct, lag.max = 4)

# sostituisco valore mancante con NAN
mean = mean((data_sel %>% filter(months == "Q2"))$colony_lost_pct, na.rm = T)
data_sel <- data_sel %>% add_row(year = 2019, months = "Q2", state="new mexico", colony_lost_pct=mean)

data_sel <- arrange(data_sel, year,months)

myts <- ts(data_sel$colony_lost_pct, frequency=4, start=c(2015, 1), end=c(2022, 2))
myds_month <- decompose(myts)
plot(myds_month)

library(forecast)
my_df_ts <- data.frame(colony_lost_pct = myts, time=as.numeric(time(myts)), Varroa.mites = data_sel$Varroa.mites)
#names(my_df_ts) <- c("colony_lost_pct", "time")
mymodel <- tslm(colony_lost_pct~season+trend+Varroa.mites,my_df_ts)
summary(mymodel)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

mod <- lm(data=data_sel, colony_lost_pct ~ months + months*Varroa.mites + months*Disesases + months*Pesticides + months*Other+ Unknown*months+PalmerDroughtSeverityIndexPDSI+Precipitation +AverageTemperature)
summary(mod)
vif(mod)

library(mgcv)
mod_gam = gam(colony_lost_pct ~ year + months + s(Varroa.mites,bs='tp') +Other.pests.parasites+Disesases+Pesticides+Other+Unknown + state, data = data)
summary(mod_gam)
