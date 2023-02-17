library(fda)
library(roahd)
library(tidyverse)
library(fdatest)

df_final <- read.csv("code/AnalysisOfFactors/Tests/data_bystate_temp_perc.csv")
df_final <- na.omit(df_final)
which(is.na(df_final))

#functional test to test the difference between summer and winter in colony lost

n <- dim(df_final)[1]

df_seasons = df_final

for(i in 1:n){
  ifelse((df_seasons$months[i] == "Q2" | df_seasons$months[i] == "Q3" ), 
         df_seasons$season[i] <- "Summer",  df_seasons$season[i] <- "Winter")
}

index_w<-which(df_seasons$season=='Winter')
index_s<-which(df_seasons$season=='Summer')
summer<-df_seasons[index_s,]
winter<-df_seasons[index_w,]

colony_lost_s <- summer[,c(1,2,3,6)]
colony_lost_w <- winter[,c(1,2,3,6)]

lost_s <- colony_lost_s %>% tidyr::pivot_wider(
  names_from = state,
  values_from = colony_lost,
  values_fill = NULL
)

lost_w <- colony_lost_w %>% tidyr::pivot_wider(
  names_from = state,
  values_from = colony_lost,
  values_fill = NULL
)

lost_s <- lost_s %>% unite(period, c("year", "months"))
lost_s <- lost_s %>% remove_rownames %>% column_to_rownames(var="period")
#rownames(lost_s)

lost_w <- lost_w %>% unite(period, c("year", "months"))
lost_w <- lost_w %>% remove_rownames %>% column_to_rownames(var="period")

n_summer <- dim(lost_s)[1]
n_winter <- dim(lost_w)[1]
grid_s <- 1:n_summer
grid_w <- 1:n_winter
f_summer <- fData(grid_s,t(lost_s))
plot(f_summer, col='red', main="summer") 
f_winter <- fData(grid_w,t(lost_w))
plot(f_winter, col='blue', main="winter") 

matplot(1:n_summer,lost_s, type='l',col='red')
matlines(1:n_summer,lost_w[1:n_winter-1,], type='l',col='blue')
#winter has one value more than summer because 2019-Q2 data is missing

lost_w2 <- lost_w[1:n_winter-1,]

#global test

seed <- 2781991
B <- 100000
data <- rbind(t(lost_s),t(lost_w2)) #global dataset
n <- nrow(data)
n_summer <- nrow(t(lost_s))
n_winter <- nrow(t(lost_w2))
meandiff <- (colMeans(t(lost_s))-colMeans(t(lost_w2)))
plot(meandiff,type = 'l') #pointwise difference
T0 <- sum(meandiff^2) #squared L2 distance between the two sample means (pointwise
#means for every column)
T0

T0_perm <- numeric(B)

for(perm in 1:B){
  permutation <- sample(n)
  data_perm <- data[permutation,]
  perm_summer <- data_perm[1:n_summer,] 
  perm_winter <- data_perm[(n_summer+1):n,] 
  T0_perm[perm] <- sum(((colMeans(perm_summer)-colMeans(perm_winter)))^2)
}

sum(T0_perm >= T0)/B
hist(T0_perm,xlim = c(0,max(T0_perm)+1000))
abline(v=T0,col='green')

#pvaue = 0.87 --> same distribution between summer and winter (??)

#I don't think this test really makes sense because the data is collected in
#different timestamps, summer distribution data is collected in summer quarters and
#winter distribution data in winter quarters

#Let's anyway try with a different test statistics

f_summer <- fData(grid_s,t(lost_s))
f_winter <- fData(grid_s,t(lost_w2))
meandiff=median_fData(f_summer,type='MBD')-median_fData(f_winter,type='MBD')
#pointwise difference between medians
plot(meandiff,type = 'l')
T0=(sum(abs(meandiff$values)))
T0 #observed test statistic

f_data <- append_fData(f_summer,f_winter) #join the two dataset

for(perm in 1:B){
  permutation <- sample(n)
  data_perm <- f_data[permutation,]
  perm_s <- data_perm[1:n_summer,] #subsetting rows on roahd object
  perm_w <- data_perm[(n_summer+1):n,] 
  meandiff <- median_fData(perm_s,type='MBD')-median_fData(perm_w,type='MBD')
  T0_perm[perm] <- sum(abs(meandiff$values))
}

sum(T0_perm >= T0)/B #pvalue 0.04798 --> reject H0 at 5% !!!!!!
hist(T0_perm,xlim = c(0,max(T0_perm)))
abline(v=T0,col='green')

#changing test statistics, with one robus to outliers, we change get the opposite
#result to the test


#let's try with the first test statistics for colony loss percentage values
colony_lost_pct_s <- summer[,c(1,2,3,7)]
colony_lost_pct_w <- winter[,c(1,2,3,7)]

lost_pct_s <- colony_lost_pct_s %>% tidyr::pivot_wider(
  names_from = state,
  values_from = colony_lost_pct,
  values_fill = 0
)

lost_pct_w <- colony_lost_pct_w %>% tidyr::pivot_wider(
  names_from = state,
  values_from = colony_lost_pct,
  values_fill = 0
)

lost_pct_s <- lost_pct_s %>% unite(period, c("year", "months"))
lost_pct_s <- lost_pct_s %>% remove_rownames %>% column_to_rownames(var="period")
#rownames(lost_pct_s)

lost_pct_w <- lost_pct_w %>% unite(period, c("year", "months"))
lost_pct_w <- lost_pct_w %>% remove_rownames %>% column_to_rownames(var="period")

n_summer <- dim(lost_pct_s)[1]
n_winter <- dim(lost_pct_w)[1]
grid_s <- 1:n_summer
grid_w <- 1:n_winter
f_pct_summer <- fData(grid_s,t(lost_pct_s))
plot(f_summer, col='red', main="summer") 
f_pct_winter <- fData(grid_w,t(lost_pct_w))
plot(f_winter, col='blue', main="winter") 

matplot(1:n_summer,lost_pct_s, type='l',col='red')
matlines(1:n_summer,lost_pct_w[1:n_winter-1,], type='l',col='blue')
#winter has one value more than summer because 2019-Q2 data is missing

lost_pct_w2 <- lost_pct_w[1:n_winter-1,]

seed <- 2781991
B <- 1000
data_pct <- rbind(t(lost_pct_s),t(lost_pct_w2)) #global dataset
n <- nrow(data_pct)
n_summer <- nrow(t(lost_pct_s))
n_winter <- nrow(t(lost_pct_w2))
meandiff <- (colMeans(t(lost_pct_s))-colMeans(t(lost_pct_w2)))
plot(meandiff,type = 'l') #pointwise difference
T0 <- sum(meandiff^2) #squared L2 distance between the two sample means (pointwise
#means for every column)
T0

T0_perm <- numeric(B)

for(perm in 1:B){
  permutation <- sample(n)
  data_perm <- data_pct[permutation,]
  perm_summer <- data_perm[1:n_summer,] 
  perm_winter <- data_perm[(n_summer+1):n,] 
  T0_perm[perm] <- sum(((colMeans(perm_summer)-colMeans(perm_winter)))^2)
}

sum(T0_perm >= T0)/B
hist(T0_perm,xlim = c(0,max(T0_perm)))
abline(v=T0,col='green')

#this test statistics work for colony loss pct !!! pvalue = 0 --> different
#distribution between summer and winter

#Local permutational inference for functional data

#Now, what I did is basically testing the hypothesis globally, I am rejecting
#if, for at least one time instant t the two curves are statistically different.
#How do I tell what is that specific time instant? I use a procedure called
#Interval-wise Testing.

tst <- IWT2(t(lost_pct_s),t(lost_pct_w2))
plot(tst)

#level of shades is the level of significance in that part of the domain (in a 
#nonparametric fashion) --> to understand which sections affect the rejection
#of H0 in the global test.

#This technique allows you to perform a two sample t-test AND to impute a
#rejection of the null to some parts of the domain. (shadings represent
#significance values, dark grey is 1%, light is 5%).

domain_indices <- which(tst$adjusted_pval < 0.05)
years <- c("2015-Q1Q2", "2015-Q3Q4", "2016-Q1Q2", "2016-Q3Q4", "2017-Q1Q2", "2017-Q3Q4",
           "2018-Q1Q2", "2018-Q3Q4", "2019-Q1Q3", "2019-Q4-2020-Q2", "2020-Q1Q3",
           "2020-Q4-2021-Q2", "2021-Q1Q3", "2021-Q4-2022-Q2")
years[domain_indices] # --> "2015-Q1Q2" "2016-Q1Q2" "2017-Q1Q2" "2018-Q1Q2"
#"2021-Q1Q3" "2021-Q4-2022-Q2" --> these are the regions where the distributions are different


#possible improvement for functional tests: fill the missing values in 2019-Q2
#with a SARIMA model

#----------------------------------------------------------------------------

#test temperature max e varroa

grid <- seq(1,29)

varroa <- df_final[,c(1,2,3,11)]
varroa_pivot <- varroa %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Varroa.mites",
  values_fill = 0
)
varroa_matrix <- as.matrix(varroa_pivot[,-c(1,2)]) #remove year and months column
f_varroa <- fData(grid,t(varroa_matrix))

temp_max <- df_final[,c(1,2,3,17)]
temp_max_pivot <- temp_max %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "MaximumTemperature",
  values_fill = 0
)
temp_max_matrix <- as.matrix(temp_max_pivot[,-c(1,2)]) #remove year and months column
f_temp_max <- fData(grid,t(temp_max_matrix))

#global test

seed <- 2781991
B <- 1000
data <- rbind(t(varroa_matrix),t(temp_max_matrix)) #global dataset
n <- nrow(data)
n_varroa <- nrow(t(varroa_matrix))
n_temp_max <- nrow(t(temp_max_matrix))
meandiff <- (colMeans(t(varroa_matrix))-colMeans(t(temp_max_matrix)))
plot(meandiff,type = 'l') #pointwise difference
T0 <- sum(meandiff^2) #squared L2 distance between the two sample means (pointwise
#means for every column)
T0

T0_perm <- numeric(B)

for(perm in 1:B){
  permutation <- sample(n)
  data_perm <- data[permutation,]
  perm_varroa <- data_perm[1:n_varroa,] 
  perm_temp_max <- data_perm[(n_temp_max+1):n,] 
  T0_perm[perm] <- sum(((colMeans(perm_varroa)-colMeans(perm_temp_max)))^2)
}

sum(T0_perm >= T0)/B
hist(T0_perm,xlim = c(0,max(T0_perm)))
abline(v=T0,col='green')
#pvalue = 0 --> difference in distribution

#local permutation test

periods <- c("2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4", "2016-Q1", "2016-Q2",
             "2016-Q3", "2016-Q4","2017-Q1", "2017-Q2","2017-Q3", "2017-Q4",
             "2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4","2019-Q1", "2019-Q3",
             "2019-Q4", "2020-Q1","2020-Q2", "2020-Q3","2020-Q4", "2021-Q1",
             "2021-Q2", "2021-Q3", "2021-Q4", "2022-Q1", "2022-Q2")
tst <- IWT2(t(varroa_matrix),t(temp_max_matrix))
plot(tst)
domain_indices <- which(tst$adjusted_pval < 0.05)
periods[domain_indices]

#local permutation tests between varroa e temp max normalized

temp_smooth <- df_final[,c(1,2,3,21)]
tempmean_smooth_new <- temp_smooth %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "AverageTemperature",
  values_fill = 0
)
data_Fp2 <- as.matrix(tempmean_smooth_new[,-c(1,2)]) #remove year and months column
f_data_temp <- fData(grid,t(data_Fp2))

tempmax_n <- t(t(temp_max_matrix)/colSums(temp_max_matrix))
varroa_n <- t(t(varroa_matrix)/colSums(varroa_matrix))

tst2 <- IWT2(t(varroa_n),t(tempmax_n))
plot(tst2)
domain_indices2 <- which(tst2$adjusted_pval < 0.05)
periods[domain_indices2]

#no relevant results!!

#--------------------------------------------------------------------

