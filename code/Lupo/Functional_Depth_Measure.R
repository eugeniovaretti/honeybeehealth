#Lupo functional depth measures

source("code/utils/read_statedata.R")
source("code/utils/read_rawstatedata.R")

library(weathermetrics)
library(roahd)
library(dplyr)
readstate <- read_statedata()
stateraw <- read_rawstatedata()

#write.csv(readstate, "data/new_data/temp_prec_trimesters.csv", row.names=FALSE)

df_final <- read.csv("data/new_data/data_bystate_temp_perc.csv")

#from readstate dataset: there are more states rather than data_bystate_temp_perc
#note that 'texas' has wrong values of precipitation, while 'Alaska' has missing
#values in pdsi

#while df_final has added information for 'Hawaii' and 'Other States' with respect
#to the states in readstate

groups <- levels(factor(readstate$state))
prec_smooth <- readstate[,c(1,2,6,8)]
prec_smooth_new <- prec_smooth %>% tidyr::pivot_wider(
  names_from = state,
  values_from = ` Precipitation`,
  values_fill = 0
)
data_Fp <- as.matrix(prec_smooth_new[,-c(1,2,44)])
#which(is.na(data_Fp))
#data_Fp[is.na(data_Fp)]<-0

grid <- seq(1,30)
matplot(grid,data_Fp, type="l", col=adjustcolor(col=1,alpha.f = .4))

f_data <- fData(grid,t(data_Fp))
plot(f_data, main="Precipitations") 
plot(f_data[1:5,])

## Computing depth measures in a FDA framework

modified_band_depth <- MBD(Data = f_data)
median_curve <- median_fData(fData = f_data, type = "MBD")
plot(median_curve) #curve that has the highest value of MBD

#Bivariate data: precipitations and average temperature

#... code missing

data_Fp2 <- as.matrix(tempmin_smooth_new[,-c(1,2)]) #remove year and months column
f_data2 <- fData(grid,t(data_Fp2))

#bivariate_data <- as.mfData(list(f_data, f_data2))
#plot(bivariate_data)
#do.call(args = lapply(1:2, function(ind)
#  MHI(bivariate_data$fDList[[ind]])), what = "cor")


#remove the rows with NaN (state 'Hawaii' and 'Other States')
df_final <- na.omit(df_final)
which(is.na(df_final))

#check Spearman correlations

#colony loss
colony_loss <- df_final[,c(1,2,3,6)]
colony_loss_new <- colony_loss %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "colony_lost",
  values_fill = 0
)
data_Fp <- as.matrix(colony_loss_new[,-c(1,2)]) #remove year and months column
grid <- seq(1,29) #29 values instead of 30 because of missing values in 2019-Q2
f_data_loss <- fData(grid,t(data_Fp))

#mean temp
temp_smooth <- df_final[,c(1,2,3,21)]
tempmean_smooth_new <- temp_smooth %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Average.Temperature",
  values_fill = 0
)
data_Fp2 <- as.matrix(tempmean_smooth_new[,-c(1,2)]) #remove year and months column
grid <- seq(1,29) #29 values instead of 30 because of missing values in 2019-Q2
f_data_temp <- fData(grid,t(data_Fp2))

#colony_loss_new$texas = NULL
#tempmin_smooth_new %>% select(-texas)

colnames(tempmin_smooth_new)
colnames(colony_loss_new)
setdiff(colnames(tempmin_smooth_new), colnames(colony_loss_new))

#bivariate with temp mean and colony loss abs
bivariate_data <- as.mfData(list(f_data_loss, f_data_temp))
plot(bivariate_data)
do.call(args = lapply(1:2, function(ind)
  MHI(bivariate_data$fDList[[ind]])), what = "cor")
#--> Spearman Correlation: 0.019

#colony loss pct
colony_loss_pct <- df_final[,c(1,2,3,7)]
colony_loss_pct_new <- colony_loss_pct %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "colony_lost_pct",
  values_fill = 0
)
data_Fp3 <- as.matrix(colony_loss_pct_new[,-c(1,2)]) #remove year and months column
f_data_loss_pct <- fData(grid,t(data_Fp3))

#bivariate with temp mean and colony loss pct
bivariate_data <- as.mfData(list(f_data_loss_pct, f_data_temp))
plot(bivariate_data)
do.call(args = lapply(1:2, function(ind)
  MHI(bivariate_data$fDList[[ind]])), what = "cor")
#--> Spearman Correlation: 0.45

#max temp
tempmax_smooth <- df_final[,c(1,2,3,17)]
tempmax_smooth_new <- tempmax_smooth %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Maximum.Temperature",
  values_fill = 0
)
data_Fp4 <- as.matrix(tempmax_smooth_new[,-c(1,2)]) #remove year and months column
f_data_tempmax <- fData(grid,t(data_Fp4))

#min temp
tempmin_smooth <- df_final[,c(1,2,3,18)]
tempmin_smooth_new <- tempmin_smooth %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Minimum.Temperature",
  values_fill = 0
)
data_Fp5 <- as.matrix(tempmin_smooth_new[,-c(1,2)]) #remove year and months column
f_data_tempmin <- fData(grid,t(data_Fp5))

#check correlation between colony loss pct, mean temp, min temp, max temp
multivariate_data <- as.mfData(list(f_data_loss_pct, f_data_temp, f_data_tempmax, f_data_tempmin))
plot(multivariate_data)
cor_spearman(multivariate_data, ordering='MHI') 


#--> Spearman Correlation matrix:
#[,1]      [,2]      [,3]      [,4]
#[1,] 1.0000000 0.4561508 0.4903346 0.3972249
#[2,] 0.4561508 1.0000000 0.9632771 0.9811693
#[3,] 0.4903346 0.9632771 1.0000000 0.9003924
#[4,] 0.3972249 0.9811693 0.9003924 1.0000000


#check correlation between colony loss pct, mean temp, min temp, max temp, precipitation

#to add precipitation I need to remove 'texas' values

