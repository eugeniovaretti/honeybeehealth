#understand if the mean loss of colony during summer is significantly different from the one in the winter
#being Y(i_summer)∼iid Y(summer) and Y(i_winter)∼iid Y(winter), we want to test the equality of the two distributions

##  H0:Y(summer)=Y(winter) vs H1:Y(summer)≠Y(winter)
library(progress)
data <- read.csv('final_data_bystate.csv')
n<-dim(data)[1]
for(i in 1:n){
  ifelse((data$months[i] == "Q2" | data$months[i] == "Q3" ), 
         data$months[i] <- "Summer",  data$months[i] <- "Winter")
}

p_value <- numeric(8)
x11()
par(mfrow=c(2,4))
year<-c("2015","2016","2017","2018","2019","2020","2021","2022")
for (j in 1:8){
  
i2<-which(data$months=="Summer"&data$year==year[j])
i3<-which(data$months=="Winter"&data$year==year[j])
t1<-data[i2,8]  #summer population in 2015
t2<-data[i3,8]  #winter population in 2015

#paired population
delta.0 <- 0
diff <- t1-t2
diff.mean <- mean(diff)
diff.cov <- var(diff)

#euclidean distance between the difference in means and the hypothesised value
T20 <- abs(diff.mean-delta.0) 
#Mahalanobis distance, but "forgetting" about the covariance between the values
#T20 <- as.numeric( (diff.mean-delta.0) %*% solve(diag(diag(diff.cov))) %*% (diff.mean-delta.0))

B = 100000
seed = 26111992
n1 <- length(t1)[1]
n2 <- length(t2)[1]
n <- n1+n2
T2 <- numeric(B)
set.seed(seed)
for(perm in 1:B){
  # Random permutation
  # obs: exchanging data within couples means changing the sign of the difference
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=1,byrow=FALSE)
  diff.mean_perm <- mean(diff_perm)
  T2[perm] <- abs(diff.mean_perm-delta.0)
}
# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

#plot(ecdf(T2))
#abline(v=T20,col=3,lwd=4)

# p-value
p_val[j] <- sum(T2>=T20)/B
}
#[1] 0.00010 0.00661 0.00001 0.00001 0.10453 0.52872
# 0.00010 0.00229










#p_val=1e-04

i2<-which(df_season$season=="Summer"&df_season$year_revisited=="2016")
i3<-which(df_season$season=="Winter"&df_season$year_revisited=="2016")
t1<-df_season[i2,4]  #summer population in 2016
t2<-df_season[i3,4]

#p_val=0.00661

i2<-which(df_season$season=="Summer"&df_season$year_revisited=="2017")
i3<-which(df_season$season=="Winter"&df_season$year_revisited=="2017")
t1<-df_season[i2,4]  #summer population in 2017
t2<-df_season[i3,4]

#p_val=1e-05

i2<-which(df_season$season=="Summer"&df_season$year_revisited=="2018")
i3<-which(df_season$season=="Winter"&df_season$year_revisited=="2018")
t1<-df_season[i2,4]  #summer population in 2018
t2<-df_season[i3,4]

#p_val=1e-05

i2<-which(df_season$season=="Summer"&df_season$year_revisited=="2019")
i3<-which(df_season$season=="Winter"&df_season$year_revisited=="2019")
t1<-df_season[i2,4]  #summer population in 2019
t2<-df_season[i3,4]

#p_val=0.10453

i2<-which(df_season$season=="Summer"&df_season$year_revisited=="2020")
i3<-which(df_season$season=="Winter"&df_season$year_revisited=="2020")
t1<-df_season[i2,4]  #summer population in 2020
t2<-df_season[i3,4]

#p_val=0.52872

i2<-which(df_season$season=="Summer"&df_season$year_revisited=="2021")
i3<-which(df_season$season=="Winter"&df_season$year_revisited=="2021")
t1<-df_season[i2,4]  #summer population in 2021
t2<-df_season[i3,4]

#p_val=1e-04

i2<-which(df_season$season=="Summer"&df_season$year_revisited=="2022")
i3<-which(df_season$season=="Winter"&df_season$year_revisited=="2022")
t1<-df_season[i2,4]  #summer population in 2022
t2<-df_season[i3,4]

#p_val=0.00229



