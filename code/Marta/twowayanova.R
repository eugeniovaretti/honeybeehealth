data <- read.csv('data_bystate_temp_perc.csv')
n<-dim(data)[1]
year<-c("2015","2016","2017","2018","2019","2020","2021","2022")
x11()
par(mfrow=c(4,4))
for (j in 1:8){
  
  
  index<-which(data$year==year[1])
  
  d_2015<-data[index,]
  #summer_2015<-summer[which(summer$year==year[j]),]
  #winter_2015<-winter[which(winter$year==year[j]),]
  
  year_2015 <- d_2015 %>%
    group_by(state) %>%
    summarise(lost_pct=mean(colony_lost_pct, na.rm = T),
              temp = mean(Average.Temperature, na.rm = T),
              prec = mean(Precipitation, na.rm = T),
               )
  
  #s_2015<-summer_2015%>%
  # group_by(state) %>%
  #summarise(num_col_summer = mean(colony_lost_pct, na.rm = T))
  
  #w_2015<-winter_2015%>%
  # group_by(state) %>%
  #summarise(num_col_winter = mean(colony_lost_pct, na.rm = T))
  
  #year_2015$num_col_summer<-s_2015$num_col_summer
  #year_2015$num_col_winter<-w_2015$num_col_winter
  states.name <- factor(year_2015$state)
  i1<-which(states.name==c("other states"))
  year_2015<-year_2015[-i1,]
  i2<-which(states.name==c("hawaii"))
  year_2015<-year_2015[-i2,]
  n<-dim(year_2015)[1]
  
  attach(year_2015)
  T0_interaction<-summary.aov(aov(lost_pct ~ temp + prec + temp:prec))[[1]][3,4]
  aov.H0 <- aov(lost_pct ~ temp + prec)
  
  residuals.H0 <- aov.H0$residuals
  B = 1000
  seed = 26111992
  set.seed(seed)
  
  T_inter <- numeric(B)
  for(perm in 1:B){
    permutation <- sample(n)
    residuals.H0 <- residuals.H0[permutation]
    km.perm.H0station_fuel <- aov.H0$fitted + residuals.H0
    T_inter[perm] <- summary.aov(aov(km.perm.H0station_fuel ~ temp + prec + temp:prec))[[1]][3,4]
  }
  
  sum(T_inter >= T0_interaction)/B

  #the interaction is not significant
  T0_prec <- summary.aov(aov(lost_pct ~ temp + prec))[[1]][1,4]
  # residuals under H0: prec=0
  aov.H0station <- aov(lost_pct ~ temp)
  residuals.H0station <- aov.H0station$residuals
  
  # TEST OF FACTOR FUEL   (H0: beta=0)
  T0_temp <- summary.aov(aov(lost_pct ~ temp + prec))[[1]][2,4]
  # residuals under H0: temp=0
  # km = mu + alpha*station
  aov.H0fuel <- aov(lost_pct ~ prec)
  residuals.H0fuel <- aov.H0fuel$residuals
  
  B <- 1000
  T_temp <- T_prec <- numeric(B)
  for(perm in 1:B){
    permutation <- sample(n)
    
    km.perm.H0station <- aov.H0station$fitted + residuals.H0station[permutation]
    T_temp[perm] <- summary.aov(aov(km.perm.H0station ~ temp + prec))[[1]][1,4]
    
    km.perm.H0fuel <- aov.H0fuel$fitted + residuals.H0fuel[permutation]
    T_prec[perm] <- summary.aov(aov(km.perm.H0fuel ~ temp + prec))[[1]][2,4]
  }
  
  sum(T_temp >= T0_temp)/B
  #0.912
  sum(T_prec >= T0_prec)/B
  #0.037 temp is the only significant variable
  