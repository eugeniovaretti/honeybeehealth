#manova (devo mettere il - nella statsitica test?)
d <- read.csv('temp_prec_trimesters.csv')
data <- read.csv('final_data_bystate.csv')
n<-dim(data)[1]
for(i in 1:n){
  ifelse((data$months[i] == "Q2" | data$months[i] == "Q3" ), 
         data$months[i] <- "Summer",  data$months[i] <- "Winter")
}

#indexw<-which(data$months=='Winter')
#indexs<-which(data$months=='Summer')
#summer<-data[indexs,]
#winter<-data[indexw,]

cluster1<-df[which(df$values==1),1]
cluster2<-df[which(df$values==2),1]
cluster3<-df[which(df$values==3),1]

p_value <- numeric(8)
#p_val_s<- numeric(8)
#p_val_w<- numeric(8)
year<-c("2015","2016","2017","2018","2019","2020","2021","2022")
x11()
par(mfrow=c(4,4))
for (j in 1:8){
  
  
  index<-which(data$year==year[j])
  
  d_2015<-data[index,]
  #summer_2015<-summer[which(summer$year==year[j]),]
  #winter_2015<-winter[which(winter$year==year[j]),]
  
  year_2015 <- d_2015 %>%
    group_by(state) %>%
    summarise(lost_pct=mean(colony_lost_pct, na.rm = T),
              varroa = mean(Varroa.mites, na.rm = T),
              pests = mean(Other.pests.parasites, na.rm = T),
              disease = mean(Disesases, na.rm = T),
              pesticide = mean(Pesticides, na.rm = T),
              other = mean(Other, na.rm = T),
              ukn = mean(Unknown, na.rm = T))
  
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
  
  for(i in 1:n){
    if(year_2015$state[i]%in%cluster1){year_2015$state[i]<-"cluster1"
    
    }else if (year_2015$state[i]%in%cluster2) {year_2015$state[i]<-"cluster2"
    
    }else if (year_2015$state[i]%in%cluster3) {year_2015$state[i]<-"cluster3"
    
    }
  }
  
  attach(year_2015)
  g<-nlevels(as.factor(state))
  n<-dim(year_2015)[1]
  
  species.name <- factor(state, labels=c('cluster1','cluster2','cluster3'))
  y_2015        <- year_2015[,2:8]
  stressor<-year_2015[,3:8]
  
  plot(y_2015,col=species.name)
  boxplot(varroa ~ state,main=paste('varroa',year[j]),col=rainbow(3),ylim=c(0,50))
  #boxplot(pesticide ~ state,main=paste('pesticide',year[j]),col=rainbow(3),ylim=c(0,50))
  #boxplot(disease ~ state,main=paste('disease',year[j]),col=rainbow(3),ylim=c(0,50))
  #boxplot(pests ~ state,main=paste('pests',year[j]),col=rainbow(3),ylim=c(0,50))
  #boxplot(other ~ state,main=paste('other',year[j]),col=rainbow(3),ylim=c(0,50))
  
  
  fit <- manova(as.matrix(stressor) ~ species.name)
  #fits<- aov(num_col_summer~ state,data=year_2015)
  #fitw<- aov(num_col_winter~ state,data=year_2015)
  
  T0 <- summary.manova(fit,test="Wilks")$stats[1,2]
  #Ts <- summary(fits)[[1]][1,4]
  #Tw <- summary(fitw)[[1]][1,4]
  B = 1000
  seed = 26111992
  T_stat <- numeric(B) 
  set.seed(seed)
  #T_stats <- numeric(B) 
  #T_statw <- numeric(B) 
  
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:n)
    species.name.perm <- species.name[permutation]
    fit_perm <-manova(as.matrix(y_2015) ~ species.name.perm)
    # Test statistic:
    T_stat[perm] <- summary.manova(fit_perm,test="Wilks")$stats[1,2]
  }
  
  #hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
  #abline(v=T0,col=3,lwd=2)
  #plot(ecdf(T_stat),xlim=c(-1,20))
  #abline(v=T0,col=3,lwd=4)
  p_value[j] <- sum(T_stat>=T0)/B
  #p_val_s[j] <- sum(T_stats>=T0)/B
  #p_val_w[j] <- sum(T_statw>=T0)/B
}
