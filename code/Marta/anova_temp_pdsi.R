data <- read.csv('final_data_bystate.csv')
n<-dim(data)[1]
for(i in 1:n){
  ifelse((data$months[i] == "Q2" | data$months[i] == "Q3" ), 
         data$months[i] <- "Summer",  data$months[i] <- "Winter")
}

indexw<-which(data$months=='Winter')
indexs<-which(data$months=='Summer')
summer<-data[indexs,]
winter<-data[indexw,]

cluster1<-df[which(df$values==1),1]
cluster2<-df[which(df$values==2),1]
cluster3<-df[which(df$values==3),1]

p_value <- numeric(8)
p_val_s<- numeric(8)
p_val_w<- numeric(8)
p_val_1_3_s<- numeric(8)
p_val_2_3_s<- numeric(8)
p_val_1_2_s<- numeric(8)
p_val_1_3_w<- numeric(8)
p_val_2_3_w<- numeric(8)
p_val_1_2_w<- numeric(8)

x11()
par(mfrow=c(4,4))

year<-c("2015","2016","2017","2018","2019","2020","2021","2022")

for (j in 1:8){
  
  index<-which(data$year==year[j])
  
  d_2015<-data[index,]
  summer_2015<-summer[which(summer$year==year[j]),]
  winter_2015<-winter[which(winter$year==year[j]),]
  
  year_2015 <- d_2015 %>%
    group_by(state) %>%
    summarise(num_col_year = mean(colony_lost_pct, na.rm = T))
  
  s_2015<-summer_2015%>%
    group_by(state) %>%
    summarise(num_col_summer = mean(colony_lost_pct, na.rm = T))
  
  w_2015<-winter_2015%>%
    group_by(state) %>%
    summarise(num_col_winter = mean(colony_lost_pct, na.rm = T))
  
  year_2015$num_col_summer<-s_2015$num_col_summer
  year_2015$num_col_winter<-w_2015$num_col_winter
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
  
  #boxplot(num_col_year ~ state,main=paste('year',year[j]),col=rainbow(3),ylim=c(0,30))
  boxplot(num_col_summer ~ state,main=paste('summer',year[j]),col=rainbow(3),ylim=c(0,30))
  boxplot(num_col_winter ~ state,main=paste('winter',year[j]),col=rainbow(3),ylim=c(0,30))
  
  fit <- aov(num_col_year ~ state,data=year_2015)
  fits<- aov(num_col_summer~ state,data=year_2015)
  fitw<- aov(num_col_winter~ state,data=year_2015)
  
  T0 <- summary(fit)[[1]][1,4]
  Ts <- summary(fits)[[1]][1,4]
  Tw <- summary(fitw)[[1]][1,4]
  B = 1000
  seed = 26111992
  set.seed(seed)
  T_stat <- numeric(B) 
  T_stats <- numeric(B) 
  T_statw <- numeric(B) 
  
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:n)
    weight_perm <- num_col_year[permutation]
    fit_perm <- aov(weight_perm ~ state)
    # Test statistic:
    T_stat[perm] <- summary(fit_perm)[[1]][1,4]
  }
  
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:n)
    weight_perm <- num_col_summer[permutation]
    fit_perm <- aov(weight_perm ~ state)
    # Test statistic:
    T_stats[perm] <- summary(fit_perm)[[1]][1,4]
  }
  
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:n)
    weight_perm <- num_col_winter[permutation]
    fit_perm <- aov(weight_perm ~ state)
    # Test statistic:
    T_statw[perm] <- summary(fit_perm)[[1]][1,4]
  }
  
  #hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
  #abline(v=T0,col=3,lwd=2)
  #plot(ecdf(T_stat),xlim=c(-1,20))
  #abline(v=T0,col=3,lwd=4)
  p_value[j] <- sum(T_stat>=T0)/B
  p_val_s[j] <- sum(T_stats>=Ts)/B
  p_val_w[j] <- sum(T_statw>=Tw)/B

  #test H0: mu3=m1 vs h1:mu3<m1
  
  i1<- which(year_2015$state=='cluster1')
  i2<- which(year_2015$state=='cluster2')
  i3<- which(year_2015$state=='cluster3')
  n1 <- length(i1)
  n2 <- length(i2)
  n3 <- length(i3)
  n  <- n1+n2+n3
  
  x1<-year_2015[i1,]
  x2<-year_2015[i3,]
  
  t1.mean_s = mean(x1$num_col_summer)
  t2.mean_s = mean(x2$num_col_summer)
  T20_s = abs(t1.mean_s[1]-t2.mean_s[1])
  t1.mean_w = mean(x1$num_col_winter)
  t2.mean_w = mean(x2$num_col_winter)
  T20_w = abs(t1.mean_w[1]-t2.mean_w[1])
  
  B = 1000
  seed = 26111992
  n1 = dim(x1)[1]
  n2 = dim(x2)[1]
  n  = n1 + n2
  # Estimating the permutational distribution under H0
  T2_s = numeric(B)
  T2_w = numeric(B)
  set.seed(seed)
  library(progress)
  pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)
  for(perm in 1:B){
    # Random permutation of indexes
    # When we apply permutations in a multivariate case, we keep the units together
    # i.e., we only permute the rows of the data matrix
    t_pooled = rbind(x1,x2)
    permutation = sample(n)
    t_perm = t_pooled[permutation,]
    t1_perm = t_perm[1:n1,]
    t2_perm = t_perm[(n1+1):n,]
    
    # Evaluation of the test statistic on permuted data
    t1.mean_perm_s = colMeans(t1_perm[3])
    t2.mean_perm_s = colMeans(t2_perm[3]) 
    t1.mean_perm_w = colMeans(t1_perm[4])
    t2.mean_perm_w = colMeans(t2_perm[4])  
    
    T2_s[perm]  = abs(t1.mean_perm_s-t2.mean_perm_s)
    T2_w[perm]  = abs(t1.mean_perm_w-t2.mean_perm_w)
    
    pb$tick()
  }
  
  #hist(T2,xlim=range(c(T2,T20)),main=paste('H0: mu3=m1'))
  #abline(v=T20,col=3,lwd=4)
  
  #plot(ecdf(T2))
  #abline(v=T20,col=3,lwd=4)
  
  p_val_1_3_s[j] = sum(T2_s>=T20_s)/B 
  p_val_1_3_w[j] = sum(T2_w>=T20_w)/B  
  #0.169
  
  
  #test H0: mu3=m2 vs h1:mu3<m2
  x1<-year_2015[i2,]
  x2<-year_2015[i3,]
  
  t1.mean_s = mean(x1$num_col_summer)
  t2.mean_s = mean(x2$num_col_summer)
  T20_s = abs(t1.mean_s[1]-t2.mean_s[1])
  t1.mean_w = mean(x1$num_col_winter)
  t2.mean_w = mean(x2$num_col_winter)
  T20_w = abs(t1.mean_w[1]-t2.mean_w[1])
  
  B = 100
  seed = 26111992
  n1 = dim(x1)[1]
  n2 = dim(x2)[1]
  n  = n1 + n2
  # Estimating the permutational distribution under H0
  T2 = numeric(B)
  set.seed(seed)
  library(progress)
  pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)
  for(perm in 1:B){
    # Random permutation of indexes
    # When we apply permutations in a multivariate case, we keep the units together
    # i.e., we only permute the rows of the data matrix
    t_pooled = rbind(x1,x2)
    permutation = sample(n)
    t_perm = t_pooled[permutation,]
    t1_perm = t_perm[1:n1,]
    t2_perm = t_perm[(n1+1):n,]
    
    # Evaluation of the test statistic on permuted data
    t1.mean_perm_s = colMeans(t1_perm[3])
    t2.mean_perm_s = colMeans(t2_perm[3]) 
    t1.mean_perm_w = colMeans(t1_perm[4])
    t2.mean_perm_w = colMeans(t2_perm[4])  
    
    T2_s[perm]  = abs(t1.mean_perm_s-t2.mean_perm_s)
    T2_w[perm]  = abs(t1.mean_perm_w-t2.mean_perm_w)
    
    pb$tick()
  }
  
  #hist(T2,xlim=range(c(T2,T20)),main = paste('H0: mu3=m2'))
  #abline(v=T20,col=3,lwd=4)
  
  #plot(ecdf(T2))
  #abline(v=T20,col=3,lwd=4)
  
  p_val_2_3_s[j] = sum(T2_s>=T20_s)/B 
  p_val_2_3_w[j] = sum(T2_w>=T20_w)/B 
  
  #test H0: mu1=m2 vs h1:mu1<m2
  x1<-year_2015[i1,]
  x2<-year_2015[i2,]
  
  t1.mean_s = mean(x1$num_col_summer)
  t2.mean_s = mean(x2$num_col_summer)
  T20_s = abs(t1.mean_s[1]-t2.mean_s[1])
  t1.mean_w = mean(x1$num_col_winter)
  t2.mean_w = mean(x2$num_col_winter)
  T20_w = abs(t1.mean_w[1]-t2.mean_w[1])
  
  B = 100
  seed = 26111992
  n1 = dim(x1)[1]
  n2 = dim(x2)[1]
  n  = n1 + n2
  # Estimating the permutational distribution under H0
  T2 = numeric(B)
  set.seed(seed)
  library(progress)
  pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)
  for(perm in 1:B){
    # Random permutation of indexes
    # When we apply permutations in a multivariate case, we keep the units together
    # i.e., we only permute the rows of the data matrix
    t_pooled = rbind(x1,x2)
    permutation = sample(n)
    t_perm = t_pooled[permutation,]
    t1_perm = t_perm[1:n1,]
    t2_perm = t_perm[(n1+1):n,]
    
    # Evaluation of the test statistic on permuted data
    t1.mean_perm_s = colMeans(t1_perm[3])
    t2.mean_perm_s = colMeans(t2_perm[3]) 
    t1.mean_perm_w = colMeans(t1_perm[4])
    t2.mean_perm_w = colMeans(t2_perm[4])  
    
    T2_s[perm]  = abs(t1.mean_perm_s-t2.mean_perm_s)
    T2_w[perm]  = abs(t1.mean_perm_w-t2.mean_perm_w)
    
    pb$tick()
  }
  
  
#hist(T2,xlim=range(c(T2,T20)),main = paste('H0: mu1=m2'))
  #abline(v=T20,col=3,lwd=4)
  
  #plot(ecdf(T2))
  #abline(v=T20,col=3,lwd=4)
  
  p_val_1_2_s[j] = sum(T2_s>=T20_s)/B 
  p_val_1_2_w[j] = sum(T2_w>=T20_w)/B 
}

#0.086 0.511 0.118 0.049 0.806 0.187 0.044 0.001
#the type of cluster is not affecting the loss of bees

#in the 2015 there is statistical evidence to state that in cluster 2 the loss of bess was higher









