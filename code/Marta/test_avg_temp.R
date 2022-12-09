data <- read.csv('final_data_bystate.csv')
source("code/utils/read_statedata.R")
source("code/utils/read_rawstatedata.R")
library(fdacluster)
library(weathermetrics)
readstate <- read_statedata() 
stateraw <- read_rawstatedata()

#avgtemp
#3 populations
cluster1<-df[which(df$values==1),1]
cluster2<-df[which(df$values==2),1]
cluster3<-df[which(df$values==3),1]

states.name <- factor(data$state)
i1<-which(states.name==c("hawaii"))
data<-data[-i1,]

p_value <- numeric(8)
p_val_1_3<- numeric(8)
p_val_2_3<- numeric(8)
p_val_1_2<- numeric(8)
x11()
par(mfrow=c(4,8))

year<-c("2015","2016","2017","2018","2019","2020","2021","2022")
for (j in 1:8){

index<-which(data$year==year[j])
d_2015<-data[index,c(8,4)]

year_2015 <- d_2015 %>%
  group_by(state) %>%
  summarise(num_col_year = mean(colony_lost_pct, na.rm = T))

n<-dim(year_2015)[1]
for(i in 1:n){
  if(year_2015$state[i]%in%cluster1){year_2015$state[i]<-"cluster1"
  
  }else if (year_2015$state[i]%in%cluster2) {year_2015$state[i]<-"cluster2"
  
  }else if (year_2015$state[i]%in%cluster3) {year_2015$state[i]<-"cluster3"
  
  }
}


i1<- which(year_2015$state=='cluster1')
i2<- which(year_2015$state=='cluster2')
i3<- which(year_2015$state=='cluster3')
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1+n2+n3
g<-3
p<-1

attach(year_2015)
boxplot(num_col_year ~ state,main=paste('year',year[j]),col=rainbow(3),ylim=c(0,30))

#H0:τi=0 ∀i vs H1:∃τi≠0
fit <- aov(num_col_year ~ state,data=year_2015)
summary(fit)
#shapiro.test(fit$residuals)
#qqnorm(fit$residuals)
#abline(a=0,b=1,col='red')
#Ps = tapply(year_2015$num_col_year, year_2015$state, function( x ) ( shapiro.test( x )$p ))

# tutti p-value bassi 
#Var = tapply( year_2015$num_col_year, year_2015$state, var )
#Var
#bartlett.test( year_2015$num_col_year, year_2015$state )
#Since the p-value is not less than 0.05, i do not have sufficient evidence to say that the three groups have different variances.

T0 <- summary(fit)[[1]][1,4]
#T0
B = 1000
seed = 26111992
T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  weight_perm <- colony_lost_pct[permutation]
  fit_perm <- aov(weight_perm ~ state)
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

#hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
#abline(v=T0,col=3,lwd=2)
#plot(ecdf(T_stat),xlim=c(-1,20))
#abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
#0.045
p_value[j]=p_val

#test H0: mu3=m1 vs h1:mu3<m1
x1<-year_2015[i1,]
x2<-year_2015[i3,]

t1.mean = mean(x1$num_col_year)
t2.mean = mean(x2$num_col_year)
T20 = abs(t1.mean[1]-t2.mean[1])

B = 1000
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
  t1.mean_perm = colMeans(t1_perm[2])
  t2.mean_perm = colMeans(t2_perm[2])                       
  
  T2[perm]  = abs(t1.mean_perm-t2.mean_perm)
  
  pb$tick()
}

hist(T2,xlim=range(c(T2,T20)),main=paste('H0: mu3=m1'))
abline(v=T20,col=3,lwd=4)

#plot(ecdf(T2))
#abline(v=T20,col=3,lwd=4)

p_val_1_3[j] = sum(T2>=T20)/B  
#0.169


#test H0: mu3=m2 vs h1:mu3<m2
x1<-year_2015[i2,]
x2<-year_2015[i3,]

t1.mean = mean(x1$num_col_year)
t2.mean = mean(x2$num_col_year)
T20 = abs(t1.mean[1]-t2.mean[1])

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
  t1.mean_perm = colMeans(t1_perm[2])
  t2.mean_perm = colMeans(t2_perm[2])                       
  
  T2[perm]  = abs(t1.mean_perm-t2.mean_perm)
  
  pb$tick()
}

hist(T2,xlim=range(c(T2,T20)),main = paste('H0: mu3=m2'))
abline(v=T20,col=3,lwd=4)

#plot(ecdf(T2))
#abline(v=T20,col=3,lwd=4)

p_val_2_3[j] = sum(T2>=T20)/B 

#test H0: mu1=m2 vs h1:mu1<m2
x1<-year_2015[i1,]
x2<-year_2015[i2,]

t1.mean = mean(x1$num_col_year)
t2.mean = mean(x2$num_col_year)
T20 = abs(t1.mean[1]-t2.mean[1])

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
  t1.mean_perm = colMeans(t1_perm[2])
  t2.mean_perm = colMeans(t2_perm[2])                       
  
  T2[perm]  = abs(t1.mean_perm-t2.mean_perm)
  
  pb$tick()
}

hist(T2,xlim=range(c(T2,T20)),main = paste('H0: mu1=m2'))
abline(v=T20,col=3,lwd=4)

#plot(ecdf(T2))
#abline(v=T20,col=3,lwd=4)

p_val_1_2[j] = sum(T2>=T20)/B 
}

