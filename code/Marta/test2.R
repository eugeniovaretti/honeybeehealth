data <- read.csv('final_data_bystate.csv')
source("code/utils/read_statedata.R")
source("code/utils/read_rawstatedata.R")
library(fdacluster)
library(weathermetrics)
readstate <- read_statedata() 
stateraw <- read_rawstatedata()

#avgtemp
temp_avg <- stateraw[,c(1,2,7,8)]
temp_avg <- temp_avg %>% tidyr::pivot_wider(
  names_from = state, 
  values_from = ` Average Temperature`,
  values_fill = 0
)

data_F <- as.matrix(temp_avg[,-c(1,2)])
which(is.na(data_F))

#creo matrice x contenente la grid di valutazione (sarànno 47stati x 30punti)
x = matrix(rep(rep(1:90),49), ncol= 90,byrow = T)
#creo matrice evaluation data (nObs*nDim*nPts) contenente i valori
y = array(t(data_F), c(49,1,90))
res <- kma(
  x,
  y,
  n_clust = 3,
  center_method = "medoid",
  warping_method = "affine",
  dissimilarity_method = "pearson"
)


# plot cluster
library(plotly)
groups <- levels(factor(stateraw$state))
us_data <- map_data("state")
df <- data.frame(
  state = tolower(groups),
  values = res$labels
)

#3 populations
cluster1<-df[which(df$values==1),1]
cluster2<-df[which(df$values==2),1]
cluster3<-df[which(df$values==3),1]

states.name <- factor(data$state)
i1<-which(states.name==c("other states"))
data<-data[-i1,]

index<-which(data$year=='2015')
d_2015<-data[index,4:8]
d_2015<-d_2015[-2]

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
plot(state, num_col_year,col=rainbow(g),main='Original Data',ylim=c(0,24))

#H0:τi=0 ∀i vs H1:∃τi≠0
fit <- aov(num_col_year ~ state,data=year_2015)
summary(fit)
shapiro.test(fit$residuals)
qqnorm(fit$residuals)
abline(a=0,b=1,col='red')
Ps = tapply(year_2015$num_col_year, year_2015$state, function( x ) ( shapiro.test( x )$p ))

# tutti p-value bassi 
Var = tapply( year_2015$num_col_year, year_2015$state, var )
Var
bartlett.test( year_2015$num_col_year, year_2015$state )
#Since the p-value is not less than 0.05, i do not have sufficient evidence to say that the three groups have different variances.

T0 <- summary(fit)[[1]][1,4]
T0
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

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val
#0.045

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

hist(T2,xlim=range(c(T2,T20)))
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

p_val = sum(T2>=T20)/B  
#With this P-value, I can reject H0 meaning that group 3 is better that gropu 1

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

hist(T2,xlim=range(c(T2,T20)))
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

p_val = sum(T2>=T20)/B 




p#High Plains--> They have extremely cold and dry winters. What precipitation does fall is usually snow. The summer is warmer, but not much wetter. What rain does fall is usually in the form of thunderstorms. Part of the reason for this climate is that by the time the air reaches these high plains, it usually passes over many mountains, releasing a lot of its moisture. This climate is a cold, semi-arid climate.
High_Plains <- c("Kansas", "Minnesota", "Nebraska", "North Dakota","South Dakota")
#Winters here can be very cold, but unlike the High Plains, these areas get quite a bit of precipitation, including snow in winter. This is because of moisture in the air from the Great Lakes and even from the Gulf of Mexico. Summers are warm, but feel worse than the temperatures would suggest because the moisture makes it humid. This warm air also produces thunderstorms and tornadoes. This climate could be described as a humid, continental climate.
Midwest<-c("llinois", "Indiana", "Iowa", "Kentucky", "Michigan", "Missouri", "Ohio","Wisconsin")
#Their climate includes mild and relatively warm winters, and wet and hot summers, creating large numbers of thunderstorms. 
#Things get cooler fast, though, and temperatures have already dropped a lot by October. 
#This climate could be described as a humid, subtropical climate.
Southeast<-c("Alabama", "Tennessee", "Florida", "Georgia", "North Carolina", "South Carolina", "Virginia")
#Winters are mild and cool thanks to being near the cooling influence of the Gulf of Mexico, and summers are wet and hot, again with a lot of thunderstorms. This climate is similar to the Southeast, and could also be described as a humid, subtropical climate. The only difference is the winters, which are mild due to the proximity to the Gulf of Mexico.
South<-c("Arkansas","Louisiana","Mississippi","Oklahoma","Texas")
Mid-Atlantic<-c("New England", "Massachusetts", "New York","Connecticut","Delaware","Maine","Maryland","New Hampshiere",)