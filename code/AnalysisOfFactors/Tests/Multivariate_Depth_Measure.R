# Multivariate Depth Measure

library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(aplpack)
library(robustbase)
library(dplyr)

df_final <- read.csv("code/AnalysisOfFactors/Tests/data_bystate_temp_perc.csv")

# year = 2015

# df_years <-  df_final %>% group_by(year, state, months) ????
  
df_2015 <- df_final[df_final$year==2015,]

losspct_avg_2015 <- df_2015 %>% group_by(state, year) %>% summarize(loss_avg = mean(colony_lost_pct))
varroa_avg_2015 <- df_2015 %>% group_by(state, year) %>% summarize(varroa_avg = mean(Varroa.mites))

state <- unique(df_2015$state)

varroa_losspct_2015 <- cbind(varroa_avg_2015$varroa_avg, losspct_avg_2015$loss_avg)
colnames(varroa_losspct_2015) <- c("varroa", "loss pct")
depthContour(varroa_losspct_2015,depth_params = list(method='Tukey'),
             graph_params = list(xlab="varroa", ylab="loss_pct"))

depthPersp(varroa_losspct_2015,depth_params = list(method='Tukey')) #plot_method = 'rgl'

maha_depth <- depth(varroa_losspct_2015,method='Mahalanobis') 
depthMedian(varroa_losspct_2015,depth_params = list(method='Mahalanobis'))

## Multivariate outlier detection via depth measures

plot(varroa_losspct_2015,xlab="varroa", ylab="loss pct", pch=19)
depthContour(
  varroa_losspct_2015,
  depth_params = list(method = 'Tukey'),
  points = TRUE,
  colors = colorRampPalette(c('white', 'navy')),
  levels = 10,
  pdmedian = F,
  graph_params = list(cex=.01, pch=1),
  pmean = F
)
#note the point with high varroa and low colony loss pct --> analyze it

depthMedian(varroa_losspct_2015) 

#bagplot
bagplot_losspct_varroa <- aplpack::bagplot(varroa_losspct_2015,factor = 2, show.whiskers = F,main="Bagplot")
outlying_obs <- bagplot_losspct_varroa$pxy.outlier

#remove outliers
ind_outlying_obs <- which(apply(varroa_losspct_2015,1,function(x)
  all(x %in% outlying_obs))) #compute the indices of the outliers
df_clean <- varroa_losspct_2015[-ind_outlying_obs,]

state[ind_outlying_obs] # --> hawaii


#Bagplot matrix

diseases_avg_2015 <- df_2015 %>% group_by(state, year) %>% summarize(diseases_avg = mean(Disesases))
pesticides_avg_2015 <- df_2015 %>% group_by(state, year) %>% summarize(pesticides_avg = mean(Pesticides))

losspct_stressors <-  cbind(varroa_avg_2015$varroa_avg, diseases_avg_2015$diseases_avg, pesticides_avg_2015$pesticides_avg, losspct_avg_2015$loss_avg)

colnames(losspct_stressors) <- c("varroa", "diseases", "pesticides", "loss pct")
pairs(losspct_stressors) #pairplot

bagplot_matrix <- aplpack::bagplot.pairs(losspct_stressors, factor = 2)

#2016

#Is distribution of colony lost pct different in 2016 with respect to 2015?

df_2016 <- df_final[df_final$year==2016,]

losspct_avg_2016 <- df_2016 %>% group_by(state, year) %>% summarize(loss_avg = mean(colony_lost_pct))

losspct_avg_2016 <- losspct_avg_2016[,-c(1,2)]
losspct_avg_2015 <- losspct_avg_2015[,-c(1,2)]

ddPlot(x = losspct_avg_2015,y = losspct_avg_2016,depth_params = list(method='Tukey'))
#different distributions!

#2017

df_2017 <- df_final[df_final$year==2017,]

losspct_avg_2017 <- df_2017 %>% group_by(state, year) %>% summarize(loss_avg = mean(colony_lost_pct))
losspct_avg_2017 <- losspct_avg_2017[,-c(1,2)]

ddPlot(x = losspct_avg_2015,y = losspct_avg_2017,depth_params = list(method='Tukey'))
#same distribution of colony lost pct between 2015 and 2017

