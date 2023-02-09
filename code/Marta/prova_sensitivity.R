data<-read.csv("sensitivity.csv")
library(survival)
library(survminer)
library(dplyr) 
library(ggplot2)
library(knitr)
library(broom)
library(tidyr)
ggplot(data=data,aes(x=state,y=time)) + 
  geom_bar(stat='identity',width=0.2) +
  geom_point(aes(color=status,shape=status),size=2) +
  coord_flip()
#response variable (time to event and status for death)
y<-Surv(data$time, data$status=="Event")
#survival curve --> KM estimator
fit <- survfit(y ~ 1, data = data)
summary(fit)
kable(head(tidy(fit),20))
surv_median(fit)
plot(fit, conf.int = T, xlab='Time [days]', ylab = 'Survival Probability', col='red',
     main="Kaplan-Meier Curve")
ggsurvplot(fit,
           data=data,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           #break.time.by=90,
           title="Kaplan-Meier Curve for Lung Cancer Survival")

#cumulative failure probability (CFP), shows the cumulative probabilities of experiencing the event of interest
cumulative_incidence <- 1 - fit$surv
ggsurvplot(fit,
           data=data,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           #break.time.by=90,
           fun='event',
           title="Cumulative Incidence Curve for Lung Cancer Survival")
#hist(data$MaximumTemperature, xlab='Age [years]', main='Histogram of age in Lung Cancer Data')
#indexes<-read.csv('cluster_index.csv')
cluster1MAX<-df[which(df$values==1),1]
cluster2MAX<-df[which(df$values==2),1]
cluster3MAX<-df[which(df$values==3),1]
cluster1AVG<-df[which(df$values==1),1]
cluster2AVG<-df[which(df$values==2),1]
cluster3AVG<-df[which(df$values==3),1]
cluster1MIN<-df[which(df$values==1),1]
cluster2MIN<-df[which(df$values==2),1]
cluster3MIN<-df[which(df$values==3),1]
cluster1P<-df[which(df$values==3),1]
cluster2P<-df[which(df$values==2),1]
cluster3P<-df[which(df$values==1),1]
data <- data %>% rename("clusterMIN" = "clusterMAX",
                                        "clusterMAX" = "clusterMIN"
                                        )
for(i in 1:n){
  if(data$clusterMIN[i]=="cluster3"){data$clusterMIN[i]<-"cluster1"
  
  }else if (data$clusterMIN[i]=="cluster1") {data$clusterMIN[i]<-"cluster3"
  
  }
}

data$time[34]=13
data$time[34]=10
data$time[35]=9
data$time[26]=21
data$time[45]=4
data$time[20]=4
n<-dim(data)[1]

for(i in 1:n){
  if(data$state[i]%in%cluster1P){data$clusterPDSI[i]<-"cluster1"
  
  }else if (data$state[i]%in%cluster2P) {data$clusterPDSI[i]<-"cluster2"
  
  }else if (data$state[i]%in%cluster3P) {data$clusterPDSI[i]<-"cluster3"
  
  }
}
##TEMP
groups <- levels(factor(data$state))
cluster_index<- cbind(state=groups)
df <- data.frame(
  state = tolower(groups),
  values = data$clusterPDSI
)
cluster_index <- cbind(cluster_index, temp_avg=data$clusterPDSI)
library(usmap)
plot_usmap(data = df) + labs(title = "Cluster by prec")+colors()

fit.MIN <- survfit(Surv(time, status=="Event") ~ clusterMIN, data=data)
ggsurvplot(fit.MIN, 
           data=data,
           conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs=c("cluster1","cluster2","cluster3"),   
           palette=c("firebrick1","forestgreen","dodgerblue2"), 
           title="Kaplan-Meier Curves by cluster for PREC")

#It looks like there’s some differences in the curves 
#the curves of cluster 1(higher temperature) has a survival probability higher than cluster3 (lower temp min)
#Is there statistical evidence for that difference?
log_rank_test <- survdiff(Surv(time, status=="Event") ~ clusterMIN, data=data)
log_rank_test
hazard_ratio <- (log_rank_test$obs[1]/log_rank_test$exp[1])/(log_rank_test$obs[3]/log_rank_test$exp[3])
hazard_ratio
#temp min
#HR= zard_ratio=0.880027
#the risk of deaths in cluster1 (temp min maggiori, stati a sud) is 0.880027 times the risk in cluster3 (temp min minori, stati a nord)

cox.age <- coxph(Surv(time, status=="Event") ~ AverageTemperature, data = data)
summary(cox.age)
#the increase of 1 unit in the temperature, decrease the hazard of 0.84535
#the higher the temperature the lower the hazard, the lower the risk of dying
min_df <- with(data,
              data.frame(AverageTemperature = c(-1,7,16) )
)
fit.min <- survfit(cox.age, newdata = min_df)
fit.min
plot(fit.min, conf.int=F,
     col=c("dodgerblue2","navy","darkmagenta"), lwd=2, lty=1,
     xlab='Time', ylab='Survival Probability',
     main='Adjusted Survival Probability Plot')
grid()
legend('topright', c("Temp = -10", "Temp = -5", "Temp = 1"),
       lty=c(1,1,1), lwd=c(2,2,2), col=c("dodgerblue2","navy","darkmagenta"))
#as temperature increases the probability of dying decreases

###PREC
groups <- levels(factor(data$state))
cluster_index<- cbind(state=groups)
df <- data.frame(
  state = tolower(groups),
  values = data$clusterPREC
)
cluster_index <- cbind(cluster_index, temp_avg=data$clusterPREC)
library(usmap)
plot_usmap(data = df) + labs(title = "Cluster by prec")

fit.MIN <- survfit(Surv(time, status=="Event") ~ clusterPREC, data=data)
ggsurvplot(fit.MIN, 
           data=data,
           conf.int = F,
           risk.table = TRUE, # Add risk table
           #risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs=c("cluster3","cluster1","cluster2"),   
           palette=c("dodgerblue2","firebrick1","forestgreen"), 
           title="Kaplan-Meier Curves by cluster for PREC")

#It looks like there’s some differences in the curves 
#the curves of cluster 2(higher temperature) has a survival probability higher than cluster3 (lower temp min)
#Is there statistical evidence for that difference?
log_rank_test <- survdiff(Surv(time, status=="Event") ~ clusterPREC, data=data)
log_rank_test
hazard_ratio <- (log_rank_test$obs[1]/log_rank_test$exp[1])/(log_rank_test$obs[3]/log_rank_test$exp[3])
hazard_ratio
#HR= zard_ratio=0.6566611
#the risk of deaths in cluster1 (temp min maggiori, stati a sud) is 0.6566611 times the risk in cluster3 (temp min minori, stati a nord)

cox.age <- coxph(Surv(time, status=="Event") ~ Precipitation, data = data)
summary(cox.age)
#the increase of 1 unit in the temperature, decrease the hazard of 0.84535
#the higher the temperature the lower the hazard, the lower the risk of dying
min_df <- with(data,
               data.frame(AverageTemperature = c(1,3,6) )
)
fit.min <- survfit(cox.age, newdata = min_df)
fit.min
plot(fit.min, conf.int=F,
     col=c("dodgerblue2","navy","darkmagenta"), lwd=2, lty=1,
     xlab='Time', ylab='Survival Probability',
     main='Adjusted Survival Probability Plot')
grid()
legend('topright', c("prec = 1", "Prec= 3", "Prec = 6"),
       lty=c(1,1,1), lwd=c(2,2,2), col=c("dodgerblue2","navy","darkmagenta"))
#as precipitation increases the probability of dying decreases

###MODEL
cox.age <- coxph(Surv(time, status=="Event") ~ AverageTemperature+Precipitation+PalmerDroughtSeverityIndexPDSI+clusterMIN, data = data)
summary(cox.age)
#holding the other covariates constant:
#the higher the temperature the lower the death (0.76503)
#higer temperature is associated with good prognostic.
#the higher the precipitation the lower the death (0.59427)
#higer precipitation is associated with good prognostic.
#the higher the PDSI (dry condition)index the higher the risk of death (1.24450)
#dry condition is associated with bad prognostic.
#as seen before states belonging to cluster3 have a lower probability of dying w.r.t state in cluster 1 (0.11681)

#The p-values for all three overall tests (likelihood, Wald, and score) are extremely small, 
#indicating that the model is significant
ggforest(cox.age, data=data)
#Goodness of fit
ggcoxdiagnostics(cox.age, type = "martingale")
#I want the residual to be centered around 0 and not oscillating above or below 0!

#H0: Hazards are proportional
#H1: Hazards are NOT proportional
#cox.zph() return tests for each X and for the global model
test.ph <- cox.zph(cox.age)
test.ph
#high p-value
#we can assume the proportional hazards.
ggcoxdiagnostics(cox.age, type = "scaledsch")
