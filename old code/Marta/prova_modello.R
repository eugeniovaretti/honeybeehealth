new_data <- read.csv('data_bystate_temp_perc.csv')
d<- read.csv('df_survival.csv')
new_data<-new_data%>%mutate(Varroa.mites=(Varroa.mites/100)*colony_max)
new_data<-new_data%>%mutate(Other.pests.parasites=(Other.pests.parasites/100)*colony_max)
new_data<-new_data%>%mutate(Disesases=(Disesases/100)*colony_max)
new_data<-new_data%>%mutate(Pesticides=(Pesticides/100)*colony_max)
new_data<-new_data%>%mutate(Other=(Other/100)*colony_max)
new_data<-new_data%>%mutate(Unknown=(Unknown/100)*colony_max)
indexes<-read.csv('cluster_index.csv')
cluster1<-tolower(indexes[which(indexes$temp_avg==1),1])
cluster2<-tolower(indexes[which(indexes$temp_avg==2),1])
cluster3<-tolower(indexes[which(indexes$temp_avg==3),1])

library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(rgl)
library(tidyverse)
n<-dim(new_data)[1]

for(i in 1:n){
  if(new_data$state[i]%in%cluster1){new_data$state[i]<-"cluster1"
  
  }else if (new_data$state[i]%in%cluster2) {new_data$state[i]<-"cluster2"
  
  }else if (new_data$state[i]%in%cluster3) {new_data$state[i]<-"cluster3"
  
  }
}


#model with California 
states.name <- factor(new_data$state)
attach(new_data)
i1<-which(states.name==c("hawaii"))
new_data<-new_data[-i1,c(1,2,3,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21)]
new_data<-new_data[i1,]
with(new_data, scatterplotMatrix(data.frame(colony_lost, MinimumTemperature,MaximumTemperature, AverageTemperature,Precipitation, PalmerDroughtSeverityIndexPDSI)))
with(new_data, scatterplotMatrix(data.frame(colony_lost_pct,Varroa.mites,Other.pests.parasites,Disesases,Pesticides,Other,Unknown)))
model_gam=lm(colony_lost ~ colony_max+colony_added+Varroa.mites+
                Other.pests.parasites+Disesases
              + Pesticides+ Other+Unknown+factor(year)+MinimumTemperature+MaximumTemperature+AverageTemperature+Precipitation+PalmerDroughtSeverityIndexPDSI
              ,data = new_data)
summary(model_gam)

model_gam2=gam(colony_lost ~ colony_max+colony_added+Varroa.mites+ 
                s(Other.pests.parasites,bs='cr')+Disesases
              + s(Pesticides,bs='cr')+ s(Other,bs='cr')+ s(Unknown,bs='cr')
              +  factor(year)
            +months
              +state
             +  s(MinimumTemperature,bs='cr')+s(MaximumTemperature,bs='cr')+s(AverageTemperature,bs='cr')+s(Precipitation,bs='cr')+s(PalmerDroughtSeverityIndexPDSI,bs='cr')
              ,data = new_data)
summary(model_gam2)

model_gam2=lm(colony_lost ~ colony_max+ns(colony_added,df=3)+Varroa.mites+ 
                 ns(Other.pests.parasites,df=3)+Disesases
               + ns(Pesticides,df=3)+ ns(Other,df=3)+ Unknown+ns(year, df = 3)+
                 ns(MinimumTemperature,df=3)+MaximumTemperature+ns(AverageTemperature,df=3)+ns(Precipitation,df=3)+ns(PalmerDroughtSeverityIndexPDSI,df=3)
               ,data = new_data)
summary(model_gam2)

model_gam3=gam(colony_lost ~ colony_max+s(colony_added,bs='cr')+Varroa.mites+ 
                s(Other.pests.parasites,bs='cr')+Disesases
              + s(Pesticides,bs='tp')+ Other+Unknown+factor(year)+s(AverageTemperature,bs='cr')+s(PalmerDroughtSeverityIndexPDSI,bs='tp')
              ,family=binomial(link='logit'),data = new_data)
summary(model_gam3)
plot(model_gam3)

preds=predict(model_quad_splines, new_data,se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

with(Prestige, plot(income ,prestige ,xlim=range(new_data$income) ,cex =.5, col =" darkgrey " ))
lines(new_data$income,preds$fit ,lwd =2, col =" blue")
matlines(new_data$income, se.bands ,lwd =1, col =" blue",lty =3)

#model1
d3<-d[,c(1,2,3,5,7,8,11,12,13,14,15,16)]

#1. model logit della percentuale (ha sens tenere colony max?)
d3<-d3%>%mutate(colony_lost_pct=(colony_lost_pct/100))
model <- glm(colony_lost_pct ~.,family=binomial(link='logit'),data=d3)
summary(model)
##mylogit <- glm(colony_lost_pct ~. , data = d3, family = "binomial")


#2. semiparametric model with the absolute value of the loss
#colony lost in percentuale o assoluto?
d2<-d[,c(1,2,3,5,6,8,11,12,13,14,15,16)]
with(d2, scatterplotMatrix(data.frame(colony_lost, colony_max, colony_added)))
with(d2, scatterplotMatrix(data.frame(colony_lost,Varroa.mites,Other.pests.parasites,Disesases,Pesticides,Other,Unknown)))

#come aggiungo il tempo? (posso usare fourier?)
model_gam=gam(colony_lost ~ colony_max+s(colony_added,bs='cr')+s(Varroa.mites,bs='cr')+ 
                s(Other.pests.parasites,bs='cr')+s(Disesases,bs='cr')
              + s(Pesticides,bs='cr')+ s(Other,bs='cr')+ s(Unknown,bs='cr')+ns(year, df = 3)
              ,data = d2)
summary(model_gam)
#come aggiungo lo stato che è un carattere(variabile categorica)?   +s(state),data=d2)
#idea: scarico i dati latitudine e longitudine di ogni stato


#installare crs --> ho trovato che la funzione crs fa la regressione con splines per categoriche
library(crs)
attach(d2)
model <- crs(colony_lost~colony_max+factor(state),cv="none",degree=3,segments=1,lambda=.1)
model <- crs(colony_lost~colony_max+colony_added+Varroa.mites+ 
               Other.pests.parasites+Disesases
             + Pesticides+Other+Unknown+year+factor(state),basis = "tensor")

model <- crs(colony_lost~colony_max+colony_added+Varroa.mites+ 
               Other.pests.parasites+Disesases
             + Pesticides+Other+Unknown+year+factor(state),cv="nomad",basis = "auto")
             
model_gam_ns <-
  lm(colony_lost ~ colony_max+colony_added+ns(Varroa.mites, df = 3) + ns(Disesases, df = 3), data = d2)






#devo aggiungere colony_max anche con le percentuali??
model2<-lm(colony_lost~.,data=d2)
#model3<-lm(colony_lost~.,data=d3)
summary(model2)
v3<-vif(model3)
v2<-vif(model2)
hist(model2$residuals)
qqnorm(model2$residuals)
#residuals are not normally distributed 
shapiro.test(model2$residuals)
#p-value < 2.2e-16
plot(model)


d2<-d[1:46,c(1,2,3,7,11,12,13,14,15,16)]

n.str1.corr <- d %>% 
   
  select(year,contains("2015"),Varroa.mites,colony_lost_pct) 


model00<-lm(`Average colony_lost_pct`~`Average varroa`,data=n.str1.corr)
qqnorm(model00$residuals)
shapiro.test(model00$residuals)

model_lm_interaction <- lm(colony_lost ~ colony_max+ Varroa.mites+Other.pests.parasites+Disesases+Other+Unknown + Other.pests.parasites:Varroa.mites, data=data) 
# model_lm_interaction <- lm(prestige ~ education * income, data=Prestige) # alternatively 
summary(model_lm_interaction)
shapiro.test(model_lm_interaction$residuals)

model_gam=gam(colony_lost_pct ~ Varroa.mites+Other.pests.parasites+Disesases+Other+Unknown+s(months,bs='cr')+ s(year,bs='cr'),data = d2)

hist(model_gam$residuals)
#seems to be normal distributed
qqnorm(model_gam$residuals)
#residuals are normally distributed 
shapiro.test(model_gam$residuals)
#we can not reject the normality of the residuals
plot(model_gam)

plot(d)
