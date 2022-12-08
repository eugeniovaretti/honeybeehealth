data <- read.csv('cleaned_data.csv')
d <- read.csv("cleaned_data_withpct.csv")
stressor <- read.csv('stressor.csv')
stressor[is.na(stressor)] <- 0
US <-read.csv("US_data.csv")
US[is.na(US)] <- 0

library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(rgl)
library(tidyverse)

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
