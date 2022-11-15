install.packages("Hmisc")
library("Hmisc")
mydata.rcorr = rcorr(as.matrix(d2))
mydata.rcorr
with(data, scatterplotMatrix(data.frame(colony_lost,year, colony_max, Varroa.mites, Other.pests.parasites, Disesases)))

library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
#d3<-data[,c(3,4,5,7,8,13,14,15,16,17,18)]
d2<-d[,c(3,4,5,7,9,13,14,15,16,17,18)]
#devo aggiungere colony_max anche con le percentuali??
model2<-lm(colony_lost_pct~.,data=d2)
#model3<-lm(colony_lost~.,data=d3)
summary(model2)
v3<-vif(model3)
v2<-vif(model2)
hist(model3$residuals)
qqnorm(model3$residuals)
#residuals are not normally distributed 
shapiro.test(model$residuals)
#p-value < 2.2e-16
plot(model)

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
