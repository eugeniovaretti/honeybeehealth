install.packages("Hmisc")
library("Hmisc")
mydata.rcorr = rcorr(as.matrix(data))
mydata.rcorr
with(data, scatterplotMatrix(data.frame(colony_lost,year, colony_max, Varroa.mites, Other.pests.parasites, Disesases)))

d<-data[,c(8,13,14,15,16,17,18)]
model<-lm(colony_lost~.,data=d)
summary(model)
hist(model$residuals)
qqnorm(model$residuals)
#residuals are not normally distributed 
shapiro.test(model$residuals)
#p-value < 2.2e-16
plot(model)

model_lm_interaction <- lm(colony_lost ~ colony_max+ Varroa.mites+Other.pests.parasites+Disesases+Other+Unknown + Other.pests.parasites:Varroa.mites, data=data) 
# model_lm_interaction <- lm(prestige ~ education * income, data=Prestige) # alternatively 
summary(model_lm_interaction)
shapiro.test(model_lm_interaction$residuals)

model_gam=gam(colony_lost ~ colony_max+ Varroa.mites+Other.pests.parasites+Disesases+Other+Unknown+s(months,bs='tp'),data = data)

hist(model_gam$residuals)
#seems to be normal distributed
qqnorm(model_gam$residuals)
#residuals are normally distributed 
shapiro.test(model_gam$residuals)
#we can not reject the normality of the residuals
plot(model_gam)

plot(d)
