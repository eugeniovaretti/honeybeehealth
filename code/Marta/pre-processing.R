data <- read.csv('data_merged.csv')
data[is.na(data)] <- 0

states.name <- factor(data$state)
i1<-which(states.name==c("United States"))
new_data<-data[-i1,]

US<-data[i1,]

new_data<-new_data%>%mutate(Varroa.mites=(Varroa.mites/100)*colony_max)
new_data<-new_data%>%mutate(Other.pests.parasites=(Other.pests.parasites/100)*colony_max)
new_data<-new_data%>%mutate(Disesases=(Disesases/100)*colony_max)
new_data<-new_data%>%mutate(Pesticides=(Pesticides/100)*colony_max)
new_data<-new_data%>%mutate(Other=(Other/100)*colony_max)
new_data<-new_data%>%mutate(Unknown=(Unknown/100)*colony_max)

i2<-which(months.name=="April-June"&year.name=="2019")
new_data<-new_data[-i2,]

new_data %>% 
  write.csv("cleaned_data.csv")

US %>% 
  write.csv("US_data.csv")


d<-new_data[,c(7,12,13,14,15,16,17)]
model<-lm(colony_lost~.,data=d)
summary(model)
plot(model)
shapiro.test(model$residuals)

plot(d)


