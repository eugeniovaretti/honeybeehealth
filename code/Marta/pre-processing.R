d <- read.csv('data_merged.csv')
d[is.na(d)] <- 0

states.name <- factor(d$state)
i1<-which(states.name==c("United States"))
d<-d[-i1,]

US<-data[i1,]
i2<-which(US$months=="April-June"&US$year=="2019")
US<-US[-i2,]

new_data<-new_data%>%mutate(Varroa.mites=(Varroa.mites/100)*colony_max)
new_data<-new_data%>%mutate(Other.pests.parasites=(Other.pests.parasites/100)*colony_max)
new_data<-new_data%>%mutate(Disesases=(Disesases/100)*colony_max)
new_data<-new_data%>%mutate(Pesticides=(Pesticides/100)*colony_max)
new_data<-new_data%>%mutate(Other=(Other/100)*colony_max)
new_data<-new_data%>%mutate(Unknown=(Unknown/100)*colony_max)

i2<-which(d$months=="April-June"&d$year=="2019")
d<-d[-i2,]

d %>% 
  write.csv("cleaned_data_withpct.csv")

new_data %>% 
  write.csv("cleaned_data.csv")

US %>% 
  write.csv("US_data.csv")



