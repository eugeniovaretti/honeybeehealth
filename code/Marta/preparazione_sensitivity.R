d<-read.csv("df_survival.csv")
year.factor<-factor(d$year)
i1<-which(year.factor==c("2015"))
d<-d[-i1,]
data<-read.csv("data_bystate_temp_perc.csv")
library(survival)
library(survminer)
library(dplyr) 
library(ggplot2)
library(knitr)
library(broom)
library(tidyr)
library(tidyverse)
df2<-d[!duplicated(d$state),]
df2$time <- paste(df2$year, df2$months)
n<-dim(df2)[1]
for(i in 1:n){
  if(df2$time[i]=="2019 Q4"){df2$time[i]<-"20"
  
  }else if (df2$time[i]=="2016 Q4"){df2$time[i]<-"8"
  
  }else if (df2$time[i]=="2017 Q1"){df2$time[i]<-"9"
  
  }else if (df2$time[i]=="2016 Q1"){df2$time[i]<-"5"
  
  }else if (df2$time[i]=="2016 Q2"){df2$time[i]<-"6"
  
  }else if (df2$time[i]=="2016 Q1"){df2$time[i]<-"5"
  
  }else if (df2$time[i]=="2019 Q1"){df2$time[i]<-"17"
  
  }else if (df2$time[i]=="2018 Q1"){df2$time[i]<-"13"
  
  }
}

df<-df2%>% inner_join(data,
                      by=c('state'='state','year'='year','months'='months'))
state.factor<-factor(df$state)
year.factor<-factor(data$year)
m.factor<-factor(data$months)
i2<-which(data$state%in%state.factor)
df<-df[,-c(4,5,6,7)]
data<-data[,-22]
data<-add_column(data, time = rep(30,22), .after = 3)
colnames(df)[5]<-"colony_n"
df3<-rbind(df,data)
df3<-df3[order(df3$state),]
n<-dim(df3)[1]
for(i in 1:n){
  ifelse((df3$time[i] == "30" ), 
         df3$status[i] <- "Censor",  df3$status[i] <- "Event")
}
df3 %>% 
  write.csv("sensitivity.csv",row.names=F)
