n.str1.corr <- d %>% 
  group_by(
    year,months) %>% 
  select(year, months,colony_lost_pct) %>% 
  summarise(`Average colony_lost_pct` = mean(colony_lost_pct))
boxplot(n.str1.corr)
plot(as.factor(n.str1.corr$months), n.str1.corr$`Average colony_lost_pct`,xlab='quarter',col=rainbow(4),main='Original Data')

n.str1.corr_s <- n.str1.corr

n <- dim(n.str1.corr)[1]

for(i in 1:n){
  ifelse((n.str1.corr$months[i] == "April-June" | n.str1.corr$months[i] == "July-September" ), 
         n.str1.corr_s$months[i] <- "Summer",  n.str1.corr_s$months[i] <- "Winter")
}
plot(as.factor(n.str1.corr_s$months), n.str1.corr$`Average colony_lost_pct`,xlab='quarter',
     col=c("red","blue"),main='Original Data')

n <- dim(data)[1]
for(i in 1:n){
  ifelse((data$months[i] == "April-June" | data$months[i] == "July-September" ), 
         data$months[i] <- "Summer",  data$months[i] <- "Winter")
}
plot(as.factor(data$months), data$colony_lost_pct,xlab='quarter',
     col=c("red","blue"),main='Original Data')



