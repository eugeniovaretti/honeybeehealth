###GAM###
df <- read.csv("data/new_data/data_bystate_temp_perc.csv")
indexes<-read.csv('data/new_data/sensitivity.csv')
df_st <- df[df$state != "hawaii" & df$state != "other states",]

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove hawaii:
df_coord <- df_coord[df_coord$state != "hawaii",]
#remove Na (for "other states"):
df_coord <- na.omit(df_coord)

df_new <- merge(df_st, df_coord, by = "state") 

library(tidyverse)
df_new <- df_new %>% arrange(state, year, months)

#remove column "state"
df_new <- df_new[,-1]

t <- rep(1:29, 44)
df_new$t <- t #numerical variable for time (needed if we want to smooth the time)

df_new$year <- factor(df_new$year)
df_new$months <- factor(df_new$months)

#transform colony_lost_pct in values between 0 and 1:
df_binary <- df_new

#dataset for the logit
cols <- c("colony_lost_pct", "Varroa.mites", "Other.pests.parasites", "Disesases", "Pesticides", "Other", "Unknown")
df_binary[cols] <- lapply(df_binary[cols], function(x) x/100)

#modello1
dgam_binary <- gam(colony_lost_pct ~ t + Varroa.mites:MinimumTemperature + Varroa.mites:Precipitation + s(AverageTemperature, bs='cr')
                  + s(lon,lat, bs = 'tp') + s(Varroa.mites, bs="cr") + s(Disesases, bs="cr")
                  + s(Pesticides, bs="cr") + + s(Unknown, bs="cr") + s(PalmerDroughtSeverityIndexPDSI, bs="cr"),
                  family=binomial(link="logit"), data=df_binary) 

summary(gam_binary)
(r_2_squared <- summary(gam_binary)$r.sq)
summary(gam_binary)$s.pv


#modello2
fun <- function(x) {(x - min(x)) / (max(x) - min(x))}
w <- fun(df_binary["colony_max"])
library(mgcv)
gam_thinplate_w <- gam(colony_lost_pct ~ year + 
                         s(lon,lat, by=unlist(w), bs="tp")+ 
                         s(Varroa.mites, bs="cr") + s(Disesases, bs="cr")
                       + s(Pesticides, bs="cr"), 
                       family=binomial(link="logit"), data=df_binary) 
summary(gam_thinplate_w)
#weight the function depending on dimension of the state (supposed proportional to colony max)


#dataset for the abs values
df_abs <- df_new
#convert stressors to absolute values:
df_abs$Varroa.mites <- df_binary$Varroa.mites*df_binary$colony_max
cols <- c("Other.pests.parasites", "Disesases", "Pesticides", "Other", "Unknown")
df_abs[cols] <- sapply(df_binary[cols], '*', df_binary$colony_max)


#modello3

df_abs$AverageTemperature_norm<-scale(df_abs$AverageTemperature, center = TRUE, scale = FALSE)
df_abs$Precipitation_norm<-scale(df_abs$Precipitation, center = TRUE, scale = FALSE)
df_abs <- df_abs %>% rename("Precipitation[,1]" = "Precipitation",
                            "AverageTemperature[,1]" = "AverageTemperature")
df_abs$AverageTemperature<-scale(df_abs$AverageTemperature, center = TRUE, scale = FALSE)
gam_tens_spacetime <- gam(colony_lost ~ offset(log(colony_max))
                          #+ s(I(Varroa.mites*Other.pests.parasites), bs="cr")
                          #+s(I(Varroa.mites*Pesticides), bs="cr") 
                          + Precipitation + AverageTemperature
                          +s(Varroa.mites, bs="cr") + s(Other.pests.parasites, bs="cr")
                          + s(Pesticides, bs="cr")   
                          + te(lon,lat,t, bs=c("tp","cr"), d=c(2,1), k=12), family=poisson(link="log"), data=df_abs)
summary(gam_tens_spacetime)
plot(gam_tens_spacetime)

gam_tens_spacetime <- gam(colony_lost ~ 0+ offset(log(colony_max))
                          + s(I(Varroa.mites*Other.pests.parasites), bs="cr")
                          #+s(I(Varroa.mites*Pesticides), bs="cr") 
                          #+ Precipitation + AverageTemperature
                          +s(Varroa.mites, bs="cr") + s(Other.pests.parasites, bs="cr")
                          + s(Pesticides, bs="cr")   
                          + te(lon,lat,t, bs=c("tp","cr"), d=c(2,1), k=12), family=poisson(link="log"), data=df_abs)
summary(gam_tens_spacetime)
plot(gam_tens_spacetime)

#MODELLO
library(dplyr)

df<- indexes%>%select(state,clusterMIN,clusterMAX,clusterAVG,clusterPREC,clusterPDSI)
new_d<- merge(df,df_abs,by='state')

gam_tens_space <- gam(colony_lost ~ offset(log(colony_max))
                          #+ s(I(Varroa.mites*Other.pests.parasites), bs="cr")
                          #+s(I(Varroa.mites*Pesticides), bs="cr") 
                          #+ s(Precipitation_norm, bs="cr") + s(AverageTemperature_norm,bs="cr")
                          + s(Varroa.mites,bs="cr")
                          + clusterMIN + s(Other.pests.parasites, bs="cr")
                          + s(Pesticides, bs="cr")   
                          + te(lon,lat,t, bs=c("tp","cr"), d=c(2,1), k=12) 
                          , family=poisson(link="log"), data=new_d)
summary(gam_tens_space)
plot(gam_tens_space)



#modello4
gam_tens_spacetime <- gam(colony_lost ~ colony_max
                          + s(I(Varroa.mites*Other.pests.parasites), bs="cr")
                          +s(I(Varroa.mites*Pesticides), bs="cr") 
                          + Precipitation + AverageTemperature
                          +s(Varroa.mites, bs="cr") + s(Other.pests.parasites, bs="cr")
                          + s(Pesticides, bs="cr") 
                          #+ s(Disesases, bs="cr")  
                          + te(lon,lat,t, bs=c("tp","cr"), d=c(2,1), k=10), data=df_abs)
summary(gam_tens_spacetime)
plot(gam_tens_spacetime)
#l'interazione tra varroa e other pests porta a una crescita della loss fino a quando non sono presenti emtrambi in 
#maniera troppo elevata. in quel caso si ha un aumento della loss se si prendono i fattori singolarmente.
#combinandoli è come se si verifiasse una compensazione tra i due stressori che non comporta un aumento della loss


#modello5
df_abs$AverageTemperature<-scale(df_abs$AverageTemperature, center = TRUE, scale = FALSE)
gam_tensprod <- gam(colony_lost ~  colony_max + s(I(Varroa.mites*Other.pests.parasites), bs="cr")
                    +s(I(Varroa.mites*Pesticides), bs="cr") +s(I(Disesases*Varroa.mites), bs="cr")
                    + Precipitation + MaximumTemperature+PalmerDroughtSeverityIndexPDSI
                    + s(Varroa.mites, bs="cr") 
                    + s(Pesticides, bs="cr")  + s(Other.pests.parasites, bs="cr") 
                    + te(t,lon,lat), data=df_abs) #family=poisson
summary(gam_tensprod)
plot(gam_tensprod)


## TRY WITH SHIFT on varroa (verify if high varroa in q3 high loss in q4)

