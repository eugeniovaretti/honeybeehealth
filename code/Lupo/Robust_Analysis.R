
#Robust Statistics Analysis by Lupo

rm(list=ls())
setwd("~/Desktop/NP_project/NP_github/np_project/")

library(robustbase)
library(psych)
library(MASS)
library(ellipse)
library(here)
library(DescTools)
library(knitr)
library(RobStatTM)

df_final <- read.csv("data/new_data/data_bystate_temp_perc.csv")

##############################################################################.

#### Stressors ####

df_stressors <- df_final[, 11:16]
pairs(df_stressors)
fit_MCD <- covMcd(x = df_stressors, alpha = 0.90, nsamp = 1000)
plot(fit_MCD,classic=TRUE) #DD-plot and outlier detection

df_finals[395,]
df_final[441,]

ind_best_subset <- fit_MCD$best
N <- nrow(df_stressors)

p <- ncol(df_stressors)
ind_rew_obs <- which(
  mahalanobis( #distance based on raw estimates
    x = df_stressors,
    center = fit_MCD$raw.center,
    cov = fit_MCD$raw.cov
  ) <= qchisq(p = .975, df = p) #DO NOT change this df
)

plot(df_stressors, col=ifelse(1:N %in% ind_best_subset, "black", "red"), pch=19, main = "Raw MCD")
plot(df_stressors, col=ifelse(1:N %in% ind_rew_obs, "black", "red"), pch=19, main = "RMCD")


###############################################################################.

#### Colony n versus colony loss ####

df_2 <- df_final[,c(1,2,3,4,6)]

fit_lts <- ltsReg(colony_n ~ colony_lost, data = df_2, alpha=.75, mcd=TRUE)
#does not accept categorical feature :(


summary(fit_lts)
plot(df_2$colony_lost,df_2$colony_n)
abline(fit_lts, col="darkgreen", lwd=2)
plot(fit_lts) 
