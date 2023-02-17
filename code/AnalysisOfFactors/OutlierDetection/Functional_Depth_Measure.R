#Lupo functional depth measures

#source("code/utils/read_statedata.R")
#source("code/utils/read_rawstatedata.R")

library(weathermetrics)
library(rgl)
library(roahd)
library(dplyr)
library(fda)
library(fdacluster)
#noreadstate <- read_statedata()
#stateraw <- read_rawstatedata()

#write.csv(readstate, "data/new_data/temp_prec_trimesters.csv", row.names=FALSE)

df_final <- read.csv("data/new_data/data_bystate_temp_perc.csv")

#from readstate dataset: there are more states rather than data_bystate_temp_perc
#note that 'Alaska' has missing values in pdsi.
#while df_final has added information for 'Hawaii' and 'Other States' with respect
#to the states in readstate

#from readstate dataset

# groups <- levels(factor(readstate$state))
# 
# #precipitations
# precipitations <- readstate[,c(1,2,6,8)]
# prec_pivot <- precipitations %>% tidyr::pivot_wider(
#   names_from = state,
#   values_from = 'Precipitation',
#   values_fill = 0
# )
# prec_matrix <- as.matrix(prec_pivot[,-c(1,2)])
# #which(is.na(data_Fp))
# #data_Fp[is.na(data_Fp)]<-0
# 
# grid <- seq(1,30)
# matplot(grid,prec_matrix, type="l", col=adjustcolor(col=1,alpha.f = .4))
# 
# f_prec <- fData(grid,t(prec_matrix))
# plot(f_prec, main="Precipitations") 
# #plot(f_prec[1:5,])
# 
# #mean temp
# temp_mean <- readstate[,c(1,2,7,8)]
# temp_mean_pivot<- temp_mean %>% tidyr::pivot_wider(
#   names_from = "state",
#   values_from = "AverageTemperature",
#   values_fill = 0
# )
# temp_mean_matrix <- as.matrix(temp_mean_pivot[,-c(1,2)]) #remove year and months column
# grid <- seq(1,30)
# f_temp_mean <- fData(grid,t(temp_mean_matrix))
# plot(f_temp_mean, main="Mean temperature") 
# 
# #Bivariate data: precipitations and average temperature
# 
# prec_mean_temp <- as.mfData(list(f_prec, f_temp_mean))
# plot(prec_mean_temp)
# do.call(args = lapply(1:2, function(ind)
#   MHI(prec_mean_temp$fDList[[ind]])), what = "cor")
# # --> Spearman correlation index: 0.46
# 
# ## Computing depth measures in a FDA framework
# 
# mbd_prec <- MBD(Data = f_prec)
# median_prec <- median_fData(fData = f_prec, type = "MBD")
# plot(median_prec) #curve that has the highest value of MBD
# 
# plot(f_prec)
# lines(grid,median_prec$values) 

#remove the rows with NaN (state 'Hawaii' and 'Other States')
df_final <- na.omit(df_final)
which(is.na(df_final))

#check Spearman correlations from df_final dataset: note that it has one temporal
#data less than readstate because 2019-Q2 is missing

grid <- seq(1,29) #29 values instead of 30 because of missing values in 2019-Q2

#colony loss
colony_loss <- df_final[,c(1,2,3,6)]
colony_loss_pivot <- colony_loss %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "colony_lost",
  values_fill = 0
)
colony_loss_matrix <- as.matrix(colony_loss_pivot[,-c(1,2)]) #remove year and months column
f_colony_loss <- fData(grid,t(colony_loss_matrix))

#mean temp
temp_smooth <- df_final[,c(1,2,3,21)]
tempmean_smooth_new <- temp_smooth %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "AverageTemperature",
  values_fill = 0
)
data_Fp2 <- as.matrix(tempmean_smooth_new[,-c(1,2)]) #remove year and months column
f_data_temp <- fData(grid,t(data_Fp2))

#colony_loss_new$texas = NULL
#tempmin_smooth_new %>% select(-texas)

colnames(tempmean_smooth_new)
colnames(colony_loss_pivot)
setdiff(colnames(tempmean_smooth_new), colnames(colony_loss_pivot))

#bivariate with temp mean and colony loss abs
bivariate_data <- as.mfData(list(f_colony_loss, f_data_temp))
plot(bivariate_data)
do.call(args = lapply(1:2, function(ind)
  MHI(bivariate_data$fDList[[ind]])), what = "cor")
#--> Spearman Correlation: 0.019

#colony loss pct
colony_loss_pct <- df_final[,c(1,2,3,7)]
colony_loss_pct_pivot <- colony_loss_pct %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "colony_lost_pct",
  values_fill = 0
)
colony_loss_pct_matrix <- as.matrix(colony_loss_pct_pivot[,-c(1,2)]) #remove year and months column
f_colony_loss_pct <- fData(grid,t(colony_loss_pct_matrix))

#bivariate with temp mean and colony loss pct
bivariate_data <- as.mfData(list(f_colony_loss_pct, f_data_temp))
plot(bivariate_data)
do.call(args = lapply(1:2, function(ind)
  MHI(bivariate_data$fDList[[ind]])), what = "cor")
#--> Spearman Correlation: 0.45

#max temp
temp_max <- df_final[,c(1,2,3,17)]
temp_max_pivot <- temp_max %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "MaximumTemperature",
  values_fill = 0
)
temp_max_matrix <- as.matrix(temp_max_pivot[,-c(1,2)]) #remove year and months column
f_temp_max <- fData(grid,t(temp_max_matrix))

#min temp
tempmin_smooth <- df_final[,c(1,2,3,18)]
tempmin_smooth_new <- tempmin_smooth %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "MinimumTemperature",
  values_fill = 0
)
data_Fp5 <- as.matrix(tempmin_smooth_new[,-c(1,2)]) #remove year and months column
f_data_tempmin <- fData(grid,t(data_Fp5))

#check correlation between colony loss pct, mean temp, min temp, max temp
multivariate_data <- as.mfData(list(f_colony_loss_pct, f_data_temp, f_temp_max, f_data_tempmin))
plot(multivariate_data)
cor_spearman(multivariate_data, ordering='MHI')

#--> Spearman Correlation matrix:
#        [,1]      [,2]      [,3]      [,4]
#[1,] 1.0000000 0.4561508 0.4903346 0.3972249
#[2,] 0.4561508 1.0000000 0.9632771 0.9811693
#[3,] 0.4903346 0.9632771 1.0000000 0.9003924
#[4,] 0.3972249 0.9811693 0.9003924 1.0000000

#check correlation between colony loss pct, mean temp, min temp, max temp, precipitation

precipitations <- df_final[,c(1,2,3,20)]
prec_pivot <- precipitations %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = 'Precipitation',
  values_fill = 0
)
prec_matrix <- as.matrix(prec_pivot[,-c(1,2)])
f_prec <- fData(grid,t(prec_matrix))

multivariate_data <- as.mfData(list(f_colony_loss_pct, f_data_temp, f_temp_max, f_data_tempmin, f_prec))
plot(multivariate_data)
cor_spearman(multivariate_data, ordering='MEI') 

#Spearman correlation matrix
#         [,1]      [,2]       [,3]      [,4]        [,5]
#[1,]  1.00000000 0.3241109 0.3553618 0.2643499 -0.03560824
#[2,]  0.32411093 1.0000000 0.9688404 0.9837818  0.58896067
#[3,]  0.35536178 0.9688404 1.0000000 0.9155325  0.40713594
#[4,]  0.26434992 0.9837818 0.9155325 1.0000000  0.68301644
#[5,] -0.03560824 0.5889607 0.4071359 0.6830164  1.00000000

#different results rather than before because here I am using Modified Epigraph Index

#-------------------------------------

#correlations of colony loss pct with stressors

#varroa
varroa <- df_final[,c(1,2,3,11)]
varroa_pivot <- varroa %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Varroa.mites",
  values_fill = 0
)
varroa_matrix <- as.matrix(varroa_pivot[,-c(1,2)]) #remove year and months column
f_varroa <- fData(grid,t(varroa_matrix))

#other pests
other_pests <- df_final[,c(1,2,3,12)]
other_pests_pivot <- other_pests %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Other.pests.parasites",
  values_fill = 0
)
other_pests_matrix <- as.matrix(other_pests_pivot[,-c(1,2)]) #remove year and months column
f_other_pests <- fData(grid,t(other_pests_matrix))

#disease
disease <- df_final[,c(1,2,3,13)]
disease_pivot <- disease %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Disesases",
  values_fill = 0
)
disease_matrix <- as.matrix(disease_pivot[,-c(1,2)]) #remove year and months column
f_disease <- fData(grid,t(disease_matrix))

#pesticides
pesticides <- df_final[,c(1,2,3,14)]
pesticides_pivot <- pesticides %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Pesticides",
  values_fill = 0
)
pesticides_matrix <- as.matrix(pesticides_pivot[,-c(1,2)]) #remove year and months column
f_pesticides <- fData(grid,t(pesticides_matrix))

#other
other <- df_final[,c(1,2,3,15)]
other_pivot <- other %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Other",
  values_fill = 0
)
other_matrix <- as.matrix(other_pivot[,-c(1,2)]) #remove year and months column
f_other <- fData(grid,t(other_matrix))

#Unknown
unknown <- df_final[,c(1,2,3,16)]
unknown_pivot <- unknown %>% tidyr::pivot_wider(
  names_from = "state",
  values_from = "Unknown",
  values_fill = 0
)
unknown_matrix <- as.matrix(unknown_pivot[,-c(1,2)]) #remove year and months column
f_unknown <- fData(grid,t(unknown_matrix))

multivariate_data <- as.mfData(list(f_colony_loss_pct, f_varroa, f_other_pests,
                                    f_disease, f_pesticides, f_other, f_unknown))
plot(multivariate_data)
cor_spearman(multivariate_data, ordering='MEI') 

#--------------------------

#outliers detection: functional boxplots and outliergrams

#colony loss absolute values
#adjusted functional boxplot (amplitude outliers)
roahd::fbplot(f_colony_loss, main="Magnitude outliers", adjust = list(VERBOSE=FALSE),
       ylab = "colony loss absolute values", xlab = "time")
outliergram(f_colony_loss, adjust = list(VERBOSE=FALSE))

#outliers: 
#california      florida      georgia        idaho     michigan    minnesota      montana 
#4            7            8            9           19           20           23 
#north dakota south dakota        texas   washington 
#29           35           37           41 

#max temperature
roahd::fbplot(f_temp_max, main="Magnitude outliers", adjust = list(VERBOSE=FALSE))

#colony loss percentage
loss_pct_outliers <- invisible(outliergram(f_colony_loss_pct, display = TRUE,
                    xlab = c("time", "MEI"), ylab = c("colony loss percentage", "MBD"),
                    main = c("Shape outliers", "Outliergram")))
loss_pct_outliers$ID_outliers #new mexico

roahd::fbplot(f_colony_loss_pct, main="Magnitude outliers", adjust = list(VERBOSE=FALSE),
       ylab = "colony loss percentage", xlab = "time")

#max temperature
outliergram(f_temp_max, adjust = list(VERBOSE=FALSE)) #no results
outliergram(f_temp_max)

#varroa
roahd::fbplot(f_varroa, main="Magnitude outliers", adjust = list(VERBOSE=FALSE))
#fbplot(f_varroa$values, main="Magnitude outliers", adjust = list(VERBOSE=FALSE))
outliergram(f_varroa, adjust = list(VERBOSE=FALSE))

#pesticides
roahd::fbplot(f_pesticides, main="Magnitude outliers", adjust = list(VERBOSE=FALSE))

#----------------------------------------------------

#comparison between median functions

#colony lost pct
mbd_lost_pct <- MBD(Data = f_colony_loss_pct)
median_lost_pct <- median_fData(fData = f_colony_loss_pct, type = "MBD")
plot(median_lost_pct) #curve that has the highest value of MBD

plot(f_colony_loss_pct)
lines(grid,median_lost_pct$values) 

#varroa
mbd_varroa <- MBD(Data = f_varroa)
median_varroa <- median_fData(fData = f_varroa, type = "MBD")
plot(median_varroa) #curve that has the highest value of MBD

plot(f_varroa)
lines(grid,median_varroa$values) 

#unknown
mbd_unknown <- MBD(Data = f_unknown)
median_unknown <- median_fData(fData = f_unknown, type = "MBD")
plot(f_unknown)
lines(grid,median_unknown$values) 

#comparison between two median functions
plot(median_varroa)
lines(grid, median_lost_pct$values)

plot(median_lost_pct, ylim=c(0,60), main = "median values with Modified Band Depth",
     col = "black", ylab = "Percentages (%)", xlab = "time")
lines(grid,median_unknown$values, col = "red") 
lines(grid, median_varroa$values, col="blue")
legend(x = "topright", legend = c("colony lost pct", "unknown", "varroa"),
       col = c("black", "red", "blue"), pch=19)
#lines(grid, c(median_varroa$values[-1],0))

#---------------------------

#I should then try to smooth the curves and register them

#When is it convenient to work with registered data? Depends on the goal,
#if we want to identify phase variability (a particular pattern), then we should
#not register the data. Otherwise register the data and focus on amplitude outliers.


# # First of all we smooth the data.
# #ATTENZIONE! se giorni sulle ascisse -> t(A) in caso
# 
# data_F <- as.matrix(tempmin_smooth_new[,-c(1,2,48,49)])
# data_F[is.na(data_F)]<-0
# which(is.na(data_F))
# 
# # Choice 1: we set a high dimensional basis (interpolating)
# # Pros: no loss of information
# # Cons: possible overfitting
# nbasis <- 30
# basis <- create.fourier.basis(rangeval=c(1,90),nbasis=30)
# basimat <- eval.basis(seq(from=1,to=90),basis)
# coef <- lsfit(basimat,data_F[,1],intercept=FALSE)$coef
# Xobs <- basimat %*% coef
# matplot(Xobs)
# 
# #qua bisogna valutare la base nei punti e:
# # - cross validazione per vedere quale nbasi è migliore
# # - valutare le derivate per passarle a kma
# #creo matrice x contenente la grid di valutazione (sarànno 47stati x 30punti)
# x = matrix(rep(rep(1:30),47), ncol= 30,byrow = T)
# #creo matrice evaluation data (nObs*nDim*nPts) contenente i valori
# y = array(t(data_F), c(47,1,30))
# res <- kma(
#   x,
#   y,
#   n_clust = 5,
#   center_method = "medoid",
#   warping_method = "affine",
#   dissimilarity_method = "pearson"
# )
# plot(res,type="data")
# 
# #in ogni caso una volta che ho smoothing ho coefs e basis -> per ottenere valore in un punto basis%*%coefs
# eval.basis(time,basis)
# data_F.fd <- Data2fd(y = data_F,argvals = time, basisobj = basis)
# plot.fd(data_F.fd)

#colony_loss_pct_matrix

m <- 3 # spline order 
degree <- m-1 # spline degree 
abscissa <- seq(1,29) #or seq(from, to, by)
NT <- length(abscissa)
#colony_loss_pct_matrix: collection of functions AS COLUMNS

####### generalized cross-validation to find nbasis (work just on 1 datum)
nbasis <- 5:29
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(rangeval=range(abscissa), nbasis[i], m)
  gcv[i] <- smooth.basis(abscissa, colony_loss_pct_matrix[,1], basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis <- nbasis[which.min(gcv)] #number of basis functions --> 5

basis <- create.bspline.basis(rangeval=range(abscissa), nbasis, norder=m)
# If breaks are not provided, equally spaced knots are created
basismat <- eval.basis(abscissa, basis) #design matrix
# Fit via LS
#colony_loss_pct_smooth: dataframe with all the curves smoothing the data as columns
colony_loss_pct_smooth <- basismat %*% lsfit(basismat, colony_loss_pct_matrix, intercept=FALSE)$coef
colnames(colony_loss_pct_smooth) <- colnames(colony_loss_pct_matrix)

f_colony_loss_pct_smooth <- fData(grid,t(colony_loss_pct_smooth))
plot(f_colony_loss_pct_smooth, main="colony loss pct") 

#data_F.fd <- Data2fd(y = colony_loss_pct_smooth,argvals = grid, basisobj = basis)
#plot.fd(data_F.fd)

loss_pct_outliers <- invisible(outliergram(f_colony_loss_pct_smooth))
loss_pct_outliers$ID_outliers #phase outliers --> missouri and new mexico


