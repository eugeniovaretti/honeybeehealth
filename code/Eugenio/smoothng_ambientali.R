library(mvtnorm)
library(rgl)
library(car)
# # 
# groups <- levels(factor(stateraw$state))
# temp_smooth <- stateraw[,c(1,2,4,8)]
# tempmin_smooth_new <- temp_smooth %>% tidyr::pivot_wider(
#   names_from = state,
#   values_from = MinimumTemperature,
#   values_fill = 0
# )
# 
# library(fda)
# # First of all we smooth the data. We choose a Fourier basis
# # (periodic). We need to set the dimension of the basis
# 
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
# library(fdacluster)
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
# 
# ### AVERAGE
# temp_smooth <- readstate[,c(1,2,7,8)]
# tempmin_smooth_new <- temp_smooth %>% tidyr::pivot_wider(
#   names_from = state, 
#   values_from = AverageTemperature,
#   values_fill = 0
# )
# 
# library(fda)
# # First of all we smooth the data. We choose a Fourier basis
# # (periodic). We need to set the dimension of the basis
# 
# #ATTENZIONE! se giorni sulle ascisse -> t(A) in caso
# 
# time <- 1:30
# 
# data_F <- as.matrix(tempmin_smooth_new[,-c(1,2,48,49)])
# data_F[is.na(data_F)]<-0
# which(is.na(data_F))
# 
# # Choice 1: we set a high dimensional basis (interpolating)
# # Pros: no loss of information
# # Cons: possible overfitting 
# nbasis <- 30
# basis <- create.fourier.basis(rangeval=c(1,30),nbasis=30) 
# data_F.fd <- Data2fd(y = data_F,argvals = time, basisobj = basis)
# plot.fd(data_F.fd)
# 
# 
# 
# 
# 
# 
# prec_smooth <- readstate[,c(1,2,6,8)]
# prec_smooth_new <- prec_smooth %>% tidyr::pivot_wider(
#   names_from = state, 
#   values_from = Precipitation,
#   values_fill = 0
# )
# 
# time <- 1:30
# 
# data_Fp <- as.matrix(prec_smooth_new[,-c(1,2,48,49)])
# data_Fp <- data_Fp[,-42 ]
# data_F[is.na(data_Fp)]<-0
# which(is.na(data_Fp))
# 
# # Choice 1: we set a high dimensional basis (interpolating)
# # Pros: no loss of information
# # Cons: possible overfitting 
# nbasis <- 30
# basis <- create.fourier.basis(rangeval=c(1,30),nbasis=30) 
# data_Fp.fd <- Data2fd(y = data_Fp,argvals = time, basisobj = basis)
# plot.fd(data_Fp.fd)
# 
# 
# 
# 
# library(fdacluster)
# #qua bisogna valutare la base nei punti e:
# # - cross validazione per vedere quale nbasi è migliore
# # - valutare le derivate per passarle a kma
# #creo matrice x contenente la grid di valutazione (sarànno 47stati x 30punti)
# x = matrix(rep(rep(1:30),46), ncol= 30,byrow = T)
# #creo matrice evaluation data (nObs*nDim*nPts) contenente i valori
# y = array(t(data_Fp), c(46,1,30))
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
# 



###PROVO CON RAW DATA ##########
source("code/utils/read_statedata.R")
source("code/utils/read_rawstatedata.R")
library(fdacluster)
library(weathermetrics)
readstate <- read_statedata() 
stateraw <- read_rawstatedata() 

groups <- levels(factor(stateraw$state))
set.seed(202212)
cluster_index<- cbind(state=groups)

########cluster by temp min#######
temp_min <- stateraw[,c(1,2,4,8)]
temp_min <- temp_min %>% tidyr::pivot_wider(
  names_from = state, 
  values_from = MinimumTemperature,
  values_fill = 0
)

data_F <- as.matrix(temp_min[,-c(1,2)])
which(is.na(data_F))

#creo matrice x contenente la grid di valutazione (sarànno 47stati x 30punti)
x = matrix(rep(rep(1:90),49), ncol= 90,byrow = T)
#creo matrice evaluation data (nObs*nDim*nPts) contenente i valori
y = array(t(data_F), c(49,1,90))
res <- kma(
  x,
  y,
  n_clust = 3,
  center_method = "medoid",
  warping_method = "affine",
  dissimilarity_method = "pearson"
)

plot(res,type="data",colors(c("green","blue","red")))


#cluster_index <- cbind(cluster_index, temp_min=res$labels)

library(usmap)
plot_usmap(data = df) + labs(title = "Cluster by temp min")



########### cluster by temp max ##############
temp_max <- stateraw[,c(1,2,3,8)]
temp_max <- temp_max %>% tidyr::pivot_wider(
  names_from = state, 
  values_from = MaximumTemperature,
  values_fill = 0
)

data_F <- as.matrix(temp_max[,-c(1,2)])
which(is.na(data_F))

#creo matrice x contenente la grid di valutazione (sarànno 47stati x 30punti)
x = matrix(rep(rep(1:90),49), ncol= 90,byrow = T)
#creo matrice evaluation data (nObs*nDim*nPts) contenente i valori
y = array(t(data_F), c(49,1,90))
res <- kma(
  x,
  y,
  n_clust = 3,
  center_method = "medoid",
  warping_method = "affine", #forse "none" è meglio
  dissimilarity_method = "pearson"
)
plot(res,type="data")
# plot cluster
library(plotly)
us_data <- map_data("state")
df <- data.frame(
  state = tolower(groups),
  values = res$labels
)
cluster_index <- cbind(cluster_index, temp_max=res$labels)
library(usmap)
plot_usmap(data = df) + labs(title = "Cluster by tempmax")

##############cluster by temp_avg###############
temp_avg <- stateraw[,c(1,2,7,8)]
temp_avg <- temp_avg %>% tidyr::pivot_wider(
  names_from = state, 
  values_from = AverageTemperature,
  values_fill = 0
)

data_F <- as.matrix(temp_avg[,-c(1,2)])
which(is.na(data_F))

#creo matrice x contenente la grid di valutazione (sarànno 47stati x 30punti)
x = matrix(rep(rep(1:90),49), ncol= 90,byrow = T)
#creo matrice evaluation data (nObs*nDim*nPts) contenente i valori
y = array(t(data_F), c(49,1,90))
res <- kma(
  x,
  y,
  n_clust = 3,
  center_method = "medoid",
  warping_method = "affine",
  dissimilarity_method = "pearson"
)


# plot cluster
library(plotly)
us_data <- map_data("state")
df <- data.frame(
  state = tolower(groups),
  values = res$labels
)
cluster_index <- cbind(cluster_index, temp_avg=res$labels)
library(usmap)
plot_usmap(data = df) + labs(title = "Cluster by average temp")

##############cluster by prec#####################à
prec <- stateraw[,c(1,2,6,8)]
prec <- prec %>% tidyr::pivot_wider(
  names_from = state, 
  values_from = Precipitation,
  values_fill = 0
)

data_F <- as.matrix(prec[,-c(1,2)]) #42+2 è il texas
which(is.na(data_F))

#creo matrice x contenente la grid di valutazione (sarànno 47stati x 30punti)
x = matrix(rep(rep(1:90),49), ncol= 90,byrow = T)
#creo matrice evaluation data (nObs*nDim*nPts) contenente i valori
y = array(t(data_F), c(49,1,90))
res <- kma(
  x,
  y,
  n_clust = 4,
  center_method = "medoid",
  warping_method = "none",#ha senso se no trasformare le curve?? shift+dilataz no
  dissimilarity_method = "pearson"
)
plot(res,type="data")


# plot cluster prec
library(plotly)
us_data <- map_data("state")
df <- data.frame(
  state = tolower(groups),
  values = res$labels
)
#cluster_index <- cbind(cluster_index, prec=res$labels)
library(usmap)
plot_usmap(data = df) + labs(title = "Cluster by prec")
cluster_idxf <- cbind(cluster_idxf, prec_warpnone = res$labels)


##############cluster by PDSI#####################à
pdsi <- stateraw[,c(1,2,5,8)]
pdsi <- pdsi %>% tidyr::pivot_wider(
  names_from = state, 
  values_from = PalmerDroughtSeverityIndexPDSI,
  values_fill = 0
)

data_F <- as.matrix(pdsi[,-c(1,2,4)]) #2+2 è Alaska
which(is.na(data_F))

#creo matrice x contenente la grid di valutazione (sarànno 47stati x 30punti)
x = matrix(rep(rep(1:90),48), ncol= 90,byrow = T)
#creo matrice evaluation data (nObs*nDim*nPts) contenente i valori
y = array(t(data_F), c(48,1,90))
res <- kma(
  x,
  y,
  n_clust = 3,
  center_method = "medoid",
  warping_method = "none",
  dissimilarity_method = "pearson"
)
plot(res,type="data")

# plot cluster pdsi
library(plotly)
us_data <- map_data("state")
df <- data.frame(
  state = tolower(groups[-2]),
  values = res$labels
)
library(usmap)
plot_usmap(data = df) + labs(title = "Cluster by pdsi")

#queste 3 righe servono per creazione cluster_idx, metto null a Alaska
pdsi_lab <- append(res$labels, NA, 1)
cluster_index <- cbind(cluster_index, pdsi=pdsi_lab)



