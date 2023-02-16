

###############################################################################.
####     SPACE TIME MODEL WITH A MANUAL MESH, TARGET = COLONY LOSS PCT     ####
###############################################################################.

#NO COVARIATES!

#load already computed model
#load("/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/smoothing_iso_temp_06022022.Rdata")

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")

#time: from 2015 Q1 to 2022 Q2, without 2019 Q2
times <- unique(df[,c("year", "months")])
rownames(times) <- NULL
time_locations = seq(0,1,length.out = dim(times)[1])

df_st <- df[,c(1,2,3,7)] 
df_st <- df_st[df_st$state != "hawaii" & df_st$state != "other states",]

data_obs <- df_st

data <- data_obs %>% tidyr::pivot_wider(
  names_from = c("year", "months"), 
  values_from = colony_lost_pct,
  values_fill = NULL
)

col_order <- order(colnames(data))
col_order <- c(1,head(col_order,-1))
data <- data[,col_order]

boundary_all <- read.table("code/Lupo/boundary.txt", header=TRUE)

#boundary <- data.frame(
#  long = c(min(boundary_all$long), max(boundary_all$long), max(boundary_all$long), min(boundary_all$long)),
#  lat = c(max(boundary_all$lat), max(boundary_all$lat), min(boundary_all$lat), min(boundary_all$lat)))

#from top left --> top right --> bottom right --> bottom left
#long <- c(min(boundary_all$long), -95, -85, -80, max(boundary_all$long), max(boundary_all$long), -77,
#          -80, -83, -84, -95, -97, min(boundary_all$long))
#lat <- c(max(boundary_all$lat), max(boundary_all$lat), 47, 43, max(boundary_all$lat), 40, 32,
#         min(boundary_all$lat), min(boundary_all$lat), 28, 28, 25, 35)
#boundary <-  data.frame( long=long, lat=lat)

#Boundary Gege:
boundary <- read.table("/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Eugenio/boundary_gg.txt", head=T)

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove hawaii:
df_coord <- df_coord[df_coord$state != "hawaii",]
#remove Na (for "other states"):
df_coord <- na.omit(df_coord)

df_new <- merge(data, df_coord, by = "state") 

#remove column 'state' from data:
data <- data[,-1]
data_loss <- matrix(as.numeric(unlist(data)),nrow=nrow(data))

data_locations <- matrix(NA,nrow=dim(df_new)[1],ncol=2)
data_locations[,1] <- as.numeric(df_new[,31])
data_locations[,2] <- as.numeric(df_new[,32])

# create the matrix p with all the data locations and boundary locations
p <- matrix(data=NA,nrow=dim(data_locations)[1]+dim(boundary)[1],ncol=2)
p[,1] <- c(data_locations[,1],boundary[,1])
p[,2] <- c(data_locations[,2],boundary[,2])
plot(p,pch=16,cex=0.4,col=c(rep('red',dim(data_locations)[1]),rep('black',dim(boundary)[1])))
#plot(st_geometry(orotl_sf), add=T)

# create the boundary segments (each row corresponds to an edge, that goes from
# the vertex in the first column to the one in the second column)
isboundary <- matrix(data=NA,nrow=dim(boundary)[1],ncol=2)
isboundary[,1] <- (dim(data)[1]+1):(dim(data)[1]+dim(boundary)[1])
isboundary[,2] <- c((dim(data)[1]+2):(dim(data)[1]+dim(boundary)[1]),(dim(data)[1]+1))

# create the mesh
mesh_1 <- create.mesh.2D(p, order = 1, segments = isboundary)
mesh <- refine.mesh.2D(mesh_1, maximum_area=1.5, minimum_angle = 30)
basisobj <- create.FEM.basis(mesh)

#plot the mesh
shapefile <- "/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp"
orotl_sf <- st_read(shapefile)
par(mar=c(0,0,0,0))
plot(mesh,asp=1, pch=".")
box()
points(mesh$nodes[which(mesh$nodesmarkers==0),], pch=16,cex=0.6)
points(mesh$nodes[which(mesh$nodesmarkers==1),], pch=16, col='red',cex=0.6)
points(p,pch=16,cex=1.5,col=c(rep('blue',dim(data_locations)[1]),rep('orange',dim(boundary)[1])))
plot(st_geometry(orotl_sf), width = 3, add=T) #col="blue"

lambdaS=10^-1
lambdaT=10^-1

start_time <- Sys.time()
smoothing_temp_iso <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                      observations=data_loss, 
                                      FEMbasis=basisobj,
                                      #lambdaS=lambdaS,
                                      #lambdaT=lambdaT
                                      lambda.selection.criterion='newton_fd', 
                                      lambda.selection.lossfunction='GCV',
                                      DOF.evaluation='stochastic')
end_time <- Sys.time()
final_time = end_time - start_time

lambdaS <- smoothing_temp_iso$optimization$lambda_solution[1]
lambdaT <- smoothing_temp_iso$optimization$lambda_solution[2]
#lambdaS and lambdaT equals to the last row of matrix: smoothing_temp_iso$optimization$lambda_vector

#save(smoothing_temp_iso, file ="code/Lupo/smoothing_iso_temp_06022022.Rdata")

#does not work!
open3d()
mfrow3d(3, 1, byrow = TRUE, sharedMouse = FALSE)
plot(smoothing_temp_iso$fit.FEM.time, time_locations=time_locations) #axes=FALSE
#axes3d(col='white')

#does not work!
open3d()
mfrow3d(3, 2, byrow = T)
for (i in 1:6) {
  plot(smoothing_temp_iso$fit.FEM.time, time_locations=time_locations) 
}
highlevel(integer()) # To trigger display as rglwidget

plot(smoothing_temp_iso$fit.FEM.time, locations=data_locations[4,])

#play3d(spin3d())
#play3d(spin3d(axis = c(1,0,0), rpm = 4), duration = 10)

coeffs <- smoothing_temp_iso$fit.FEM.time$coeff
range(coeffs)

time_instant <- 5 #from 1 to 29
plot(smoothing_temp_iso$fit.FEM.time, time_locations=time_locations[time_instant]) #lambdaS = 1, lambdaT = 1

plot(smoothing_temp_iso$fit.FEM.time, time_locations=time_locations)

###############################################################################.
##########                  ADD COVARIATES                          ##########
###############################################################################.

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")
df <- df[df$state != "hawaii" & df$state != "other states",]

#Varroa.mites = 11, Other pests = 12, Diseases = 13, #Pesticides = 14
#Other = 15, Unknown = 16, Max Temp = 17, Min Temp = 18, Precipitation = 20
#Avg temp = 21
df_covariates <- df[,c(1,2,3,11,12,13,14,15,16,17,18,20,21)]

#First alternative to build covariates matrix
ncov <- 6
covariates <- matrix(NA,nrow=dim(df_covariates)[1],ncol=ncov)
covariates[,1] <- as.numeric(df_covariates[,4]) #varroa
covariates[,2] <- as.numeric(df_covariates[,7]) #pesticides
covariates[,3] <- as.numeric(df_covariates[,8]) #other
covariates[,4] <- as.numeric(df_covariates[,9]) #unknown
covariates[,5] <- as.numeric(df_covariates[,11]) #min temp
covariates[,6] <- as.numeric(df_covariates[,12]) #precipitation

# CHECK THE COVARIATE MATRIX IS IN THE RIGHT ORDER OF TIME AND SPACE: here
#the order is with time t associated to all the states, followed by time t+1
#associated to all the states...

#smoothing_temp_cov_iso and smoothing_temp_cov_iso2 are the models with covariates
#rows ordered as described above
#load("/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/smoothing_temp_cov_iso_VarroaDisPest_08022022.Rdata")

############################################################################.

# Let's try with the covariate matrices inverted, meaning that, on the rows,
# the data is listed with a state and all the possible times, followed
# by the next state in alphabetical order with all the times, ...
# (THIS SHOULD BE THE RIGHT APPROACH)

df_covariates <- df_covariates %>% arrange(state, year, months)
ncov <- 3
covariates <- matrix(NA,nrow=dim(df_covariates)[1],ncol=ncov)
covariates[,1] <- as.numeric(df_covariates[,4]) #varroa
covariates[,2] <- as.numeric(df_covariates[,7]) #pesticides
covariates[,3] <- as.numeric(df_covariates[,8]) #other
#covariates[,4] <- as.numeric(df_covariates[,9]) #unknown
#covariates[,5] <- as.numeric(df_covariates[,11]) #min temp
#covariates[,6] <- as.numeric(df_covariates[,12]) #precipitation

#smoothing_temp_cov_iso3 is the model with covariates and rows ordered as described above
#load("/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/smoothing_temp_covinv_iso_VarroaPestOther_09022022.Rdata")

############################################################################.

#QUESTION: does the matrix of covariates, in the rows, should have first the same state for each time
#or first each time followed by all the state ?????

start_time <- Sys.time()
smoothing_temp_cov_iso <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                          observations=data_loss, 
                                          FEMbasis=basisobj,
                                          covariates = covariates,
                                          lambda.selection.criterion='newton_fd', 
                                          lambda.selection.lossfunction='GCV',
                                          DOF.evaluation='stochastic')
end_time <- Sys.time()
final_time = end_time - start_time


#save(smoothing_temp_cov_iso , file ="code/Lupo/smoothing_temp_cov_iso_VarroaDisPest_08022022.Rdata")
#save(smoothing_temp_cov_iso2 , file ="code/Lupo/smoothing_temp_cov_iso_VarroaPestOtherUnknMinTempPrec_09022022.Rdata")
#save(smoothing_temp_cov_iso3 , file ="code/Lupo/smoothing_temp_covinv_iso_VarroaPestOther_09022022.Rdata")

smoothing_temp_cov_iso$solution$rmse
f <- smoothing_temp_cov_iso$solution$f
g <- smoothing_temp_cov_iso$solution$g

smoothing_temp_cov_iso$beta
smoothing_temp_cov_iso$optimization$lambda_solution
smoothing_temp_cov_iso$optimization$GCV_vector

#3d plot
time_instant <- 5 #from 1 to 29
plot(smoothing_temp_cov_iso$fit.FEM.time, time_locations=time_locations[time_instant])
points3d(data_locations[,1], data_locations[,2], data_loss[,time_instant], cex=1, col="black")

plot(smoothing_temp_cov_iso$fit.FEM.time, time_locations=time_locations[time_instant])

states <- df_new$state
text3D(data_locations[,1], data_locations[,2], data_loss[,time_instant], states,
       colvar = data_loss[,time_instant], col = gg.col(100), theta = 60, phi = 20,
       xlab = "Longitude", ylab = "Latitude", zlab = "Colony Loss pct", 
       main = "Colony Loss Percentage in USA at t=time_instant", cex = 0.6,
       ticktype = "detailed", font = 2) #bty = "g", d = 2, adj = 0.5

rgl.postscript("....pdf", fmt="pdf")

#2d plot
image(smoothing_temp_cov_iso$fit.FEM.time, t=time_locations[c(1,10,29)])

#other 2d plot
source("plotFEM2d_time.R")
par(mfrow=c(1,1))
time_instant <- 5
zlim <- range(data_loss[,time_instant])
levels <- seq(from=min(data_loss[,time_instant]),to=max(data_loss[,time_instant]),length=10)
plotFEM2d_time(smoothing_temp_cov__iso$fit.FEM.time, time=time_locations[time_instant],
               zlim=zlim,levels=levels,
               xlim=range(boundary[,1])+c(-0.5,0.5),ylim=range(boundary[,2])+c(-0.5,0.5),
               asp=1,xlab='',ylab='',xaxt="n",yaxt="n", Nx=200, Ny=200)
plot(st_geometry(orotl_sf), lwd=3, add=T)

#### function handwritten
SolutionObj <- smoothing_temp_cov_iso$fit.FEM.time
time_instants <- c(1,10,20,29) #change here!
xlim <- range(boundary[,1])+c(-0.5,0.5)
ylim <- range(boundary[,2])+c(-0.5,0.5)
Nx <- 200
Ny <- 200
zlim <- c(0,20)

xmin <- xlim[1]
xmax <- xlim[2]
ymin <- ylim[1]
ymax <- ylim[2]

X <- matrix(seq(xmin, xmax, len=Nx),ncol=1)
Y <- matrix(seq(ymin, ymax, len=Ny),ncol=1)    

Xmat <- X %*% matrix(1,nrow=1,ncol=Ny)
Ymat <- matrix(1,nrow=Nx,ncol=1) %*% t(Y)
Xvec <- NULL
for (numc in 1:Ny)
{
  Xvec <- c(Xvec,Xmat[,numc])
}
Yvec <- NULL
for (numc in 1:Ny)
{
  Yvec <- c(Yvec,Ymat[,numc])
}

par(mfrow=c(2,2)) #change here!
for (t in time_instants){
  eval_points <- cbind(Xvec, Yvec)
  eval_sol <- rep(NA,nrow(eval_points))
  eval_sol <- eval.FEM.time(SolutionObj, locations = eval_points, time.instants=time_locations[t])
  evalmat <- matrix(eval_sol, nrow=Nx, ncol=Ny, byrow=F)
  #zlim <- range(data_loss[,t])
  levels <- seq(from=min(data_loss[,t]),to=max(data_loss[,t]),length=10)
  
  image(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, xlab="longitude",
        ylab="latitude", main = paste("Colony loss percentage in US at year =", times[t,1],  times[t,2]))
  contour(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, add=T,colkey=F,col="black",levels=levels)
  plot(st_geometry(orotl_sf), width=2, add=T)
}

#outcome: adding the covariates in the case of colony_loss_pct does not change that
#much in terms of rmse, and the estimate values of betas parameter are all really
#low, around 0.001 / 0.005. Moreover, GCV around 46-48.
#case with covariates are the models: smoothing_temp_cov_iso, smoothing_temp_cov_iso2,
#smoothing_temp_cov_iso3

#The case without covariates, i.e. smoothing_temp_iso has GCV around 48 and rmse
#around 6.38

#plots at a given location
index_loc <- 25
data_locations[index_loc,]
state <- df_coord[index_loc,"state"]
plot(smoothing_temp_cov_iso$fit.FEM.time, locations=data_locations[index_loc,],
     main = paste("state = ", state), xlim = c(0,1))
     #ylim = range(data_loss[index_loc,]))
     #xlab = "time (t)", ylab = "colony loss percentage", decorate=T)
points(time_locations, data_loss[index_loc,], pch = 19, cex = 0.5)
data_loss[index_loc,]


###############################################################################.
######                          LAMBDA GRID                            #########
###############################################################################.

smoothing_temp_cov_iso$solution$rmse
smoothing_temp_cov_iso$optimization$lambda_solution
smoothing_temp_cov_iso$optimization$GCV_vector

#lambdaS <- c(1e-4, 1e-3, 1e-2, 1e-1, 1, 5)
#lambdaT <- c(1, 5)
lambdaS <- c(1e-2)
lambdaT <- c(1e-3)
start_time <- Sys.time()
smoothing_temp_cov_lambda <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                             observations=data_loss, 
                                             FEMbasis=basisobj,
                                             covariates = covariates,
                                             lambda.selection.criterion='grid',
                                             lambda.selection.lossfunction='GCV',
                                             lambdaS = lambdaS,
                                             lambdaT = lambdaT)
end_time <- Sys.time()
final_time = end_time - start_time

smoothing_temp_cov_lambda$solution$rmse
smoothing_temp_cov_lambda$optimization
smoothing_temp_cov_lambda$optimization$GCV_vector

#lambda solution --> lambdaS = 0.1, lambdaT = 0.5
#GCV = 47.70, rmse = 6.49

#3d plot
time_instant <- 5 #from 1 to 29
plot(smoothing_temp_cov_lambda$fit.FEM.time, time_locations=time_locations[time_instant])
points3d(data_locations[,1], data_locations[,2], data_loss[,time_instant], cex=1, col="black")

#index at given location
index_loc <- 25
data_locations[index_loc,]
state <- df_coord[index_loc,"state"]
plot(smoothing_temp_cov_lambda$fit.FEM.time, locations=data_locations[index_loc,],
     main = paste("state = ", state), xlim = c(0,1))
#ylim = range(data_loss[index_loc,]))
#xlab = "time (t)", ylab = "colony loss percentage", decorate=T)
points(time_locations, data_loss[index_loc,], pch = 19, cex = 0.5)
data_loss[index_loc,]

#######         Write the percentage as values between 0 and 1         #########

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")
df <- df[df$state != "hawaii" & df$state != "other states",]
cols <- c("colony_lost_pct", "Varroa.mites", "Other.pests.parasites", "Disesases", "Pesticides", "Other", "Unknown")
df[cols] <- lapply(df[cols], function(x) x/100)

data_obs <- df[,c(1,2,3,7)]

data <- data_obs %>% tidyr::pivot_wider(
  names_from = c("year", "months"), 
  values_from = colony_lost_pct,
  values_fill = NULL
)

col_order <- order(colnames(data))
col_order <- c(1,head(col_order,-1))
data <- data[,col_order]
data <- data[,-1] #remove column 'state' from data:
data_loss_pct <- matrix(as.numeric(unlist(data)),nrow=nrow(data))

#here the order is with time t associated to all the states, followed by time t+1
#associated to all the states...
df_covariates <- df[,c(1,2,3,11,14,15)]

#second alternative to build covariates matrix
ncov <- 3
covariates <- matrix(NA,nrow=dim(df_covariates)[1],ncol=ncov)
covariates[,1] <- as.numeric(df_covariates[,4]) #varroa
covariates[,2] <- as.numeric(df_covariates[,5]) #pesticides
covariates[,3] <- as.numeric(df_covariates[,6]) #other

lambdaS <- c(1e-4, 1e-3, 1e-2, 1e-1, 1, 5, 10)
#lambdaS <- c(1e-4)
lambdaT <- c(1e-1, 1, 5)
start_time <- Sys.time()
smoothing_temp_pct_cov <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                          observations=data_loss_pct, 
                                          FEMbasis=basisobj,
                                          covariates = covariates,
                                          lambda.selection.criterion='grid',
                                          lambda.selection.lossfunction='GCV',
                                          lambdaS = lambdaS,
                                          lambdaT = lambdaT)
end_time <- Sys.time()
final_time = end_time - start_time

smoothing_temp_pct_cov$solution$rmse
smoothing_temp_pct_cov$optimization
smoothing_temp_pct_cov$optimization$GCV_vector

#lambda solution --> lambdaS = 1, lambdaT = 5
#GCV = 0.0042, rmse = 0.063

#3d plot
time_instant <- 5 #from 1 to 29
plot(smoothing_temp_pct_cov$fit.FEM.time, time_locations=time_locations[time_instant])
points3d(data_locations[,1], data_locations[,2], data_loss_pct[,time_instant], cex=1, col="black")

#save(smoothing_temp_pct_cov, file ="code/Lupo/smoothing_temp_losspct_VarroaPestOther_09022022.Rdata")

## PROVA SETTANDO MANUALMENTE I VALORI DI LAMBDA

### TRY WITH COLONY LOSS AS TARGET INSTEAD OF COLONY LOSS PCT

# PROVA A CONVERTIRE LE COORDINATE IN UTM DA LAT-LONG

# PROVA WITH COLONY LOSS AS PERCENTAGE LIKE VALUES BETWEEN 0 AND 1 AND NOT BETWEEN
# 0 AND 100

# TRY TO SCALE LOCATION POSITIONS AND TARGET COVARIATES

##############################################################################.
######       not needed process of covariates as tensor of matrices      #######

#varroa.mites
data_varroa <- df_covariates[,c(1,2,3,4)] %>% tidyr::pivot_wider(
  names_from = c("year", "months"), 
  values_from = Varroa.mites,
  values_fill = NULL
)

col_order <- order(colnames(data_varroa))
col_order <- c(1,head(col_order,-1))
data_varroa <- data_varroa[,col_order]
data_varroa <- data_varroa[,-1] #remove column State
data_varroa <- matrix(as.numeric(unlist(data_varroa)),nrow=nrow(data_varroa))

#Diseases
data_diseases <- df_covariates[,c(1,2,3,5)] %>% tidyr::pivot_wider(
  names_from = c("year", "months"), 
  values_from = Disesases,
  values_fill = NULL
)

col_order <- order(colnames(data_diseases))
col_order <- c(1,head(col_order,-1))
data_diseases <- data_diseases[,col_order]
data_diseases <- data_diseases[,-1] #remove column State
data_diseases <- matrix(as.numeric(unlist(data_diseases)),nrow=nrow(data_diseases))

#Pesticides
data_pesticides <- df_covariates[,c(1,2,3,6)] %>% tidyr::pivot_wider(
  names_from = c("year", "months"), 
  values_from = Pesticides,
  values_fill = NULL
)

col_order <- order(colnames(data_pesticides))
col_order <- c(1,head(col_order,-1))
data_pesticides <- data_pesticides[,col_order]
data_pesticides <- data_pesticides[,-1] #remove column State
data_pesticides <- matrix(as.numeric(unlist(data_pesticides)),nrow=nrow(data_pesticides))

#tensor of covariates
new_m <- cbind(data_varroa, data_diseases, data_pesticides)
dim(new_m) <- c(nrow(data_loss), ncol(data_loss), 3) #first matrix: varroa, second matrix: diseases

###############################################################################.
