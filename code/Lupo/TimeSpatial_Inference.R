
rm(list=ls())
graphics.off()
set.seed(1)

library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(np)
library(fda)
library(dplyr)

library(RColorBrewer)
library(sf)
library(fdaPDE)
library(viridis)
library(latex2exp)


###############################################################################.
####     SPACE TIME MODEL WITH A MANUAL MESH, TARGET = COLONY LOSS PCT     ####
####               INFERENCE ON COVARIATES PARAMETERS                      ####
###############################################################################.

####### MODEL WITH COVARIATES #######.

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

boundary <- read.table("code/Eugenio/boundary_gg.txt", head=T)

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
shapefile <- "code/Lupo/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp"
orotl_sf <- st_read(shapefile)
par(mar=c(0,0,0,0))
plot(mesh,asp=1, pch=".")
box()
points(mesh$nodes[which(mesh$nodesmarkers==0),], pch=16,cex=0.6)
points(mesh$nodes[which(mesh$nodesmarkers==1),], pch=16, col='red',cex=0.6)
points(p,pch=16,cex=1.5,col=c(rep('blue',dim(data_locations)[1]),rep('orange',dim(boundary)[1])))
plot(st_geometry(orotl_sf), width = 3, add=T) #col="blue"

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
ncov <- 4
covariates <- matrix(NA,nrow=dim(df_covariates)[1],ncol=ncov)
covariates[,1] <- as.numeric(df_covariates[,4]) #varroa
covariates[,2] <- as.numeric(df_covariates[,7]) #pesticides
covariates[,3] <- as.numeric(df_covariates[,8]) #other
covariates[,4] <- as.numeric(df_covariates[,9]) #unknown
#covariates[,5] <- as.numeric(df_covariates[,11]) #min temp
#covariates[,6] <- as.numeric(df_covariates[,12]) #precipitation

# CHECK THE COVARIATE MATRIX IS IN THE RIGHT ORDER OF TIME AND SPACE: here
#the order is with time t associated to all the states, followed by time t+1
#associated to all the states...

#QUESTION: does the matrix of covariates, in the rows, should have first the same state for each time
#or first each time followed by all the state ?????

obj_inf <- inferenceDataObjectBuilder(test = 'sim', type = 'esf', component = "parametric",
                                      dim = 2, n_cov = ncov, beta0 = rep(0,ncov))

start_time <- Sys.time()
smooth_temp_inference <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                          observations=data_loss, 
                                          FEMbasis=basisobj,
                                          covariates = covariates,
                                          lambda.selection.criterion='newton_fd', 
                                          lambda.selection.lossfunction='GCV',
                                          DOF.evaluation='stochastic',
                                          inference.data.object = obj_inf)
end_time <- Sys.time()
final_time = end_time - start_time

smooth_temp_inference$solution$rmse
smooth_temp_inference$beta
smooth_temp_inference$optimization$lambda_solution
smooth_temp_inference$optimization$GCV_vector
smooth_temp_inference$inference$beta

#3d plot
time_instant <- 5 #from 1 to 29
plot(smoothing_temp_cov_iso$fit.FEM.time, time_locations=time_locations[time_instant])
points3d(data_locations[,1], data_locations[,2], data_loss[,time_instant], cex=1, col="black")

plot(smoothing_temp_cov_iso$fit.FEM.time, time_locations=time_locations[time_instant])

rgl.postscript("....pdf", fmt="pdf")

time_instant <- 5 #from 1 to 29
plot(smoothing_temp_iso$fit.FEM.time, time_locations=time_locations[time_instant]) #lambdaS = 1, lambdaT = 1

plot(smoothing_temp_iso$fit.FEM.time, time_locations=time_locations)

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


########

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

