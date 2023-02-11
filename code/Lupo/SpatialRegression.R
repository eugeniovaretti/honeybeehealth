
rm(list=ls())
graphics.off()
set.seed(1)
setwd("~/Desktop/NP_project/NP_github/np_project/")

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
#### SPATIAL REGRESSION WITH A MANUAL MESH ####
###############################################################################.

#Let's remove the time dependency considering only 1 year and 1 trimester

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")

df_spatial <- df[df$year==2019 & df$months =="Q1",]

#remove 'hawaii' and 'other states':
df_spatial <- df_spatial[df_spatial$state != "hawaii" & df_spatial$state != "hawaii",]

#average colony loss percentage over a time window
#df_2016 <- df_2016[,c(1,3,7)]
#df_new <- df_2016 %>% group_by(state, year) %>% summarize(loss_avg = mean(colony_lost_pct))

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove 'hawaii' and remove Na (for "other states"):
df_coord <- df_coord[df_coord$state != "hawaii",]
df_coord <- na.omit(df_coord)

df_final <- merge(df_spatial, df_coord, by = "state") #dataset without hawaii and other states

#order by state and remove columns of the states and given year and trimester
df_final <- df_final %>% arrange(state, year, months)
df_final <- df_final[,-c(1,2,3)]

#shapefile for US boundaries
shapefile <- "/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp"
orotl_sf <- st_read(shapefile)

#boundary points:
boundary <- read.table("/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Eugenio/boundary_gg.txt", head=T)

#target variable: colony loss pct
data <- as.numeric(df_final[,4]) #/scale_data
range(data)

#data locations matrix (in long-lat coordinates)
data_locations <- matrix(NA,nrow=dim(df_final)[1],ncol=2)
data_locations[,1] <- as.numeric(df_final[,19])
data_locations[,2] <- as.numeric(df_final[,20])

#covariates matrix
#ncvov <- 
#covariates <- matrix(NA,nrow=dim(df_final)[1],ncol=ncov)
#covariates[,1] <- as.numeric(df_final[,4])
#covariates[,2] <- as.numeric(df_final[,5])
#covariates[,3] <- as.numeric(df_final[,6])
#covariates[,4] <- as.numeric(df_final[,7])

# create the matrix p with all the data locations and boundary locations
p <- matrix(data=NA,nrow=dim(data_locations)[1]+dim(boundary)[1],ncol=2)
p[,1] <- c(data_locations[,1],boundary[,1])
p[,2] <- c(data_locations[,2],boundary[,2])
plot(p,pch=16,cex=0.4,col=c(rep('red',dim(data_locations)[1]),rep('black',dim(boundary)[1])))

# create the boundary segments (each row corresponds to an edge, that goes from
# the vertex in the first column to the one in the second column)
isboundary <- matrix(data=NA,nrow=dim(boundary)[1],ncol=2)
isboundary[,1] <- (length(data)+1):(length(data)+dim(boundary)[1])
isboundary[,2] <- c((length(data)+2):(length(data)+dim(boundary)[1]),(length(data)+1))

# create and plot the mesh
mesh_1 <- create.mesh.2D(p, order = 1, segments = isboundary)
mesh <- refine.mesh.2D(mesh_1, maximum_area=0.5, minimum_angle = 30)
par(mar=c(0,0,0,0))
plot(mesh,asp=1, pch=".")
box()
points(mesh$nodes[which(mesh$nodesmarkers==0),], pch=16,cex=0.6)
points(mesh$nodes[which(mesh$nodesmarkers==1),], pch=16, col='red',cex=0.6)
points(p,pch=16,cex=1.5,col=c(rep('blue',dim(data_locations)[1]),rep('orange',dim(boundary)[1])))
plot(st_geometry(orotl_sf), lwd = 3, add=T) #col="blue"

basisobj <- create.FEM.basis(mesh)

start_time <- Sys.time()
smooth_spatial <- smooth.FEM(locations=data_locations, observations=data, 
                            FEMbasis=basisobj, lambda.selection.criterion='newton', 
                            lambda.selection.lossfunction='GCV',
                            DOF.evaluation='exact',
                            lambda.optimization.tolerance = 0.001)
end_time <- Sys.time()
final_time = end_time - start_time

#or lambda.selection.criterion='newton_fd', lambda.selection.lossfunction='GCV', DOF.evaluation='stochastic'

lambda_grid <- c(1e-4, 1e-3, 1e-2, 1e-1, 1, 5, 10, 20)
lambda_grid <- c(5e-1)
smooth_spatial2 <- smooth.FEM(locations=data_locations, observations=data, 
                             FEMbasis=basisobj,
                             lambda.selection.criterion='grid',
                             lambda.selection.lossfunction='GCV',
                             lambda=lambda_grid)

#beta <- smooth_spatial$solution$beta
#z_hat <- smooth_spatial$solution$z_hat
smooth_spatial2$solution$rmse
smooth_spatial2$optimization

#compute rmse with k-fold cross validation
p <- 10
RMSE_cv <- rep(NA,p)
for(i in 1:p)
{
  k <- floor(length(data)/p)
  smooth_cv <- smooth.FEM(locations=data_locations[-((k*(i-1)+1):(k*i)),], observations=data[-((k*(i-1)+1):(k*i))], 
                              FEMbasis=basisobj, lambda.selection.criterion='grid',
                              lambda.selection.lossfunction='GCV',
                              lambda=lambda_grid)
  RMSE_cv[i] = sqrt(sum((eval.FEM(smooth_cv$fit.FEM, data_locations[((k*(i-1)+1):(k*i)),]) - data[((k*(i-1)+1):(k*i))] )^2)/k)
}
mean(RMSE_cv)

# 2d plot smoothings
source("/Users/lupomarsigli/Desktop/Tesi_Teo/plotFEM2d.R")
plotFEM2d(smooth_spatial2$fit.FEM,zlim=c(0,45),levels=levels,
          xlim=range(boundary[,1])+c(-0.5,0.5),ylim=range(boundary[,2])+c(-0.5,0.5),
          asp=1,xlab='',ylab='',xaxt="n",yaxt="n", Nx=200, Ny=200)
#points(boundary,type = 'l',lwd=2)
#points(boundary[c(dim(boundary)[1],1),],type = 'l',lwd=2)
plot(st_geometry(orotl_sf), lwd=2, add=T)

#3d plot
plot(smooth_spatial2$fit.FEM, axes = FALSE)
#axes3d(col='white')
points3d(data_locations[,1], data_locations[,2], data, col="black", pch=19)


#plot of a window with more 2d plots

xlim <- range(boundary[,1])+c(-0.5,0.5)
Nx <- 100
Ny <- 100
zlim <- c(0,50)
#zlim <- range(data)

xmin = xlim[1]
xmax = xlim[2]
ymin = ylim[1]
ymax = ylim[2]
X <- matrix(seq(xmin, xmax, len=Nx),ncol=1)
Y <- matrix(seq(ymin, ymax, len=Ny),ncol=1)    
Xmat <- X %*% matrix(1,nrow=1,ncol=Ny)
Ymat <- matrix(1,nrow=Nx,ncol=1) %*% t(Y)
Xvec = NULL
for (numc in 1:Ny)
{
  Xvec <-mc(Xvec,Xmat[,numc])
}
Yvec = NULL
for (numc in 1:Ny)
{
  Yvec <- c(Yvec,Ymat[,numc])
}
eval_points <- cbind(Xvec, Yvec)
eval_sol=rep(NA,nrow(eval_points))

SolutionObj <- smooth_spatial2$fit.FEM
eval_sol <- eval.FEM(SolutionObj, locations = eval_points)
evalmat <- matrix(eval_sol, nrow=Nx, ncol=Ny, byrow=F)
levels <- seq(from=min(data),to=max(data),length=10)

image(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim)
contour(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, add=T,colkey=F,col="black",levels=levels)



