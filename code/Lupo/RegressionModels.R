
#### Regression models by Lupo ####

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

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")

df_coord <- read.csv("data/state_coords_lon_lat.csv")

df_final <- merge(df, df_coord, by = "state")

lat_range = range(df_final$lat, na.rm = T)
lon_range = range(df_final$lon, na.rm = T)

df_noNa <- na.omit(df_final)

####### LOCAL POLYNOMIAL REGRESSION ####

#https://pjbartlein.github.io/GeogDataAnalysis/lec14.html


#Represent the notion of locally weighted regression–a curve- or function-fitting
#technique that provides a generally smooth curve, the value of which at a particular
#location along the x-axis is determined only by the points in that vicinity.
#The method consequently makes no assumptions about the form of the relationship,
#and allows the form to be discovered using the data itself.


model_loess <- loess(colony_lost_pct ~ lon + lat, data=df_noNa,
                parametric = F, span=0.4) #parametric = "colony_max"
# CHANGE SPAN PARAMETER

summary(model_loess)
cor(df_noNa$colony_lost_pct, model_loess$fitted)^2

#interpolation target grid

grid_longitude <- seq(min(lon_range)-10, max(lon_range)+10, length.out = 50)
grid_latitude <- seq(min(lat_range)-10, max(lat_range)+10, length.out = 50)
grid_mar <- list(lon=grid_longitude, lat=grid_latitude)

# get the fitted (interpolated) values
tann_interp <- predict(model_loess, expand.grid(grid_mar))
tann_z <- matrix(tann_interp, length(grid_longitude),
                 length(grid_latitude))

# plot the interpolated values as shaded rectangles and contours
nclr <- 8
plotclr <- brewer.pal(nclr, "PuOr")
plotclr <- plotclr[nclr:1] # reorder colors

shapefile <- "/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"
orotl_sf <- st_read(shapefile)
#orotl_sf

plot(st_geometry(orotl_sf), axes=TRUE, xlim = c(-130, -60), ylim = c(25,45))
#plot(st_geometry(orotl_sf), axes=TRUE, xlim = lon_range, ylim = lat_range)
image(grid_longitude, grid_latitude, tann_z, col=plotclr, add=T)
contour(grid_longitude, grid_latitude, tann_z, add=TRUE)
points(df_noNa$lon, df_noNa$lat)
plot(st_geometry(orotl_sf), add=T)

#############################################################################.

#### Spatial Regression ####


shapefile <- "/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp"
orotl_sf <- st_read(shapefile)
#orotl_sf

#geom <- ortl_sf$geometry
#obj <- geom ..
#obj@polygons[[1]]@Polygons[[10]]@coords

plot(st_geometry(orotl_sf), axes=T, xlim = c(-130, -60), ylim = c(25,45))
plot(st_geometry(orotl_sf), axes=T)

US_coords <- orotl_sf %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  dplyr::select(X,Y) %>% 
  dplyr::rename(long = X, lat = Y)

US_restricted_coords <- US_coords[US_coords$long > -130 & US_coords$long < -60 &
                                    US_coords$lat > 20 &  US_coords$lat < 55,]

#write.table(US_restricted_coords,"code/Lupo/boundary.txt",row.names=FALSE)

################ Spatial Regression 

#Let's remove the time dependency considering only 1 year and averaging over the trimesters
#YEAR 2016
df <- read.csv("data/new_data/data_bystate_temp_perc.csv")

df_2016 <- df[df$year==2016,]
#get just the variables state, year and colony_lost_pct
df_2016 <- df_2016[,c(1,3,7)]
df_new <- df_2016 %>% group_by(state, year) %>% summarize(loss_avg = mean(colony_lost_pct))
#remove hawaii:
df_new = df_new[df_new$state != "hawaii",]

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove hawaii:
df_coord <- df_coord[df_coord$state != "hawaii",]
#remove Na (for "other states"):
df_coord <- na.omit(df_coord)
df_final_2016 <- merge(df_new, df_coord, by = "state") #dataset without hawaii and other states

# load the coordinates of the boundary of the domain
boundary_all <- read.table("code/Lupo/boundary.txt", header=TRUE)

#sample boundary points:
#indices <- sample(nrow(boundary_all), 1000, replace = FALSE)
indices <- seq(1,nrow(boundary_all), by=300)
boundary <- boundary_all[indices,]

#boundary <- unique(boundary_all)
rownames(boundary) <- NULL

#translate locations really close to the borders (east border), otherwise the mesh
#cannot be built (not needed for rectangular mesh):
for (i in 1:nrow(df_final_2016)){
  long = df_final_2016[i,4]
  lat = df_final_2016[i,5]
  if (-70 > long & long > -80 & 35 < lat & lat < 45)
    df_final_2016[i,4] = df_final_2016[i,4] - 1
}

data_obs <- df_final_2016

data <- as.numeric(data_obs[,3]) #/scale_data
range(data)

data_locations <- matrix(NA,nrow=dim(data_obs)[1],ncol=2)
data_locations[,1] <- as.numeric(data_obs[,4])
data_locations[,2] <- as.numeric(data_obs[,5])
#data_locations[,1] <- as.numeric(scale(data_obs[,4], center=T, scale=T))
#data_locations[,2] <- as.numeric(scale(data_obs[,5], center=T, scale=T))

rm(data_obs)

#select the unique locations from data locations, since to create the mesh
#I should not have repeated nodes
#locs <- unique(data_locations)
locs <- data_locations

#plots:
#longitude values from -130 to -60
#latitude values from 25 to 45

# create the matrix p with all the data locations and boundary locations
p <- matrix(data=NA,nrow=dim(locs)[1]+dim(boundary)[1],ncol=2)
p[,1] <- c(locs[,1],boundary[,1])
p[,2] <- c(locs[,2],boundary[,2])
plot(p,pch=16,cex=0.4,col=c(rep('red',dim(locs)[1]),rep('black',dim(boundary)[1])))

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

Tri <- mesh$triangles

basisobj <- create.FEM.basis(mesh)

n <- length(data)

# graphical parameters for the plots
zlim <- range(data)
levels <- seq(from=min(data),to=max(data),length=10)

# plot data
nodes <- data_locations
z <- data

width <- seq(from=0.5,to=2.5,length=100)

Min <- min(z)
Max <- max(z)

color=inferno(100)
plot(boundary,col='white',asp=1,xlab='',ylab='',axes=FALSE)
points(nodes[which(round((z-Min)/(Max-Min)*100)==0),1],nodes[which(round((z-Min)/(Max-Min)*100)==0),2],col=heat.colors(130)[1],cex=0.5-diff(seq(from=0.5,to=2,length=100))[1],pch=16)
for(i in 1:100)
  points(nodes[which(round((z-Min)/(Max-Min)*100)==i),1],nodes[which(round((z-Min)/(Max-Min)*100)==i),2],col=color[i],cex=width[i],pch=16)

points(boundary,type = 'l',lwd=2)
points(boundary[c(dim(boundary)[1],1),],type = 'l',lwd=2)

######## isotropic smoothing
smoothing_iso <- smooth.FEM(locations=data_locations, observations=data, 
                            FEMbasis=basisobj, lambda.selection.criterion='newton', 
                            lambda.selection.lossfunction='GCV',
                            DOF.evaluation='exact',
                            lambda.optimization.tolerance = 0.001)

# 5-fold cross validation
RMSE_iso <- rep(NA,5)
for(i in 1:5)
{
  k <- floor(length(data)/5)
  smoothing_iso <- smooth.FEM(locations=data_locations[-((k*(i-1)+1):(k*i)),], observations=data[-((k*(i-1)+1):(k*i))], 
                              FEMbasis=basisobj, lambda.selection.criterion='newton', 
                              lambda.selection.lossfunction='GCV',
                              DOF.evaluation='exact',
                              lambda.optimization.tolerance = 0.001)
  RMSE_iso[i] = sqrt(sum((eval.FEM(smoothing_iso$fit.FEM, data_locations[((k*(i-1)+1):(k*i)),]) - data[((k*(i-1)+1):(k*i))] )^2)/k)
}

### Plot the estimated tensor and the estimated field

source("/Users/lupomarsigli/Desktop/Tesi_Teo/plotFEM2d.R")

# 2d plot smoothings
x11()
plotFEM2d(smoothing_iso$fit.FEM,zlim=c(-0.1,3.5),levels=levels,
          xlim=range(boundary[,1])+c(-0.5,0.5),ylim=range(boundary[,2])+c(-0.5,0.5),
          asp=1,xlab='',ylab='',xaxt="n",yaxt="n", Nx=200, Ny=200)
points(boundary,type = 'l',lwd=2)
points(boundary[c(dim(boundary)[1],1),],type = 'l',lwd=2)

###############################################################################.
#### SPATIAL REGRESSION WITH A RECTANGULAR MESH ####
###############################################################################.

###### NO COVARIATES ######

#Let's remove the time dependency considering only 1 year and averaging over the trimesters
#YEAR 2016
df <- read.csv("data/new_data/data_bystate_temp_perc.csv")

df_2016 <- df[df$year==2016,]
#get just the variables state, year and colony_lost_pct
df_2016 <- df_2016[,c(1,3,7)]
df_new <- df_2016 %>% group_by(state, year) %>% summarize(loss_avg = mean(colony_lost_pct))
#remove hawaii:
df_new = df_new[df_new$state != "hawaii",]

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove hawaii:
df_coord <- df_coord[df_coord$state != "hawaii",]
#remove Na (for "other states"):
df_coord <- na.omit(df_coord)
df_final_2016 <- merge(df_new, df_coord, by = "state") #dataset without hawaii and other states

boundary_all <- read.table("code/Lupo/boundary.txt", header=TRUE)

#top-left, top-right, bottom-right, bottom-left
#boundary <- data.frame(
#  long = c(min(boundary_all$long), max(boundary_all$long), max(boundary_all$long), min(boundary_all$long)),
#  lat = c(max(boundary_all$lat), max(boundary_all$lat), min(boundary_all$lat), min(boundary_all$lat)))

data_obs <- df_final_2016

data <- as.numeric(data_obs[,3]) #/scale_data
range(data)

data_locations <- matrix(NA,nrow=dim(data_obs)[1],ncol=2)
data_locations[,1] <- as.numeric(data_obs[,4])
data_locations[,2] <- as.numeric(data_obs[,5])
#data_locations[,1] <- as.numeric(scale(data_obs[,4], center=T, scale=T))
#data_locations[,2] <- as.numeric(scale(data_obs[,5], center=T, scale=T))

rm(data_obs)

#plots:
#longitude values from -130 to -60
#latitude values from 25 to 45

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

Tri <- mesh$triangles

basisobj <- create.FEM.basis(mesh)

n <- length(data)

# graphical parameters for the plots
zlim <- range(data)
levels <- seq(from=min(data),to=max(data),length=10)

# plot data
nodes <- data_locations
z <- data

width <- seq(from=0.5,to=2.5,length=100)

Min <- min(z)
Max <- max(z)

color=inferno(100)
plot(boundary,col='white',asp=1,xlab='',ylab='',axes=FALSE)
points(nodes[which(round((z-Min)/(Max-Min)*100)==0),1],nodes[which(round((z-Min)/(Max-Min)*100)==0),2],col=heat.colors(130)[1],cex=0.5-diff(seq(from=0.5,to=2,length=100))[1],pch=16)
for(i in 1:100)
  points(nodes[which(round((z-Min)/(Max-Min)*100)==i),1],nodes[which(round((z-Min)/(Max-Min)*100)==i),2],col=color[i],cex=width[i],pch=16)

points(boundary,type = 'l',lwd=2)
points(boundary[c(dim(boundary)[1],1),],type = 'l',lwd=2)

smoothing_iso <- smooth.FEM(locations=data_locations, observations=data, 
                            FEMbasis=basisobj, lambda.selection.criterion='newton', 
                            lambda.selection.lossfunction='GCV',
                            DOF.evaluation='exact',
                            lambda.optimization.tolerance = 0.001)


smoothing_iso2 <- smooth.FEM(locations=data_locations, observations=data, 
                            FEMbasis=basisobj, lambda.selection.criterion='grid', lambda=c(1e-3,15,1e-1))

### Plot the estimated tensor and the estimated field

source("/Users/lupomarsigli/Desktop/Tesi_Teo/plotFEM2d.R")

# 2d plot smoothings

shapefile <- "/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp"
orotl_sf <- st_read(shapefile)

plotFEM2d(smoothing_iso$fit.FEM,zlim=c(5,17),levels=levels,
          xlim=range(boundary[,1])+c(-0.5,0.5),ylim=range(boundary[,2])+c(-0.5,0.5),
          asp=1,xlab='',ylab='',xaxt="n",yaxt="n", Nx=200, Ny=200)
points(boundary,type = 'l',lwd=2)
points(boundary[c(dim(boundary)[1],1),],type = 'l',lwd=2)
plot(st_geometry(orotl_sf), add=T)

#3d plot
plot(smoothing_iso$fit.FEM, axes = FALSE)
axes3d(col='white')
points3d(data_locations[,1], data_locations[,2], data, col="black", pch=19)

plot(smoothing_iso2$fit.FEM, axes = FALSE)
axes3d(col='white')
points3d(data_locations[,1], data_locations[,2], data, col="black", pch=19)

#### ADDING COVARIATES ####

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")

df_2016 <- df[df$year==2016,]

#get the variables state, year and colony_lost_pct + covariates:
#- colony_max -> column 5
#- varroa -> column 11
#- diseases -> column 13
#- pesticides -> column 14
#- precipitation -> column 20
#- avg temperature -> column 21

df_2016 <- df_2016[,c(1,3,5,7,11,13,14)] #20,21
df_new <- df_2016 %>% group_by(state, year) %>% summarize(loss_avg = mean(colony_lost_pct),
                                                          col_max = max(colony_max),
                                                          varroa_avg = mean(Varroa.mites),
                                                          diseases_avg = mean(Disesases),
                                                          pest_avg = mean(Pesticides)
                                                          #prec_avg =mean(Precipitation),
                                                          #temp_avg = mean(AverageTemperature)
                                                          )
#remove hawaii:
df_new <- df_new[df_new$state != "hawaii",]

#df_new[rowSums(is.na(df_new)) > 0,]

#df_new2 <- df_new %>% mutate(across(all_of(c("col_max", "varroa_avg", "diseases_avg", "pest_avg")),scale)) #does not work

#results change depending on how I scale!
#scale with mean = 0 and sd = 1:
#cols_to_scale <- c("col_max", "varroa_avg", "diseases_avg", "pest_avg", "prec_avg", "temp_avg")
cols_to_scale <- c("col_max", "varroa_avg", "diseases_avg", "pest_avg")
df_new[cols_to_scale] <- scale(df_new[cols_to_scale])
#min-max scaler:
df_new[cols_to_scale] <- lapply(df_new[cols_to_scale], function(x) (x - min(x)) / (max(x) - min(x)))

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove hawaii:
df_coord <- df_coord[df_coord$state != "hawaii",]
#remove Na (for "other states"):
df_coord <- na.omit(df_coord)

df_final_2016 <- merge(df_new, df_coord, by = "state") #dataset without hawaii and other states

data_obs <- df_final_2016

data <- as.numeric(data_obs[,3]) #/scale_data
range(data)

data_locations <- matrix(NA,nrow=dim(data_obs)[1],ncol=2)
data_locations[,1] <- as.numeric(data_obs[,8])
data_locations[,2] <- as.numeric(data_obs[,9])

covariates <- matrix(NA,nrow=dim(data_obs)[1],ncol=4)
covariates[,1] <- as.numeric(data_obs[,4])
covariates[,2] <- as.numeric(data_obs[,5])
covariates[,3] <- as.numeric(data_obs[,6])
covariates[,4] <- as.numeric(data_obs[,7])
#covariates[,5] <- as.numeric(data_obs[,8])
#covariates[,6] <- as.numeric(data_obs[,9])

# The mesh is the same as the above case without covariates

smoothing_iso <- smooth.FEM(locations=data_locations, observations=data, 
                            FEMbasis=basisobj, covariates=covariates,
                            lambda.selection.criterion='newton', 
                            lambda.selection.lossfunction='GCV',
                            DOF.evaluation='exact',
                            lambda.optimization.tolerance = 0.01)

### Plot the estimated tensor and the estimated field

source("/Users/lupomarsigli/Desktop/Tesi_Teo/plotFEM2d.R")
shapefile <- "/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp"
orotl_sf <- st_read(shapefile)

# 2d plot smoothings

plotFEM2d(smoothing_iso$fit.FEM,zlim=c(5,17),levels=levels,
          xlim=range(boundary[,1])+c(-0.5,0.5),ylim=range(boundary[,2])+c(-0.5,0.5),
          asp=1,xlab='',ylab='',xaxt="n",yaxt="n", Nx=200, Ny=200)
points(boundary,type = 'l',lwd=2)
points(boundary[c(dim(boundary)[1],1),],type = 'l',lwd=2)
plot(st_geometry(orotl_sf), add=T)

beta <- smoothing_iso$solution$beta
z_hat <- smoothing_iso$solution$z_hat
rmse <- smoothing_iso$solution$rmse

# 3d plot smoothings
plot(smoothing_iso$fit.FEM, axes = FALSE)
axes3d(col='white')


#' ## Compute the coeff vector evaluating the desired function at the mesh nodes
#' ## In this case we consider the fs.test() function introduced by Wood et al. 2008
#' coeff = fs.test(mesh$nodes[,1], mesh$nodes[,2])
#' ## Create the FEM object
#' FEMfunction = FEM(coeff, FEMbasis)

###############################################################################.
#### SPACE TIME MODEL WITH A MANUAL MESH ####
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

########## ADD COVARIATES ##########

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")
df <- df[df$state != "hawaii" & df$state != "other states",]

#Varroa.mites = 11, Other pests = 12, Diseases = 13, #Pesticides = 14
#Other = 15, Unknown = 16, Max Temp = 17, Min Temp = 18, Precipitation = 20
#Avg temp = 21
df_covariates <- df[,c(1,2,3,11,12,13,14,15,16,17,18,20,21)]

############################################################### not needed
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

#second alternative to build covariates matrix
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

#QUESTION: does the matrix of covariates, in the rows, should have first the same state for each time
#or first each time followed by all the state ?????

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

#################### LAMBDA SET #############################################

smoothing_temp_cov_iso$solution$rmse
smoothing_temp_cov_iso$optimization$lambda_solution
smoothing_temp_cov_iso$optimization$GCV_vector

lambdaS <- c(1e-4, 1e-3, 1e-2, 1e-1, 1, 5)
lambdaT <- c(1, 5)
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

########## Write the percentage as values between 0 and 1 ################

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



###############################################################################.
#### SEMIPARAMETRIC SPATIAL AUTOREGRESSIVE MODEL ####
###############################################################################.


library(pspatreg)



###############################################################################.
#### PAPER API #####
###############################################################################.

###############################################################################.
##### SLIDE MGCV #######
###############################################################################.
