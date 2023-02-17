###### RUN LIBRARIES ### -----
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
#### SPACE TIME MODEL WITH A MANUAL MESH, TARGET = MONEY LOSS EVERY 3 MONTHS ####
####               INFERENCE ON COVARIATES PARAMETERS                      ####
###############################################################################.

####### MODEL WITH COVARIATES ####### ---

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")

#time: from 2015 Q1 to 2022 Q2, without 2019 Q2
times <- unique(df[,c("year", "months")])
rownames(times) <- NULL
#time_locations = seq(0,1,length.out = dim(times)[1])
time_locations = seq(1,29,length.out = dim(times)[1])

df_st <- df[,c(1,2,3,7)] 
df_st <- df_st[df_st$state != "hawaii" & df_st$state != "other states",]
df_st$state <- gsub( " ", "", df_st$state)

df_prices <- read.csv("data/production_year_new.csv")
df_prices <- df_prices[,-1]
df_prices$state <- tolower(df_prices$state)
df_prices <- df_prices[df_prices$state != "hawaii",]

df_final <- merge(df_st, df_prices, by = c("state", "year")) #dataset without hawaii and other states
#order by state and remove columns of the states and given year and trimester
df_final <- df_final %>% arrange(state, year, months)

#target variable: 
df_final$loss_price <- df_final$colony_lost_pct * df_final$yield_per_colony_kg * df_final$average_price #lost money in dollar
#transform the price in Kdollars
df_final$loss_price <- df_final$loss_price/1000
data <- as.numeric(df_final$loss_price)
range(data)

data_obs <- df_final[,-c(4,5,6)]
data <- data_obs %>% tidyr::pivot_wider(
  names_from = c("year", "months"), 
  values_from = loss_price,
  values_fill = NULL
)

boundary <- read.table("code/Eugenio/boundary_gg.txt", head=T)

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove hawaii:
df_coord <- df_coord[df_coord$state != "hawaii",]
#remove Na (for "other states"):
df_coord <- na.omit(df_coord)

#remove column 'state' from data:
data <- data[,-1]
data_loss_price <- matrix(as.numeric(unlist(data)),nrow=nrow(data))

data_locations <- matrix(NA,nrow=dim(df_coord)[1],ncol=2)
data_locations[,1] <- as.numeric(df_coord[,1])
data_locations[,2] <- as.numeric(df_coord[,2])

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
points(p,pch=16,cex=1,col=c(rep('blue',dim(data_locations)[1]),rep('orange',dim(boundary)[1])))
plot(st_geometry(orotl_sf), lwd = 2, add=T) #col="blue"

###############################################################################.
##########                  ADD COVARIATES                           ##########
###############################################################################.

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")
df <- df[df$state != "hawaii" & df$state != "other states",]

#Varroa.mites = 11, Other pests = 12, Diseases = 13, #Pesticides = 14
#Other = 15, Unknown = 16, Max Temp = 17, Min Temp = 18, Precipitation = 20
#Avg temp = 21
df_covariates <- df[,c(1,2,3,11,12,13,14,15,16,17,18,20,21)]

df_covariates <- df_covariates %>% arrange(state, year, months)
ncov <- 2
covariates <- matrix(NA,nrow=dim(df_covariates)[1],ncol=ncov)
covariates[,1] <- as.numeric(df_covariates[,4]) #varroa
#covariates[,2] <- as.numeric(df_covariates[,6]) #diseases
covariates[,2] <- as.numeric(df_covariates[,7]) #pesticides
#covariates[,4] <- as.numeric(df_covariates[,8]) #other
#covariates[,5] <- as.numeric(df_covariates[,9]) #unknown
#covariates[,5] <- as.numeric(df_covariates[,11]) #min temp
#covariates[,6] <- as.numeric(df_covariates[,12]) #precipitation

# CHECK THE COVARIATE MATRIX IS IN THE RIGHT ORDER OF TIME AND SPACE: here
#the order is with state associated to all times, followed by the next state in
#alphabetical order associated again to all times...


obj_inf <- inferenceDataObjectBuilder(test = 'sim', type = 'esf', component = "parametric",
                                      dim = 2, n_cov = ncov, beta0 = rep(0,ncov))
obj_inf2 <- inferenceDataObjectBuilder(test = 'oat', type = 'esf', component = "parametric",
                                      dim = 2, n_cov = ncov, beta0 = rep(0,ncov))

lambdaS <- c(1e-1)
lambdaT <- c(1e-1)
start_time <- Sys.time()
price_temp_cov_inference <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                         observations=data_loss_price, 
                                         FEMbasis=basisobj,
                                         covariates = covariates,
                                         lambda.selection.criterion='grid',
                                         lambda.selection.lossfunction='GCV',
                                         lambdaS = lambdaS,
                                         lambdaT = lambdaT,
                                         inference.data.object = obj_inf)
end_time <- Sys.time()
final_time = end_time - start_time

start_time <- Sys.time()
smoothing_temp_cov_inference <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                          observations=data_loss_price, 
                                          FEMbasis=basisobj,
                                          covariates = covariates,
                                          lambda.selection.criterion='newton_fd', 
                                          lambda.selection.lossfunction='GCV',
                                          DOF.evaluation='stochastic',
                                          inference.data.object = obj_inf)
end_time <- Sys.time()
final_time = end_time - start_time

start_time <- Sys.time()
smoothing_temp_cov_inference <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                          observations=data_loss_price, 
                                          FEMbasis=basisobj,
                                          covariates = covariates,
                                          lambda.selection.criterion='newton_fd', 
                                          lambda.selection.lossfunction='GCV',
                                          DOF.evaluation='stochastic',
                                          inference.data.object = obj_inf2)
end_time <- Sys.time()
final_time = end_time - start_time

price_temp_cov_inference$solution$rmse
price_temp_cov_inference$beta
price_temp_cov_inference$optimization$lambda_solution
price_temp_cov_inference$optimization$GCV_vector
price_temp_cov_inference$inference$beta

smoothing_temp_cov_inference$solution$rmse
smoothing_temp_cov_inference$beta
smoothing_temp_cov_inference$optimization$lambda_solution
smoothing_temp_cov_inference$optimization$GCV_vector
smoothing_temp_cov_inference$inference$beta

#Solutions
# - lambda = (1e-1, 1), rmse = 3.21, beta for varroa, pest, other, unknown: 0.024,
#-0.022, 0.15, 0.18, GCV = 19.21
# - lambda = (1e-1, 1e-1), rmse = 2.85, beta for varroa, diseases, pest, other, unknown: 0.005,
#0.021, -0.0008, 0.020, 0.013, GCV = 23 --> pvalues eigen sign flip: 0
# - lambda = (1e-1, 1e-1), rmse = 2.85, beta for varroa, pest, other, unknown: 0.007,
#0.003, 0.02, GCV = 20.25

# optimal gcv (not converged to global optimum): lambda = (2e10, 5e32), rmse = 6.41, gcv = 24
#pvalues oat for varroa e pesticides: 0.00, 0.918

# 3d plot
time_instant <- 5 #from 1 to 29
plot(price_temp_cov_inference$fit.FEM.time, time_locations=time_locations[time_instant])
points3d(data_locations[,1], data_locations[,2], data_loss_price[,time_instant], cex=1, col="black")


############################## time series plots ############################
#plots at a given location
index_loc <- 13
data_locations[index_loc,]
state <- df_coord[index_loc,"state"]
plot.new()
plot.window(xlim=c(0,29), ylim=c(0,25))
plot(price_temp_cov_inference$fit.FEM.time, locations=data_locations[index_loc,], add=T) #xlim = c(1,29) #ylim = c(0,20)
#ylim = range(data_loss[index_loc,]))
#xlab = "time (t)", ylab = "colony loss percentage", decorate=T)
points(time_locations, data_loss_price[index_loc,], pch = 19, cex = 0.5)
box(); axis(1); axis(2); title(main = paste("state = ", state), xlab="Time (from 2015-Q1 to 2022-Q2)", ylab="Money Loss (Kdollars) for 100 colonies")
data_loss_price[index_loc,]

############################### surface plots ################################

SolutionObj <- price_temp_cov_inference$fit.FEM.time
time_instants <- c(1,10,20,29) #change here!
xlim <- range(boundary[,1])+c(-0.5,0.5)
ylim <- range(boundary[,2])+c(-0.5,0.5)
Nx <- 200
Ny <- 200
zlim <- c(-5,28)

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
  levels <- seq(from=min(data_loss_price[,t]),to=max(data_loss_price[,t]),length=10)
  
  image(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, xlab="longitude",
        ylab="latitude", main = paste("Money Loss in US at year =", times[t,1],  times[t,2]))
  contour(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, add=T,colkey=F,col="black",levels=levels)
  plot(st_geometry(orotl_sf), width=2, add=T)
}
