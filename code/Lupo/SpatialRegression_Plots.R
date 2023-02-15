

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

#### PLOTS FOR PRESENTATION ON SPATIAL REGRESSION ####

#create mesh first

df_coord <- read.csv("data/state_coords_lon_lat.csv")
#remove 'hawaii' and remove Na (for "other states"):
df_coord <- df_coord[df_coord$state != "hawaii",]
df_coord <- na.omit(df_coord)
df_coord <- df_coord %>% arrange(state)

#shapefile for US boundaries
shapefile <- "code/Lupo/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp"
orotl_sf <- st_read(shapefile)

#boundary points:
boundary <- read.table("code/Eugenio/boundary_gg.txt", head=T)
#boundary <- read.table("/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Lupo/boundary_gg_first.txt", head=T)

#data locations matrix (in long-lat coordinates)
data_locations <- matrix(NA,nrow=dim(df_coord)[1],ncol=2)
data_locations[,1] <- as.numeric(df_coord[,1])
data_locations[,2] <- as.numeric(df_coord[,2])

# create the matrix p with all the data locations and boundary locations
p <- matrix(data=NA,nrow=dim(data_locations)[1]+dim(boundary)[1],ncol=2)
p[,1] <- c(data_locations[,1],boundary[,1])
p[,2] <- c(data_locations[,2],boundary[,2])
plot(p,pch=16,cex=0.4,col=c(rep('red',dim(data_locations)[1]),rep('black',dim(boundary)[1])))

# create the boundary segments (each row corresponds to an edge, that goes from
# the vertex in the first column to the one in the second column)
isboundary <- matrix(data=NA,nrow=dim(boundary)[1],ncol=2)
isboundary[,1] <- (dim(data_locations)[1]+1):(dim(data_locations)[1]+dim(boundary)[1])
isboundary[,2] <- c((dim(data_locations)[1]+2):(dim(data_locations)[1]+dim(boundary)[1]),(dim(data_locations)[1]+1))

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

#data
df <- read.csv("data/new_data/data_bystate_temp_perc.csv")
#remove 'hawaii' and 'other states':
df <- df[df$state != "hawaii" & df$state != "other states",]

times <- unique(df[,c("year", "months")])
rownames(times) <- NULL
seq_t <- c(seq(1,17, by=4), c(20,24,28))

df_prices <- read.csv("data/production_year_new.csv")
df_prices$state <- tolower(df_prices$state)
df_prices <- df_prices[df_prices$state != "hawaii",]

stats <- data.frame(matrix(NA, nrow = 4, ncol = 8))
colnames(stats) <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
rownames(stats) <- c("lambda", "rmse", "rmse_CV", "GCV")

#seq_t <- 1
for (i in seq_t){
  
  year <- times[i,1]
  month <- times[i,2]
  print(paste("year = ", year, ", month = ", month))
  
  df_spatial <- df[df$year==year & df$months==month,]
  df_final <- merge(df_spatial, df_coord, by = "state") #dataset without hawaii and other states
  #order by state and remove columns of the states and given year and trimester
  df_final <- df_final %>% arrange(state, year, months)
  df_final <- df_final[,-c(1,2,3)]
  
  df_prices_spatial <- df_prices[df_prices$year == year,]
  df_prices_spatial$loss_pct <- df_final$colony_lost_pct
  
  #target variable: 
  loss_price <- df_prices_spatial$loss_pct * df_prices_spatial$yield_per_colony_kg * df_prices_spatial$average_price #lost money in dollar
  #transform the price in Kdollars
  loss_price <- loss_price/1000
  data <- as.numeric(loss_price)
  range(data)
  
  assign(paste("data_", year, sep = ""), data)
  assign(paste("levels_", year, sep = ""), seq(from=min(data),to=max(data),length=10))
  
  #covariates matrix
  #Varroa.mites = 8, Pesticides = 11, Unknown = 13
  #ncov <- 3
  #covariates <- matrix(NA,nrow=dim(df_final)[1],ncol=ncov)
  #covariates[,1] <- as.numeric(df_final[,8])
  #covariates[,2] <- as.numeric(df_final[,11])
  #covariates[,3] <- as.numeric(df_final[,13])
  #covariates[,4] <- as.numeric(df_final[,7])
  
  lambda_grid <- 5e-1
  stats[1, as.character(year)] = lambda_grid
  assign(paste("smooth_", year, sep = ""), 
         smooth.FEM(locations=data_locations, observations=data, 
                                FEMbasis=basisobj,
                                #covariates = covariates,
                                lambda.selection.criterion='grid',
                                lambda.selection.lossfunction='GCV',
                                lambda=lambda_grid)
  )
  
  #smooth_spatial$solution$beta
  stats[2, as.character(year)] = get(paste("smooth_", year, sep = ""))$solution$rmse
  stats[4, as.character(year)] = get(paste("smooth_", year, sep = ""))$optimization$GCV
  
  #compute rmse with k-fold cross validation
  p <- 10
  assign(paste("RMSE_cv_", year, sep = ""), c())
  print(paste("k-fold cross validation for year = ", year))
  for(j in 1:p)
  {
    k <- floor(length(data)/p)
    smooth_cv <- smooth.FEM(locations=data_locations[-((k*(j-1)+1):(k*j)),], observations=data[-((k*(j-1)+1):(k*j))], 
                            FEMbasis=basisobj, lambda.selection.criterion='grid',
                            lambda.selection.lossfunction='GCV',
                            lambda=lambda_grid)
    assign(paste("RMSE_cv_", year, sep = ""), c(get(paste("RMSE_cv_", year, sep = "")), 
                                                sqrt(sum((eval.FEM(smooth_cv$fit.FEM, data_locations[((k*(j-1)+1):(k*j)),]) - data[((k*(j-1)+1):(k*j))] )^2)/k)))
  }
  stats[3, as.character(year)] = mean(get(paste("RMSE_cv_", year, sep = "")))

}

###############################################################################.
####                       2 dimensional plots                             ####

xlim <- range(boundary[,1])+c(-0.5,0.5)
ylim <- range(boundary[,2])+c(-0.5,0.5)
Nx <- 100
Ny <- 100
zlim <- c(-2,35)
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
  Xvec <- c(Xvec,Xmat[,numc])
}
Yvec = NULL
for (numc in 1:Ny)
{
  Yvec <- c(Yvec,Ymat[,numc])
}
eval_points <- cbind(Xvec, Yvec)

eval_sol <- rep(NA,nrow(eval_points))

#x11()
par(mfrow=c(4,2)) #oma=c(0,0,0,0)
for (i in seq_t){
  
  year <- times[i,1]
  month <- times[i,2]
  SolutionObj <- get(paste("smooth_", year, sep = ""))$fit.FEM
  eval_sol <- eval.FEM(SolutionObj, locations = eval_points)
  evalmat <- matrix(eval_sol, nrow=Nx, ncol=Ny, byrow=F)
  
  
  image(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, xlab="longitude",
        ylab="latitude", main = paste("Money loss (kdollars) in following 3 months every 100 colonies in ", year, "-", month))
  contour(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, add=T,colkey=F,
          col="black",levels=get(paste("levels_", year, sep = "")))
  #points(boundary,type = 'l',lwd=2)
  #points(boundary[c(dim(boundary)[1],1),],type = 'l',lwd=2)
  plot(st_geometry(orotl_sf), lwd=2, add = T) #add=T
}

dev.off()



###############################################################################.
####                       Plot 3D                                         ####

#2016 is the year with the least rmse

#3d plot
year <- 2016
plot(get(paste("smooth_", year, sep = ""))$fit.FEM, axes = FALSE)
#axes3d(col='white')
points3d(data_locations[,1], data_locations[,2], get(paste("data_", year, sep = "")), col="black", pch=19)

###############################################################################.
####                           Inference                                  ####

#obj<-inferenceDataObjectBuilder(test = 'oat', dim = 2, beta0 = rep(1,4), n_cov = 4)
#obj2<-inferenceDataObjectBuilder(test = 'sim', dim = 3, component = 'nonparametric', n_cov = 3)

i <- 9 #time_instant (CHANGE!)
year <- times[i,1]
month <- times[i,2]
print(paste("year = ", year, ", month = ", month))

df_spatial <- df[df$year==year & df$months==month,]
df_final <- merge(df_spatial, df_coord, by = "state") #dataset without hawaii and other states
#order by state and remove columns of the states and given year and trimester
df_final <- df_final %>% arrange(state, year, months)
df_final <- df_final[,-c(1,2,3)]

df_prices_spatial <- df_prices[df_prices$year == year,]
df_prices_spatial$loss_pct <- df_final$colony_lost_pct

#target variable: 
loss_price <- df_prices_spatial$loss_pct * df_prices_spatial$yield_per_colony_kg * df_prices_spatial$average_price #lost money in dollar
#transform the price in Kdollars
loss_price <- loss_price/1000
data <- as.numeric(loss_price)
range(data)

#covariates matrix
#Varroa.mites = 8, Pesticides = 11, Unknown = 13
ncov <- 3
covariates <- matrix(NA,nrow=dim(df_final)[1],ncol=ncov)
covariates[,1] <- as.numeric(df_final[,8])
covariates[,2] <- as.numeric(df_final[,11])
covariates[,3] <- as.numeric(df_final[,13])
#covariates[,4] <- as.numeric(df_final[,7])

obj_inf <- inferenceDataObjectBuilder(test = 'sim', type = 'esf', component = "parametric",
                                      dim = 2, n_cov = ncov, beta0 = rep(0,ncov))
#test = 'sim' or 'oat

lambda_grid <- 5e-1
smooth_inference <- smooth.FEM(locations=data_locations, observations=data, 
                  FEMbasis=basisobj,
                  covariates = covariates,
                  lambda.selection.criterion='grid',
                  lambda.selection.lossfunction='GCV',
                  lambda=lambda_grid,
                  inference.data.object = obj_inf)

smooth_inference$solution$beta
smooth_inference$solution$rmse
smooth_inference$optimization$GCV
smooth_inference$inference$beta

###############################################################################.
####                Plots for models of colony loss pct                     ####
###############################################################################.

stats_losspct <- data.frame(matrix(NA, nrow = 4, ncol = 8))
colnames(stats_losspct) <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
rownames(stats_losspct) <- c("lambda", "rmse", "rmse_CV", "GCV")

for (i in seq_t){
  
  year <- times[i,1]
  month <- times[i,2]
  print(paste("year = ", year, ", month = ", month))
  
  df_spatial <- df[df$year==year & df$months==month,]
  df_final <- merge(df_spatial, df_coord, by = "state") #dataset without hawaii and other states
  #order by state and remove columns of the states and given year and trimester
  df_final <- df_final %>% arrange(state, year, months)
  df_final <- df_final[,-c(1,2,3)]
  
  data <- as.numeric( df_final[,4])
  print(range(data))
  
  assign(paste("data_", year, sep = ""), data)
  assign(paste("levels_", year, sep = ""), seq(from=min(data),to=max(data),length=10))
  
  #covariates matrix
  #Varroa.mites = 8, Pesticides = 11, Unknown = 13
  #ncov <- 3
  #covariates <- matrix(NA,nrow=dim(df_final)[1],ncol=ncov)
  #covariates[,1] <- as.numeric(df_final[,8])
  #covariates[,2] <- as.numeric(df_final[,11])
  #covariates[,3] <- as.numeric(df_final[,13])
  #covariates[,4] <- as.numeric(df_final[,7])
  
  lambda_grid <- 5e-1
  stats_losspct[1, as.character(year)] = lambda_grid
  assign(paste("smooth_losspct_", year, sep = ""), 
         smooth.FEM(locations=data_locations, observations=data, 
                    FEMbasis=basisobj,
                    #covariates = covariates,
                    lambda.selection.criterion='grid',
                    lambda.selection.lossfunction='GCV',
                    lambda=lambda_grid)
  )
  
  #smooth_spatial$solution$beta
  stats_losspct[2, as.character(year)] = get(paste("smooth_losspct_", year, sep = ""))$solution$rmse
  stats_losspct[4, as.character(year)] = get(paste("smooth_losspct_", year, sep = ""))$optimization$GCV
  
  #compute rmse with k-fold cross validation
  p <- 10
  assign(paste("RMSE_cv_", year, sep = ""), c())
  print(paste("k-fold cross validation for year = ", year))
  for(j in 1:p)
  {
    k <- floor(length(data)/p)
    smooth_cv <- smooth.FEM(locations=data_locations[-((k*(j-1)+1):(k*j)),], observations=data[-((k*(j-1)+1):(k*j))], 
                            FEMbasis=basisobj, lambda.selection.criterion='grid',
                            lambda.selection.lossfunction='GCV',
                            lambda=lambda_grid)
    assign(paste("RMSE_cv_", year, sep = ""), c(get(paste("RMSE_cv_", year, sep = "")), 
                                                sqrt(sum((eval.FEM(smooth_cv$fit.FEM, data_locations[((k*(j-1)+1):(k*j)),]) - data[((k*(j-1)+1):(k*j))] )^2)/k)))
  }
  stats_losspct[3, as.character(year)] = mean(get(paste("RMSE_cv_", year, sep = "")))
  
}

###############################################################################.
####                       2 dimensional plots                             ####

xlim <- range(boundary[,1])+c(-0.5,0.5)
ylim <- range(boundary[,2])+c(-0.5,0.5)
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
  Xvec <- c(Xvec,Xmat[,numc])
}
Yvec = NULL
for (numc in 1:Ny)
{
  Yvec <- c(Yvec,Ymat[,numc])
}
eval_points <- cbind(Xvec, Yvec)

eval_sol <- rep(NA,nrow(eval_points))

par(mfrow=c(4,2))
for (i in seq_t){
  
  year <- times[i,1]
  month <- times[i,2]
  SolutionObj <- get(paste("smooth_losspct_", year, sep = ""))$fit.FEM
  eval_sol <- eval.FEM(SolutionObj, locations = eval_points)
  evalmat <- matrix(eval_sol, nrow=Nx, ncol=Ny, byrow=F)
  
  
  image(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, xlab="longitude",
        ylab="latitude", main = paste("Colony loss in pct in ", year, "-", month))
  contour(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, add=T,colkey=F,
          col="black",levels=get(paste("levels_", year, sep = "")))
  #points(boundary,type = 'l',lwd=2)
  #points(boundary[c(dim(boundary)[1],1),],type = 'l',lwd=2)
  plot(st_geometry(orotl_sf), lwd=2, add=T)
}

############################################################################
