

###############################################################################.
#### SPACE TIME MODEL WITH A MANUAL MESH ####
###############################################################################.

# Model with covariates, using as target variable the absolute value of colony losses

df <- read.csv("data/new_data/data_bystate_temp_perc.csv")

#set time grid: from 2015 Q1 to 2022 Q2, without 2019 Q2
times <- unique(df[,c("year", "months")])
rownames(times) <- NULL
time_locations = seq(0,1,length.out = dim(times)[1])

df <- df[df$state != "hawaii" & df$state != "other states",]

#set the values of stressors as absolute values
df_abs <- df
cols <- c("Varroa.mites", "Other.pests.parasites", "Disesases", "Pesticides", "Other", "Unknown")
df_abs[cols] <- sapply(df[cols], '*', df$colony_max)

#Spatial coordinates:
df_coord <- read.csv("data/state_coords_lon_lat.csv")
df_coord <- df_coord[df_coord$state != "hawaii",] #remove hawaii
df_coord <- na.omit(df_coord) #remove Na (for "other states")

#Boundary values:
boundary <- read.table("/Users/lupomarsigli/Desktop/NP_project/NP_github/np_project/code/Eugenio/boundary_gg.txt", head=T)

df_abs <- merge(df_abs, df_coord, by = "state") 

#set target variable matrix
data_obs <- df_abs[,c(1,2,3,6)]

data <- data_obs %>% tidyr::pivot_wider(
  names_from = c("year", "months"), 
  values_from = colony_lost,
  values_fill = NULL
)

col_order <- order(colnames(data))
col_order <- c(1,head(col_order,-1))
data <- data[,col_order]
data <- data[,-1] #remove column 'state' from data
data_loss <- matrix(as.numeric(unlist(data)),nrow=nrow(data))

#set locations matrix
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

#create covariates matrix_
#- colony n: 4
#- Varroa.mites = 11
#- Pesticides: 14

df_covariates <- df_abs[,c(1,2,3,4,11,14)]

#second alternative to build covariates matrix
ncov <- 4
covariates <- matrix(NA,nrow=dim(df_covariates)[1],ncol=ncov)
covariates[,1] <- as.numeric(df_covariates[,4]) #colony n
covariates[,2] <- as.numeric(df_covariates[,5]) #varroa
covariates[,3] <- as.numeric(df_covariates[,6]) #pesticides

#here the order of covariates for row is with time t associated to all the states,
#followed by time t+1 associated to all the states...

# Afterwards, try with the covariate matrices inverted, meaning that, on the rows,
# the data is listed with a state and all the possible times, followed
# by the next state in alphabetical order with all the times, ...

start_time <- Sys.time()
smoothing_abs_temp_cov_iso <- smooth.FEM.time(locations=data_locations, time_locations=time_locations,
                                           observations=data_loss, 
                                           FEMbasis=basisobj,
                                           covariates = covariates,
                                           lambda.selection.criterion='newton_fd', 
                                           lambda.selection.lossfunction='GCV',
                                           DOF.evaluation='stochastic')
end_time <- Sys.time()
final_time = end_time - start_time

#save the result:
save(smoothing_abs_temp_cov_iso , file ="code/Lupo/smoothing_abs_temp_cov_iso_NcolVarroaPest_09022022.Rdata")

#3d plot
time_instant <- 5 #from 1 to 29
plot(smoothing_temp_cov_iso$fit.FEM.time, time_locations=time_locations[time_instant])
points3d(data_locations[,1], data_locations[,2], data_loss[,time_instant], cex=1, col="black")

# 2d plot for a vector of time instants
SolutionObj <- smoothing_abs_temp_cov_iso$fit.FEM.time
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

#print of relevant quantities
smoothing_abs_temp_cov_iso$solution$rmse
smoothing_abs_temp_cov_iso$beta
smoothing_abs_temp_cov_iso$optimization$lambda_solution
smoothing_abs_temp_cov_iso$optimization$GCV_vector


# PROVA A CONVERTIRE LE COORDINATE IN UTM DA LAT-LONG

## PROVA SETTANDO MANUALMENTE I VALORI DI LAMBDA


