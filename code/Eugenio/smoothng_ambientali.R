library(mvtnorm)
library(rgl)
library(car)

groups <- levels(factor(readstate$state))
temp_smooth <- readstate[,c(1,2,4,8)]
tempmin_smooth_new <- temp_smooth %>% tidyr::pivot_wider(
  names_from = state, 
  values_from = ` Minimum Temperature`,
  values_fill = 0
)

library(fda)
# First of all we smooth the data. We choose a Fourier basis
# (periodic). We need to set the dimension of the basis

#ATTENZIONE! se giorni sulle ascisse -> t(A) in caso

time <- 1:30

data_F <- as.matrix(tempmin_smooth_new[,-c(1,2,48,49)])
data_F[is.na(data_F)]<-0
which(is.na(data_F))

# Choice 1: we set a high dimensional basis (interpolating)
# Pros: no loss of information
# Cons: possible overfitting 
nbasis <- 30
basis <- create.fourier.basis(rangeval=c(1,30),nbasis=30) 
data_F.fd <- Data2fd(y = data_F,argvals = time, basisobj = basis)
plot.fd(data_F.fd)

### AVERAGE
temp_smooth <- readstate[,c(1,2,7,8)]
tempmin_smooth_new <- temp_smooth %>% tidyr::pivot_wider(
  names_from = state, 
  values_from = ` Average Temperature`,
  values_fill = 0
)

library(fda)
# First of all we smooth the data. We choose a Fourier basis
# (periodic). We need to set the dimension of the basis

#ATTENZIONE! se giorni sulle ascisse -> t(A) in caso

time <- 1:30

data_F <- as.matrix(tempmin_smooth_new[,-c(1,2,48,49)])
data_F[is.na(data_F)]<-0
which(is.na(data_F))

# Choice 1: we set a high dimensional basis (interpolating)
# Pros: no loss of information
# Cons: possible overfitting 
nbasis <- 30
basis <- create.fourier.basis(rangeval=c(1,30),nbasis=30) 
data_F.fd <- Data2fd(y = data_F,argvals = time, basisobj = basis)
plot.fd(data_F.fd)