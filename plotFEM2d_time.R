plotFEM2d_time <- function(SolutionObj, time, Nx = 100, Ny = 100, levels=NULL,
                   xlim = NA, ylim = NA, zlim = NULL,out=NULL, ...)
{
  if(is.na(xlim[1]))
  {
    xmin = min(SolutionObj$fit.FEM.time$FEMbasis$mesh$nodes[,1])
    xmax = max(SolutionObj$fit.FEM.time$FEMbasis$mesh$nodes[,1])
  }
  else
  {
    xmin = xlim[1]
    xmax = xlim[2]
  }
  
  if(is.na(ylim[1]))
  {
    ymin = min(SolutionObj$fit.FEM.time$FEMbasis$mesh$nodes[,2])
    ymax = max(SolutionObj$fit.FEM.time$FEMbasis$mesh$nodes[,2])
  }
  else
  {
    ymin = ylim[1]
    ymax = ylim[2]
  }
  
  X    = matrix(seq(xmin, xmax, len=Nx),ncol=1)
  Y    = matrix(seq(ymin, ymax, len=Ny),ncol=1)    
  
  Xmat = X %*% matrix(1,nrow=1,ncol=Ny)
  Ymat = matrix(1,nrow=Nx,ncol=1) %*% t(Y)
  Xvec = NULL
  for (numc in 1:Ny)
  {
    Xvec=c(Xvec,Xmat[,numc])
  }
  Yvec = NULL
  for (numc in 1:Ny)
  {
    Yvec=c(Yvec,Ymat[,numc])
  }
  eval_points = cbind(Xvec, Yvec)
  eval_sol=rep(NA,nrow(eval_points))
  if(!is.null(out))
    eval_points = eval_points[-out,]
  eval_sol_in = eval.FEM.time(SolutionObj, locations = eval_points, time.instants=time)
  if(!is.null(out)){
    eval_sol[(1:length(eval_sol))[-out]]=eval_sol_in
  }else{
    eval_sol=eval_sol_in
  }
  outside=which(is.na(eval_sol))
  evalmat = matrix(eval_sol, nrow=Nx, ncol=Ny, byrow=F)
  
  if(is.null(zlim))
  {
    zlim[1] = min(eval_sol, na.rm = TRUE)
    zlim[2] = max(eval_sol, na.rm = TRUE)
  }
  if(is.null(levels))
  {
    levels = seq(zlim[1],zlim[2],length.out = 10)
  }
  
  image(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim,...)
  contour(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, add=T,colkey=F,col="black",levels=levels,...)
  # outside
}