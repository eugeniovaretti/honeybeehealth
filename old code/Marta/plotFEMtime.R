plot.FEM.time = function(x, time_locations = NULL, locations = NULL,
                         lambdaS = NULL, lambdaT = NULL, num_refinements = NULL, Nt = 100,
                         add = FALSE, main = NULL, col = "red", ...)
{
  if(!is(x, "FEM.time"))
    stop("x is not of class 'FEM.time'")
  
  if (!is.null(time_locations) && !is.null(locations))
    stop("time_locations and locations can not be both provided")
  
  if (is.null(time_locations) && is.null(locations)) {
    # default plot
    # space evolution:
    time_locations = mean(x$mesh_time)
    if(is.null(lambdaS) || is.null(lambdaT)){
      if(exists("x$bestlambda")){
        lambdaS = x$bestlambda[1]
        lambdaT = x$bestlambda[2]
      }else{
        lambdaS = 1
        lambdaT = 1
      }
    }
    N = nrow(x$FEMbasis$mesh$nodes)
    
    storage.mode(time_locations) <- "double"
    storage.mode(N) <- "integer"
    storage.mode(x$mesh_time) <- "double"
    storage.mode(x$coeff) <- "double"
    storage.mode(x$FLAG_PARABOLIC) <- "integer"
    
    solution <- .Call("eval_FEM_time_nodes",N,x$mesh_time,time_locations,x$coeff[,lambdaS,lambdaT],x$FLAG_PARABOLIC, PACKAGE = "fdaPDE")
    plot = FEM(solution[1:N],x$FEMbasis)
    plot.FEM(x = plot, num_refinements = num_refinements,main = paste0("Spatial function in t = ",time_locations),...)
    
    # time evolution:
    locations = x$FEMbasis$mesh$nodes[1,]
    
    # add default title to the plot
    if(is.null(main))
      main = paste0("Time evolution in (",round(locations,2),")")
    
    # define the evaluation points
    t = seq(min(x$mesh_time),max(x$mesh_time),length.out=Nt)
    eval_points = cbind(t, t(kronecker(matrix(1,ncol=Nt), locations)))
    # evaluate the function
    eval_sol = eval.FEM.time(FEM.time = x, space.time.locations = eval_points, lambdaT=lambdaT, lambdaS=lambdaS)
    
    # define variables for the plot
    type = "l"
    ylab = ""
    xlab = "Time"
    ylim = range(eval_sol)
    
    # plot the function evaluations
    if (add == FALSE) {
      plot(t[1:Nt],eval_sol[1:Nt],type=type,xlab=xlab,ylab=ylab,main=main,col=col,ylim=ylim, ...)
    }else{
      points(t[1:Nt],eval_sol[1:Nt],type=type,col=col, ...)
    }
  }
  
  if (!is.null(time_locations) && is.null(locations)) {
    # plot the spatial function for the provided time_locations
    
    # check that time_locations is a vector
    if (!is.vector(time_locations) && nrow(time_locations)!=1 && ncol(time_locations)!=1)
      stop("time_locations must be a vector of a column/row matrix")
    time_locations = as.vector(time_locations)
    
    # check that time_locations is in the correct range
    if(min(time_locations)<x$mesh_time[1] || max(time_locations)>x$mesh_time[length(x$mesh_time)])
      stop("time_locations must be within the 'time_mesh' range")
    
    if(is.null(lambdaS) || is.null(lambdaT)){
      if(exists("x$bestlambda")){
        lambdaS = x$bestlambda[1]
        lambdaT = x$bestlambda[2]
      }else{
        lambdaS = 1
        lambdaT = 1
      }
    }
    N = nrow(x$FEMbasis$mesh$nodes)
    
    storage.mode(time_locations) <- "double"
    storage.mode(N) <- "integer"
    storage.mode(x$mesh_time) <- "double"
    storage.mode(x$coeff) <- "double"
    storage.mode(x$FLAG_PARABOLIC) <- "integer"
    
    solution <- .Call("eval_FEM_time_nodes",N,x$mesh_time,time_locations,x$coeff[,lambdaS,lambdaT],x$FLAG_PARABOLIC, PACKAGE = "fdaPDE")
    for(i in 1:length(t))
    {
      plot = FEM(solution[(1+(i-1)*N):(N+(i-1)*N)],x$FEMbasis)
      plot.FEM(x = plot, num_refinements = num_refinements,...)
    }
  }
  
  
  if (is.null(time_locations) && !is.null(locations)) {
    # plot temporal evolution in the provided locations
    
    if (is(x$FEMbasis$mesh, "mesh.2D") | is(x$FEMbasis$mesh, "mesh.1.5D")){
      # check locations dimension
      if (is.vector(locations) && length(locations) != 2)
        stop("locations does not have the correct dimension")
      if (!is.vector(locations) && ncol(locations) != 2)
        stop("locations does not have the correct dimension")
      if (is.vector(locations))
        locations = matrix(locations, ncol = 2)
      
      # add default title to the plot
      if(is.null(main) && nrow(locations)==1)
        main = paste0("Time evolution in (",locations[1,1],",",locations[1,2],")")
      
      # define the evaluation points
      t = rep(seq(min(x$mesh_time),max(x$mesh_time),length.out=Nt),times=nrow(locations))
      eval_points = cbind(t, rep(locations[,1], each = Nt), rep(locations[,2], each = Nt))
    }
    
    if (is(x$FEMbasis$mesh, "mesh.2.5D") | is(x$FEMbasis$mesh, "mesh.3D")){
      # check locations dimension
      if (is.vector(locations) && length(locations) != 3)
        stop("locations does not have the correct dimension")
      if (!is.vector(locations) && ncol(locations) != 3)
        stop("locations does not have the correct dimension")
      if (is.vector(locations))
        locations = matrix(locations, ncol = 3)
      
      # add default title to the plot
      if(is.null(main) && nrow(locations)==1)
        main = paste0("Time evolution in (",locations[1,1],",",locations[1,2],",",locations[1,3],")")
      
      # define the evaluation points
      t = rep(seq(min(x$mesh_time),max(x$mesh_time),length.out=Nt),times=nrow(locations))
      eval_points = cbind(t, rep(locations[,1], each = Nt),
                          rep(locations[,2], each = Nt), rep(locations[,3], each = Nt))
    }
    
    # evaluate the function
    eval_sol = eval.FEM.time(FEM.time = x, space.time.locations = eval_points, lambdaT=lambdaT, lambdaS=lambdaS)
    
    # define variables for the plot
    if (length(col) < nrow(locations)){
      # recicle colors if not provided enough
      col = rep(col, nrow(locations))
    }
    if (!exists("type")) {
      type = "l"
    }
    if (!exists("ylab")) {
      ylab = ""
    }
    if (!exists("xlab")) {
      xlab = "Time"
    }
    if (!exists("ylim")) {
      ylim = range(eval_sol)
    }
    
    # plot the function evaluations
    for (ind in 1:nrow(locations)) {
      if (add == FALSE && ind == 1) {
        plot(t[1:Nt],eval_sol[(ind-1)*Nt+(1:Nt)],type=type,xlab=xlab,ylab=ylab,main=main,col=col[ind],ylim=ylim, ...)
      }else{
        points(t[1:Nt],eval_sol[(ind-1)*Nt+(1:Nt)],type=type,col=col[ind], ...)
      }
    }
    
  }
  
}


plot.FEM = function(x, colormap = "heat.colors", num_refinements = NULL, ...)
{
  if(is(x$FEMbasis$mesh, "mesh.2D")){
    if(x$FEMbasis$order == 1)
    {
      R_plot.ORD1.FEM(x, colormap, ...)
    }else{
      R_plot.ORDN.FEM(x, colormap, num_refinements, ...)
    }
  }else if(is(x$FEMbasis$mesh, "mesh.2.5D")){
    R_plot_manifold(x,...)
  }else if(is(x$FEMbasis$mesh, "mesh.3D")){
    R_plot_volume(x,...)
  }else if(is(x$FEMbasis$mesh, "mesh.1.5D")){
    R_plot_graph(x, ...)
  }
}

R_plot.ORD1.FEM = function(FEM, colormap, ...)
{
  # PLOT  Plots a FEM object FDOBJ over a rectangular grid defined by
  # vectors X and Y;
  #
  
  nodes <- FEM$FEMbasis$mesh$nodes
  triangles <- as.vector(t(FEM$FEMbasis$mesh$triangles))
  
  colormap <- match.fun(colormap)
  heat <- colormap(100)
  # How many plots are needed?
  nplots <- ncol(FEM$coeff)
  
  for (i in 1:nplots) {
    
    if (i > 1)
      readline("Press any key for the next plot...")
    
    open3d()
    axes3d()
    
    z <- FEM$coeff[triangles,i]
    triangles3d(nodes[triangles,1], nodes[triangles,2], z,
                color = heat[round(99*(z-min(z))/diff(range(z)))+1],...)
    
    aspect3d(2,2,1)
    view3d(0,-45)
  }
}
