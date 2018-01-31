xlim = data$xlim
ylim = data$ylim
M = 500
s.no=70

locations = function (results, s.no, M, xlim, ylim) {
  ## Points at which local density will be estimated
  xg = seq(xlim[1], xlim[2], length = 50)
  yg = seq(ylim[1], ylim[2], length = 50)
  
  
  ## Activity Centres
  # Names of variables that have been monitored
  names = names(results[1,])
  # Extracting "z" values from MCMC results
  Z = results[,grep("z", names)]
  # Logical vector - TRUE if z=1, FALSE if z=0
  logical = Z == 1
  
  # Extracting "s" values from MCMC results
  S = results[,grep("s[^i]", names)]
  # x-coordinates of all activity centres
  Sx = S[,1:M]
  # y-coordinates of all activity centres
  Sy = S[,-(1:M)]
  
  # Extracting relevant activity centres - x co-ordinates and y co-ordinates where z=1
  Sxout = Sx[logical==1]
  Syout = Sy[logical==1]
  
  # Associating each coordinate from above with the proper pixel
  Sxout = cut(Sxout, breaks=xg, include.lowest=TRUE)
  Syout = cut(Syout, breaks=yg, include.lowest=TRUE)
  
  # Tallying up how many activity centres are in each pixel
  Dn = table(Sxout, Syout)
  
  
  ## Drawing the density map
  # When drawing histograms in summarising function, have changed layout - need to change back! Also, specifying margins
  # so can see values on bottom and left 
  par(mfrow=c(1,1), mar=c(2, 2, 1, 1))
  image(xg, yg, Dn/ncol(Z), col=terrain.colors(10))
  
  
  ## Adding the lines showing movement of activity centres
  # Creating string for column names for selected activity centre's x- and y-coordinates
  col.1 = paste("s[", s.no, ",1]", sep="")
  col.2 = paste("s[", s.no, ",2]", sep="")
  
  # Creating string for column name for individual's 'z' vector
  z.string = paste("z[", s.no, "]", sep="")
  # Extracting z values
  z = results[,z.string]
  # Logical vector - TRUE if z=1, FALSE if z=0
  logical = z == 1
  
  # Extracting all x- and y-coordinates for range of activity centres for individual
  points.x = results[,col.1]
  points.y = results[,col.2]
  # Extracting only those x- and y-coordinates for activity centres associated with a z value of 1
  xout = points.x[logical==1]
  yout = points.y[logical==1]
  
  ## Drawing the lines!
  lines(xout, yout, lwd=0.1)
}



