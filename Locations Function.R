xlim = data$xlim
ylim = data$ylim
M = 500

locations = function (s.no) {
  col.1 = paste("s[", s.no, ",1]", sep="")
  col.2 = paste("s[", s.no, ",2]", sep="")
  
  z.string = paste("z[", s.no, "]", sep="")
  z = results[,z.string]
  logical = z == 1
  
  points.x = results[,col.1]
  points.y = results[,col.2]
  
  xout = points.x[logical==1]
  yout = points.y[logical==1]
  
  ## Points at which local density will be estimated
  xg = seq(min(xout), max(xout), length = 50)
  yg = seq(min(yout), max(yout), length = 50)
  
  xout.2 = cut(xout, breaks=xg, include.lowest=TRUE)
  yout.2 = cut(yout, breaks=yg, include.lowest=TRUE)
  
  table = table(xout.2, yout.2)
  
  par(mfrow=c(1,1), mar=c(2, 2, 1, 1))
  image(xg, yg, table/ncol(Z), col=terrain.colors(10))
  
  lines(xout, yout, lwd=0.1)
}

## Zoomed into the points at which the activity centre for the individual appears to be - if want to zoom out to
# overall area for all individuals, change values of xg and yg - use the min(xlim), max(xlim), min(ylim), max(ylim)