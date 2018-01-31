prep4image = function(data, plot = TRUE, contour = TRUE, key = TRUE, ...){
  
  # convert factor data$z to integer:
  zfactor=FALSE
  if(is.factor(data$z)) {
    zfactor=TRUE
    fac=data$z
    facnames=levels(fac)
    nlevels=length(facnames)
    data$z=rep(0,length(fac))
    got=rep(FALSE,nlevels)
    for(i in 1:nlevels){
      j=which(fac==facnames[i])
      if(length(j)>0) got[i]=TRUE
      data$z[j]=i
    }
    facnames=facnames[got] # remove factor names not in mask
  }
  data = as.matrix(data)
  
  x = sort(unique(data[,"x"]))
  y = sort(unique(data[,"y"]))
  
  z = matrix(NA, nrow = length(x), ncol = length(y))
  
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      m = which(data[,"x"] == x[i] & data[,"y"] == y[j]) ; m
      z[i,j] = if(length(m) == 0) NA else data[,"z"][m]
    }
  }
  
  if(plot){
    if(key){
      image.plot(x, y, z, ...)
    }else{
      image(x, y, z, ...)
    }
    if(contour) contour(x, y, z, add = TRUE)
  }
  
  outlist=list(x = x, y = y, z = z)
  if(zfactor) attributes(outlist)$facnames=facnames
  
  invisible(outlist)
  
}
