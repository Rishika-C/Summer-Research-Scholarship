## When writing function - using results from uneven trap grid - need to resolve error with even trap grid!

trace = function(results, par=NULL, s=FALSE, s.no=NULL) {
  if (s) {
    val.1 = paste("s", "[", s.no, ",1]", sep="")
    val.2 = paste("s", "[", s.no, ",2]", sep="")
    plot(as.numeric(results[,val.1]), as.numeric(results[,val.2]))
  } else {
    plot(results[, par])
  }
}


## NOTE - are the trace plots created for the activity centres what was required? They involve both the x- and y-
# co-ordinates - are basically scatter plots covering all 10000 iterations of the MCMC. 
## Should the plot be 3-D?