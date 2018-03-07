trace = function(results, par=NULL, s=FALSE, s.no=NULL) {
  if (s) {
    # Extracting s-value column names from the 'results' object, so that can later extract the required
    # columns from the 'results' object
    val.1 = paste("s", "[", s.no, ",1]", sep="")
    val.2 = paste("s", "[", s.no, ",2]", sep="")
    # Resetting margins so plot looks good
    par(mfrow=c(1,1), mar=c(2, 2, 1, 1))
    # Creating the plot
    plot(as.numeric(results[,val.1]), as.numeric(results[,val.2]))
  } else {
    plot(results[, par])
  }
}

 
