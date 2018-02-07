## FUNCTIONS FOR NEW MODEL

## Function to generate the final matrices for the activity centres, which can then be used to generate the final
## map - these matrices contain rows of zeroes evertyime an animal has a z-value of 0 for an MCMC iteration
activity.matrices = function(results, M) {
  ## Collecting all original MCMC activity centres, first - storing them in the object 'list'
  # Creating an empty vector, the same length as the maximum possible number of animals (M)
  vector = rep(0, M)
  # Making this empty vector a list, so that each element can be a matrix (activity centres over all 10000 iterations)
  list = as.list(vector)
  for (i in 1:M) {
    s1 = paste("s[", i, ",1]", sep="")
    s2 = paste("s[", i, ",2]", sep="")
    act.cent = cbind(results[,s1], results[,s2])
    act.cent = as.matrix(act.cent)
    list[[i]] = act.cent
  }
  
  ## Obtaining all of the z values from the MCMC results
  # Names of variables that have been monitored
  names = names(results[1,])
  # Extracting "z" values from MCMC results
  z.values = results[,grep("z", names)]
  
  ## Multiplying the activity centre matrices stored in 'list' by each corresponding column of 'z.values'
  ## This means that any activity centres associated with a z-value of 0 will be reset to 0
  vector.M = rep(0, M)
  new.list = as.list(vector.M)
  for (i in 1:M) {
    multiply = list[[i]] * z.values[,i]
    new.list[[i]] = multiply
  }
  new.list
}
# Final list of activity centre matrices
activity.centres = activity.matrices(results, 500)


## Function to generate array of z values
Z.values = function(results) {
  ## Obtaining all of the z values from the MCMC results
  # Names of variables that have been monitored
  names = names(results[1,])
  # Extracting "z" values from MCMC results
  z.values = results[,grep("z", names)]
}
## FINAL ARRAY OF Z VALUES
z.values = Z.values(results)


## Function to generate pixel centres
centres = function(xrange, yrange, x.pixels, y.pixels) {
  window.2 = owin(xrange=xrange, yrange=yrange)
  points = gridcentres(window.2, x.pixels, y.pixels)
  centres = as.matrix(cbind(points$x, points$y))
  centres
}
## TEST
centres.3 = centres(xrange=c(-90,190), yrange=c(-90,190), x.pixels=77, y.pixels=77)


## Function to take the activity centre matrix, and find the subsequent distance/probability/lambda/distribution matrix
new.model = function (activity.matrix, pixel.centres, z.vector, g0 = results[,"g0"], coeff = results[,"coeff"]) {
  # Distance matrix
  distances = crossdist(activity.matrix[,1], activity.matrix[,2], pixel.centres[,1], pixel.centres[,2])
  # Squaring the distance matrix, to get squared distances (as used in detection function)
  sq.distances = distances^2
  
  ### Finding the probability of the animal being present in each pixel, over all iterations - therefore, the
  ### number of rows of the final matrix is the number of MCMC iterations; the number of columns is the number
  ### of pixels/points at which the density is being estimated
  prob = g0 * exp(-coeff * sq.distances) * z.vector
  ## Therefore, 'prob' is the probability matrix, giving the probability of the first animal's activity centre 
  ## being in each identified pixel
  
  ## Finding the lambda values for the selected animal for each pixel
  lambda = -log(1-prob)
  
  # Adding together values for each row, and then dividing the elements of each row by their specific row totals 
  add = apply(lambda, 1, sum)
  # Replacing any 0's in 'add' with 1's, so that when normalising the row elements by the row totals, the 0's
  # will be divided by 1 instead of 0 (so will get 0 instead of NaN)
  add[add == 0] = 1
  
  # Normalising the elements of the lambda matrix - dividing each element by its row total
  norm = sweep(lambda, 1, add, FUN="/")
  
  # Combining the normalised matrix into a final vector, by finding the mean across each column
  final = apply(norm, 2, mean)
}


## Function to (hopefully) draw final map!
final.map = function(activity.centres, pixel.centres, z.values, M, n.pixels, points=FALSE, traps=NULL,
                     xlim=NA, ylim=NA) {
  ## Obtaining the final distribution for each possible animal (there are M possible animals), and storing the results
  ## in a matrix with M rows and the same number of columns as the number of pixels at which density is being estimated 
  vectors = matrix(0, ncol=n.pixels, nrow=M)
  for (i in 1:M) {
    model = new.model(activity.centres[[i]], pixel.centres=pixel.centres, z.vector=z.values[,i])
    vectors[i,] = model
  }
  ## Obtaining the final final distribution!
  final = apply(vectors, 2, sum)
  
  ### Trying to make the final map!
  # Creating objects required for image()
  final.prep = prep4image(data.frame(x=pixel.centres[,1], y=pixel.centres[,2], z=final), plot=FALSE)
  # Setting the margin size, so that the plot is as large as possible, but the axes are still visible
  par(mar=c(2,2,1,1))
  # Creating image
  image(x=unique(pixel.centres[,1]), y=unique(pixel.centres[,2]), z=final.prep$z, xlab=NA, ylab=NA, xlim=xlim,
        ylim=ylim, col=terrain.colors(10))
  
  # Showing the trap locations, if points=TRUE
  if (points) {
    points(traps, pch=16)
  }
}

final.map(activity.centres, pixel.centres=centres.3, z.values, M=1, n.pixels=5929, xlim=c(-40,140), ylim=c(-40,140))
