## THE IMPORTANT VALUES
M = 500
n.iter = 10000
xlim = data$xlim
ylim = data$ylim
n.pixels = 5929

## Collecting all activity centres, first - storing them in the object 'list'
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

#### TEST ####
## To check - from 77 onwards (only 77 animals observed), elements of new.list/the activity centre matrices should
## consist of some rows of 0's
new.list[[1]]
new.list[[77]]
new.list[[78]]
new.list[[400]]
length(new.list)
###############


# Creating the grid of points at which density will be estimated. 
# Need to add an additional buffer of 50 to each side of the map - so, would have an x and y range of -90 to 190.
# Adding 0.4 so the range is 90.4, 100.4, so that a whole number of pixels can be added on each side - in total, 78 pixels long and wide
window.3 = owin(xrange=c(-90,190), yrange=c(-90,190))
points.big = gridcentres(window.3, 77, 77)
centres.2 = as.matrix(cbind(points.big$x, points.big$y))
points(centres.2, pch=3)


## Function to take the activity centre matrix, and find the subsequent distance/probability/lambda/distribution matrix
new.model.1 = function (activity.matrix, pixel.centres, z.vector, g0 = results[,"g0"], coeff = results[,"coeff"]) {
    # Distance matrix
    distances = crossdist(activity.matrix[,1], activity.matrix[,2], pixel.centres[,1], pixel.centres[,2])
    # Squaring the distance matrix, to get squared distances (as used in detection function)
    sq.distances = distances^2
    
    ### Finding the probability of the animal being present in each pixel, over all iterations - therefore, the
    ### number of rows of the final matrix is the number of MCMC iterations; the number of columns is the number
    ### of pixels/points at which the density is being estimated
    # Creating an empty matrix that will have the same number of rows as the number of MCMC iterations, and the
    # same number of columns as the number of pixels at which density is to be estimated
    prob = matrix(0, ncol=dim(sq.distances)[2], nrow=dim(sq.distances)[1])
    # Using a for loop to fill the empty matrix created above with the probability of an animal being present in
    # each selected pixel, for each MCMC iteration
    for (i in 1:dim(activity.matrix)[1]) {
      matrix = sq.distances[i,]
      prob[i,] = g0[i] * exp(-coeff[i] * matrix) * z.vector[i]
    }
    ## Therefore, 'prob' is the probability matrix, giving the probability of the first animal's activity centre 
    ## being in each identified pixel
    
    prob.2 = g0 * exp(-coeff * sq.distances) * z.vector
    
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

####### TEST #######
# Getting final vector for the first animal
checking = new.model(new.list[[1]], pixel.centres=centres.2, z.vector=z.values[,1])
# Creating the object required when calling image()
z = prep4image(data.frame(x=centres.2[,1], y=centres.2[,2], z=checking), plot=FALSE)
# Creating the image
image(x=unique(centres.2[,1]), y=unique(centres.2[,2]), z=z$z)

# Trying for an animal that may or may not be real
imaginary = new.model(new.list[[78]], pixel.centres=centres.2, z.vector=z.values[,78])
# Creating objects required for image()
prep = prep4image(data.frame(x=centres.2[,1], y=centres.2[,2], z=imaginary), plot=FALSE)
# Creating image
image(x=unique(centres.2[,1]), y=unique(centres.2[,2]), z=prep$z)
# Showing the trap locations
points(traps,pch=16)
######################


## Obtaining the final distribution for each possible animal (there are M possible animals), and storing the results
## in a matrix with M rows and the same number of columns as the number of pixels at which density is being estimated 
vectors = matrix(0, ncol=n.pixels, nrow=M)
for (i in 1:M) {
  model = new.model(new.list[[i]], pixel.centres=centres.2, z.vector=z.values[,i])
  vectors[i,] = model
}

model = new.model(activity.centres[[1]], pixel.centres=centres.3, z.vector=z.values[,1])
testing.prep = prep4image(data.frame(x=centres.3[,1], y=centres.3[,2], z=model), plot=FALSE)
image(x=unique(centres.3[,1]), y=unique(centres.3[,2]), z=testing.prep$z, xlab=NULL, ylab=NULL)

## 'vectors' should be the final matrix, containing the final distribution for each possible animal (there are
## M possible animals - in this case, 500 possible animals)
### NOTE it took approximately five hours to get 'vectors' - need to fix the code!!! 

## Obtaining the final final distribution!
final = apply(vectors, 2, sum)

par(mar=c(2,2,1,1))
### Trying to make the final map!
# Creating objects required for image()
final.prep = prep4image(data.frame(x=centres.2[,1], y=centres.2[,2], z=model), plot=FALSE)
# Creating image
image(x=unique(centres.2[,1]), y=unique(centres.2[,2]), z=final.prep$z, xlab=NA, ylab=NA)
# Showing the trap locations
points(traps,pch=16)
