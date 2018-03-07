## Pixels on map at which probability density will be calculated
# Installing the necessary package
install.packages("spatstat")
library("spatstat")

density.map(results, M=500, xlim=data$xlim, ylim=data$ylim, points=FALSE, traps=traps)

# Creating the grid of points
#### NOTE - can change xrange, yrange to be in terms of xlim, ylim!
window.2 = owin(xrange=c(-40,140), yrange=c(-40,140))
points.2 = gridcentres(window.2, 49, 49)
# The co-ordinates of the pixels at which the probability density will be calculated
centres = as.matrix(cbind(points.2$x, points.2$y))
points(centres, pch=4)

# Need to add an additional buffer of 50 to each side of the map - so, would have an x and y range of -90 to 190.
# Adding 0.4 so the range is 90.4, 100.4, so that a whole number of pixels can be added on each side - in total, 78 pixels long and wide
window.3 = owin(xrange=c(-90.4,190.4), yrange=c(-90.4,190.4))
points.big = gridcentres(window.3, 77, 77)
centres.2 = as.matrix(cbind(points.big$x, points.big$y))
points(centres.2, pch=3)


## FIRST ANIMAL
# Activity centres over all 10,000 iterations
ac.1 = cbind(results[,"s[1,1]"], results[,"s[1,2]"]) 
# Sigma over all 10,000 iterations
sigma = results[,"sigma"]
# g0 over all 10,000 iterations
g0 = results[,"g0"]
# coeff over all 10,000 iterations
coeff = results[,"coeff"]

## Calculating the distance between each pixel and each activity centre from the MCMC
distances = e2dist(ac.1, centres.2)
sq.distances = distances^2

## Finding probability density for ALL coeff, g0 values, and ALL distances between the 10000 activity centres 
# from the MCMC and all 2401 pixels
prob = matrix(0, ncol=dim(sq.distances)[2], nrow=dim(sq.distances)[1])
for (i in 1:dim(ac.1)[1]) {
  matrix = sq.distances[i,]
  prob[i,] = g0[i] * exp(-coeff[i] * matrix)
}
## Therefore, 'prob' is the probability matrix, giving the probability of the first animal's activity centre 
# being in each identified pixel

## Finding lambda values for the FIRST animal for each pixel
lambda = -log(1-prob)

# Adding together values for each row, and then dividing the elements of each row by their specific row totals 
add = apply(lambda, 1, sum)
norm = sweep(lambda, 1, add, FUN="/")

## Checking - if normalised correctly, should add to 1!
sum(norm[1,])

