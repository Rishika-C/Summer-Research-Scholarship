### TESTS

## PACKAGES TO LOAD
# Spatstat package - for grid points
install.packages("spatstat")
library("spatstat")
# AHMBook package - for e2dist function (distance matrices)
install.packages("AHMBook")
library("AHMBook")
# Rjags package - to run MCMC!
install.packages("rjags")
library("rjags")


### Simulation Function
## Creating even grid of traps
window = owin(xrange=c(0,100), yrange=c(0,100))
points = gridcentres(window, 5, 5)
traps = as.matrix(cbind(points$x, points$y))
## Sourcing function
source("Simulation Function.R")
# Simulating data
data = simul.data(traps=traps, D=50, buffer=50, g0=0.9, sigma=10, n.occassions=10, seed=2017, discard0=TRUE)


### Model-Fitting Function
## Sourcing the file
source("Model-fitting function.R")
# NOTE - smaller D - means that N remains under 500 (so can still use M=500)!
results = fit.model(data=data, M=500, parameters = c("g0", "coeff", "sigma", "N", "D", "z", "s"), n.iter=10000, g0.start = 0.5, log_coeff.start = -5)


### Summarising Function
## Sourcing the file
source("Summarising Function.R")
## Running the code
summarised = summarise(results)
summarised


# When drawing histograms in summarising function, have changed layout - need to change back before drawing
# density map! Also, specifying margins so can see axes values on bottom and left 
par(mfrow=c(1,1), mar=c(2, 2, 1, 1))
### Density Map Function
## Sourcing the function
source("Density Map Function.R")
# Creating the map
density.map(results, M=500, xlim=data$xlim, ylim=data$ylim, points=FALSE, traps=traps)


### Trace Function
## Sourcing the function
source("Trace Function.R")
# Creating the trace plots
trace(results=results, "D")
trace(results=results, s=TRUE, s.no=5)


### Locations Function
## Sourcing the function
source("Locations Function.R")
# Creating the map
locations(results=results, s.no=77, M=500, xlim=data$xlim, ylim=data$ylim)


### Functions for new model
## Sourcing the funcionts
source("prep4image function.R")
source("New Model Functions.R")
# Generating a list of all the activity centre matrices for each animal from the MCMC iterations - for every
# z value of 0, the activity centre matrix has x- and y-coordinates of 0!
activity.centres = activity.matrices(results, M=500)
# Extracting all the z vectors from 'results'
z.values = Z.values(results)
# Generating a matrix of the pixel centres at which animal density will be estimated
centres.3 = centres(xrange=c(-90.4,190.4), yrange=c(-90.4,190.4), x.pixels=77, y.pixels=77)
# Function to draw the final map! The function itself calls on another function created (new.model() - this can
# be used to draw maps for individual animals' activity centres)
final.map(activity.centres, pixel.centres=centres.3, z.values, M=500, n.pixels=5929, xlim=c(-40,140), ylim=c(-40,140))
## Adding traps
points(traps, pch=16)
