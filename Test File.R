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
# Simulating data
data = simul.data(traps=traps, D=50, buffer=50, g0=0.9, sigma=10, n.occassions=2, seed=2017, discard0=TRUE)


### Model-Fitting Function
# NOTE - smaller D - means that N remains under 500 (so can still use M=500)!
results = fit.model(data=data, M=500, parameters = c("g0", "coeff", "sigma", "N", "D", "z", "s"), n.iter=10000, g0.start = 0.5, log_coeff.start = -5)


### Summarising Function
summarised = summarise(results)
summarised


### Density Map Function
density.map(results, M=500, xlim=data$xlim, ylim=data$ylim, points=FALSE, traps=traps)


### Trace Function
trace(results=results, "D")
trace(results=results, s=TRUE, s.no=5)


### Locations Function
locations(results=results, s.no=77, M=500, xlim=data$xlim, ylim=data$ylim)


