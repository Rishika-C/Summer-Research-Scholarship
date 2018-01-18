### TESTS

### Simulation Function
## Creating even grid of traps
install.packages("spatstat")
library("spatstat")
window = owin(xrange=c(0,100), yrange=c(0,100))
points = gridcentres(window, 5, 5)
traps = as.matrix(cbind(points$x, points$y))


### Model-Fitting Function
## Even trap grid
# Smaller D - means that N remains under 500!
data = simul.data(traps=traps, D=50, buffer=50, g0=0.9, sigma=10, n.occassions=2, seed=2017, discard0=TRUE)
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
locations(results=results, s.no=1, M=500, xlim=data$xlim, ylim=data$ylim)


