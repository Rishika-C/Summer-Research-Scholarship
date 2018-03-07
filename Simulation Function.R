simul.data = function(traps, D, buffer, g0, sigma, n.occassions, seed=2017, discard0=FALSE) {
  # Matrix of trap co-ordinates
  traps = as.matrix(traps)
  # Number of traps
  ntraps = nrow(traps)
  
  # Lowest and highest x-values possible
  xlim = c(min(traps[,1] - buffer), max(traps[,1] + buffer))
  # Lowest and highest y-values possible
  ylim = c(min(traps[,2] - buffer), max(traps[,2] + buffer))
  
  # To get reproducible random results
  set.seed(seed)
  
  # Population size
  mean = (abs(diff(xlim)) * abs(diff(ylim))) * (D/10000)
  pop.size = rpois(1, mean)
  
  # To get reproducible random results
  set.seed(seed)
  
  # Simulating the activity centres
  sx = runif(pop.size, xlim[1], xlim[2])
  sy = runif(pop.size, ylim[1], ylim[2])
  S = cbind(sx, sy)
  
  # Distance matrix for trap locations and activity centres
  dist = e2dist(S, traps)
  
  # Computing probability values
  coeff = (1/(2*sigma*sigma))
  prob.matrix = g0 * exp(-coeff * dist * dist)
  
  # Generating the capture matrix
  Y = matrix(0, nrow=pop.size, ncol=ntraps)
  for (i in 1:nrow(Y)) {
    # To get reproducible random results
    set.seed(seed)
    Y[i,] = rbinom(ntraps, n.occassions, prob.matrix[i,])
  }
  
  if (discard0) {
    Y = Y[apply(Y, 1, sum)>0, ]
  } else {NULL}
  
  # Putting the encounter data (Y), trap locations (traps) and simulated activity centres (S) into a list
  data = list(encounter.data=Y, trap.loc=traps, activity.centres=S, xlim=xlim, ylim=ylim, n.occassions=n.occassions)
  
}


