## Assuming when produced data, discard0 = TRUE
fit.model = function(data, M, g0.prior = "g0 ~ dunif(0, 1)", no.logu.coeff = FALSE, coeff.prior, parameters, n.iter=1000) {
  
  # Encounter matrix, no zeroes
  y = data$encounter.data
  # Trap locations matrix
  traplocs = data$trap.loc
  # Number of traps
  trap.no = nrow(traplocs)
  # Number of occassions data was collected
  n.occ = data$n.occassions
  # xlim
  xlim = data$xlim
  # ylim
  ylim = data$ylim
  # Number of animals detected
  pop.size = nrow(y)
  
  ## Data augmentation
  M = M
  # Adding all-0 rows
  y = rbind(y, matrix(0, nrow=M-pop.size, ncol=ncol(y)))
  # Vector of 0's and 1's - 1 if a 'real' individual (a detected individual), 0 for an 'added' individual 
  z = c(rep(1, pop.size), rep(0, M-pop.size))
  
  ## Starting values for s - want activity centres at or near the traps at which individuals were captured - make activity
  ## centre the 'mean' for all the traps at which the individual was detected
  # NOTE - activity center created for every individual, real or potential
  sst = cbind(runif(M, xlim[1], xlim[2]), runif(M, ylim[1], ylim[2]))
  # NOTE - don't need a 'next' condition, because from rows 1 to pop.size, no rows contain all zeroes (in data, discard0=TRUE)
  for (i in 1:pop.size) {
    sst[i,1] = mean(traplocs[y[i,]>0,1])
    sst[i,2] = mean(traplocs[y[i,]>0,2])
  }
  
  ## JAGS model
  x = " 
  model{
  g0 ~ dunif(0,1)
  log_coeff ~ dunif(-10,10)
  coeff <- exp(log_coeff)
  sigma <- sqrt(1/(2*coeff))
  psi ~ dunif(0,1)

  for (i in 1:M) {
    z[i] ~ dbern(psi)
    s[i,1] ~ dunif(xlim[1], xlim[2])
    s[i,2] ~ dunif(ylim[1], ylim[2])
    for (j in 1:trap.no) {
      d[i,j] <- sqrt((s[i,1] - traplocs[j,1])^2 + (s[i,2] - traplocs[j,2])^2)
      p[i,j] <- z[i] * g0 * exp(-coeff * d[i,j] * d[i,j])
      y[i,j] ~ dbin(p[i,j], n.occ)
    }
  }
  N = sum(z)
  D = (N/((max(xlim) - min(xlim)) * (max(ylim) - min(ylim)))) * 10000
  }
  "
  
  if (g0.prior != "g0 ~ dunif(0, 1)") gsub("g0 ~ dunif\\(0, 1\\)", g0.prior, x)
  if (no.logu.coeff) {x = gsub("log_coeff ~ dunif\\(-10,10\\)", "", x) 
    gsub("coeff <- exp\\(log_coeff\\)", coeff.prior, x)}
  
  
  # Using model
  cat(x, file="simulJAGS2.txt")
  
  ## Data to put into JAGS
  jags.data = list(y=y, traplocs=traplocs, n.occ=n.occ, M=M, trap.no=trap.no, xlim=xlim, ylim=ylim)
  
  ## Initial Values
  inits = function() {
    list (g0=runif(1, 0, 1), log_coeff=runif(1, 0, 1), s=sst, z=z)
  }
  
  ## Parameters to monitor
  parameters = parameters
  
  ## Running JAGS
  jinit = jags.model("simulJAGS2.txt", data=jags.data, inits=inits, n.chains=1, n.adapt=1000)
  ## Results
  jout = coda.samples(jinit, parameters, n.iter=n.iter, thin=1)
  
  ## Final object - mcmc output
  results = jout[[1]]
  
}

## Test
data = simul.data(traps=traps, D=1000, buffer=2, g0=0.9, sigma=2, n.occassions=10, seed=2017, discard0=TRUE)
results = fit.model(data=data, M=500, parameters = c("g0", "coeff", "sigma", "N", "D", "z", "s"), n.iter=10000)

## Even trap grid
# Smaller D - means that N remains under 500!
data = simul.data(traps=traps, D=500, buffer=2, g0=0.9, sigma=10, n.occassions=10, seed=2017, discard0=TRUE)
results = fit.model(data=data, M=500, parameters = c("g0", "coeff", "sigma", "N", "D", "z", "s"), n.iter=10000)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
