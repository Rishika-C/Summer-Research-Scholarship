data = simul.data(traps=traps, D=1000, buffer=2, g0=0.9, sigma=2, n.occassions=10, seed=2017, discard0=TRUE)

## More activity centres than encounter data - i.e. discard0 argument has meant that some of the population (with known population size) have NOT been included
nrow(data$encounter.data)
nrow(data$activity.centres)

y = data$encounter.data
traplocs = data$trap.loc
trap.no = nrow(traplocs)
n.occ = data$n.occassions
xlim = data$xlim
ylim = data$ylim
pop.size = nrow(y)

## Data augmentation
# Given M (using M from book)
M = 500
# Adding all-0 rows
y = rbind(y, matrix(0, nrow=M-pop.size, ncol=ncol(y)))
# Vector of 0's and 1's - 1 if a 'real' individual, 0 for an 'added' individual 
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

cat(x, file="simulJAGS2.txt")

## Data to put into JAGS
jags.data = list(y=y, traplocs=traplocs, n.occ=n.occ, M=M, trap.no=trap.no, xlim=xlim, ylim=ylim)

## Initial Values
inits = function() {
  list (g0=runif(1, 0, 1), log_coeff=runif(1, 0, 1), s=sst, z=z)
}

## Parameters to monitor
parameters = c("g0", "coeff", "sigma", "N", "D", "z", "s")

## Running JAGS
jinit = jags.model("simulJAGS2.txt", data=jags.data, inits=inits, n.chains=1, n.adapt=1000)
## Results
jout = coda.samples(jinit, parameters, n.iter=1000, thin=1)
results = jout[[1]]


## NOTE that D is density per hectare, NOT per 10,000 hectares