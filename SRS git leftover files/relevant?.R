install.packages("mvtnorm")

M = 500
xlim = data$xlim
ylim = data$ylim

## Points at which local density will be estimated
xg = seq(xlim[1], xlim[2], length = 50)
yg = seq(ylim[1], ylim[2], length = 50)

## Activity Centres
# Names of variables that have been monitored
names = names(results[1,])
# Extracting "z" values from MCMC results
Z = results[,grep("z", names)]
# Logical vector - TRUE if z=1, FALSE if z=0
logical = Z == 1

# Extracting "s" values from MCMC results
S = results[,grep("s[^i]", names)]
# x-coordinates of all activity centres
Sx = S[,1:M]
# y-coordinates of all activity centres
Sy = S[,-(1:M)]

# Extracting relevant activity centres - x co-ordinates and y co-ordinates where z=1
Sxout = Sx[logical==1]
Syout = Sy[logical==1]

# Associating each coordinate from above with the proper pixel
Sxout = cut(Sxout, breaks=xg, include.lowest=TRUE)
Syout = cut(Syout, breaks=yg, include.lowest=TRUE)

# Tallying up how many activity centres are in each pixel
Dn = table(Sxout, Syout)



## FIRST ANIMAL
# Activity centres over all 10,000 iterations
ac.1 = cbind(results[,"s[1,1]"], results[,"s[1,2]"])
# Sigma over all 10,000 iterations
sigma = results[,"sigma"]
# g0 over all 10,000 iterations
g0 = results[,"g0"]

install.packages("spatstat")
library("spatstat")
window.2 = owin(xrange=c(-40,140), yrange=c(-40,140))
points.2 = gridcentres(window.2, 49, 49)
centres = as.matrix(cbind(points.2$x, points.2$y))
points(centres)

?dmvnorm
centres[1,]
ac.1[1,]
sigma[1]
sum((xg[2]-xg[1])^2 * test)
test = dmvnorm(centres, mean=ac.1[1,], sigma=matrix(c(sigma[1], 0, 0, sigma[1]), nrow=2))
sum(test)
