## FOR ALL ANIMALS
# g0 over all 10,000 iterations
g0 = results[,"g0"]
# coeff over all 10,000 iterations
coeff = results[,"coeff"]

# Need to multiply all activity centres if the z vector consists of any 0's! So, if animal is unobserved, they will
# NOT be included in the final model - due to the possibility that they do not exist?

## Collecting all activity centres, first - storing them in the object 'list'

M = 500
vector = rep(0, M)
list = as.list(vector)
for (i in 1:M) {
  s1 = paste("s[", i, ",1]", sep="")
  s2 = paste("s[", i, ",2]", sep="")
  act.cent = cbind(results[,s1], results[,s2])
  act.cent = as.matrix(act.cent)
  list[[i]] = act.cent
}

#### IN CASE WHEN USING THE CODE DON'T ALWAYS WANT 10000 ITERATIONS
n.iter = 10000
## Resetting the z values from the object 'results', so that any z vectors containing 0's are replaced by all-zero vectors
# Names of variables that have been monitored
names = names(results[1,])
# Extracting "z" values from MCMC results
z.values = results[,grep("z", names)]
sum(z.values[,"z[1]"])
# Resetting z-values accordingly
for (i in 1:M) {
  z = paste("z[", i, "]", sep="")
  z.values[,z]
  if (sum(z.values[,z]) != n.iter) {
    z.values[,z] = rep(0, n.iter)
  }
  z.values
}

## Multiplying all the elements of the activity centre list by all of the z vectors, so that by the end, any
# animals that are possibly imaginary have activity centre values of 0 - this means that they won't be counted when
# creating the final distribution!
vector.M = rep(0, M)
new.list = as.list(vector.M)
for (i in 1:M) {
  multiply = list[[i]] * z.values[,i]
  new.list[[i]] = multiply
}

## So, new.list is a list consisting of activity centres ONLY for observed animals - unobserved animals have activity
# centres of 0 - this means that when probability matrices, lambda matrices, etc. are constructed, they will have values of 0!

squared.distances = function(activity.centre, pixel.centres) {
  dist = e2dist(activity.centre, pixel.centres)
  squared = dist ^ 2
}
squared = squared.distances(activity.centre=new.list[[1]], centres.2)

## Try to find squared distances element-wise through the list - do the same for lambda, etc. Maybe find a way
# to remove/combine the purely 0 elements of the list???