## From the model-fitting function!
results

summarise = function(results) {
  
  ## Gathering the variables that have been monitored (excluding s, z), so that the posterior samples can be summarised
  # The names of the variables that have been monitored in the model-fitting funciton
  names = names(results[1,])
  # If "s" and "z" have not been monitored in the model-fitting function
  if (length(grep("s[^i]", names)) == 0 & length(grep("z", names)) == 0) {results.2 = results}
  # If "s" and "z" have been monitored in the model-fitting function
  if (length(grep("s[^i]", names)) != 0 & length(grep("z", names)) != 0) {
    results.2 = results[,-c(grep("s[^i]", names), grep("z", names))]
    names.2 = names[-c(grep("s[^i]", names), grep("z", names))]
  }
  # If "s" has been monitored and "z" has not
  if (length(grep("s[^i]", names)) != 0 & length(grep("z", names)) == 0) {
    results.2 = results[,-c(grep("s[^i]", names))]
    names.2 = names[-c(grep("s[^i]", names))]
  }
  # If "z" has been monitored and "s" has not
  if (length(grep("s[^i]", names)) == 0 & length(grep("z", names)) != 0) {
    results.2 = results[,-c(grep("z", names))]
    names.2 = names[-c(grep("z", names))]
  }
  
  
  ## Numeric summaries
  # Means
  means = apply(results.2, 2, mean)
  # Standard deviations
  sdeviations = apply(results.2, 2, sd)
  
  # Creating function so can find medians
  median = function(x) {
    sorted = sort(x)
    median = sorted[0.5*length(x)]
  }
  # Medians
  medians = apply(results.2, 2, median)
  
  
  list = list(means=means, standard.deviations=sdeviations, medians=medians)
  
  
  ## Histograms
  plot.new()
  par(mfrow = c(length(names.2), 1))
  par(mar = c(2, 0, 1, 0))
  for (i in 1:length(names.2)) {
    hist(results.2[,i], breaks=100, main=names.2[i])
  }
  
  return(list)
}

## Test
summarised = summarise(results)
summarised

