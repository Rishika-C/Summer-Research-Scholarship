## From the model-fitting function!
results

summarise = function(results) {
  
  ## Gathering the variables that have been monitored (excluding s, z), so that the posterior samples can be summarised
  # The names of the variables that have been monitored in the model-fitting funciton
  names = names(results[1,])
  # If "s" and "z" have not been monitored in the model-fitting function
  if (length(grep("s[^i]", names)) == 0 & length(grep("z", names)) == 0) {list = split(results, names)}
  # If "s" and "z" have been monitored in the model-fitting function
  if (length(grep("s[^i]", names)) != 0 & length(grep("z", names)) != 0) {
    results.2 = results[,-c(grep("s[^i]", names), grep("z", names))]
    names.2 = names[-c(grep("s[^i]", names), grep("z", names))]
    list = split(results.2, names.2)
  }
  # If "s" has been monitored and "z" has not
  if (length(grep("s[^i]", names)) != 0 & length(grep("z", names)) == 0) {
    results.2 = results[,-c(grep("s[^i]", names))]
    names.2 = names[-c(grep("s[^i]", names))]
    list = split(results.2, names.2)
  }
  # If "z" has been monitored and "s" has not
  if (length(grep("s[^i]", names)) == 0 & length(grep("z", names)) != 0) {
    results.2 = results[,-c(grep("z", names))]
    names.2 = names[-c(grep("z", names))]
    list = split(results.2, names.2)
  }
  
  names(list)
  
  
  
  
}


