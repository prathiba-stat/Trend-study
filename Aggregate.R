setwd('C:/Users/Computer/Desktop/Single case projects/Trend Analysis/Master-run')
results <- matrix(NA, 76800, 91)
actualmaster <- matrix(NA, 76800, 1)
for (i in 1:4){
  actualmaster[i] <- paste0("results", i, ".csv")
  results[i,1] <- i
  results[i,2:91] <- as.matrix(read.csv(actualmaster[i], header=FALSE))
}
results <- matrix(results, 76800, 91)

