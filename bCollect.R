setwd('H:/Research/Trend/Parallel/')
results <- matrix(NA, 76800, 19)
actualmaster <- matrix(NA, 76800, 1)
for (i in 1:76800){
  actualmaster[i] <- paste0("bresults", i, ".csv")
  results[i,1] <- i
  results[i,2:19] <- as.matrix(read.csv(actualmaster[i], header=FALSE))
}
results <- matrix(results, 76800, 19)
colnames(results) <- c("no", "length", "sigma", "int.es", "slope.es", "rho", "n.sim",
                       "b11-2.5", "b11-97.5", "b12-2.5", "b12-97.5",
                       "b21-2.5", "b21-97.5", "b22-2.5", "b22-97.5",
                       "b11-0-cov", "b12-0-cov", "b21-0-cov", "b22-0-cov")

data <- read.csv("bresults.csv")
write.csv(data,"C:/Users/Computer/Desktop/Single case projects/Trend Analysis/Master-run/Parallel/bresults.csv" )
