#simulate trend data
setwd("H:/Research/Trend/Parallel")
wd <- "H:/Research/Trend/Parallel"
source('fit_BCP_slope.R')
require(parallel)
require(doParallel)
library(foreach)

coverage <- function(x, y, samples){
  dummy <- 0
  if ((x <= quantile((samples[ , paste0("", y)]), .975)) && 
      (x >= quantile((samples[ , paste0("", y)]), .025)))
  dummy <- 1
  return(dummy)
}

perf.coverage <- function(x, y, samples){
  dummy <- 0
  if ((x <= quantile((samples[ , paste0("", y)]), .975)) && 
      (x >= quantile((samples[ , paste0("", y)]), .025)) &&
      coverage(0, y, samples)==0)
  dummy <- 1
  return(dummy)
}

n.sim <- 100
#Read the data
full.data <- read.csv('full.data.csv', header = TRUE)
#final.res <- data.frame(matrix(NA, 4*3*4*4*4*n.sim, 90))
#estimation
nCores <- 4
registerDoParallel(nCores)
len <- nrow(full.data)
before <- proc.time()
result <- foreach (i1 = 1:4) %dopar% {
  temp <- full.data[i1, 1]
  y <- rbind(as.numeric(full.data[i1, (7:(temp + 6))]), 
             as.numeric(full.data[i1, ((temp + 7):(2 * temp + 6))]))
  results <- fit_BCP_slope(y)
  samples <- combine.mcmc(results$mcmc)
  outcome <- c(full.data[i1, 1:6], 
                            quantile((samples[ , "beta[1,1]"]), c(.025, .975)),
                            quantile((samples[ , "beta[1,2]"]), c(.025, .975)),
                            quantile((samples[ , "beta[2,1]"]), c(.025, .975)),
                            quantile((samples[ , "beta[2,2]"]), c(.025, .975)),
                            coverage(0, "beta[1,1]", samples),
                            coverage(0, "beta[1,2]", samples),
                            coverage(0, "beta[2,1]", samples),
                            coverage(0, "beta[2,2]", samples)
                            )
  # colnames(outcome) <- c("b11-2.5", "b11-97.5", "b12-2.5", "b12-97.5",
  #                        "b21-2.5", "b21-97.5", "b22-2.5", "b22-97.5",
  #                        "b11-0-cov", "b12-0-cov", "b21-0-cov", "b22-0-cov")
write.table(outcome, paste0('bresults', i1, '.csv'), sep = ",", col.names = FALSE, 
            row.names = FALSE)
}
after <- proc.time()
time.est <- after - before


# result.formatted <- matrix(unlist(result), 5, 90, byrow = T)        
# colnames(result.formatted) <- colnames(final.res)
# write.csv(result.formatted, 'final.results.csv')

# fry <- matrix(unlist(result), 5, 90, byrow = T)


#understand gamma
# colnames(final.res) <- c("length", "sigma", "int.es", "slope.es", "rho", "n.sim",
#                          "b11.ns.mn", "b11.mn","b11.reg.mn","b11.ll.mn",
#                          "b11.ns.sd", "b11.sd", "b11.reg.sd", "b11.ll.sd", 
#                          "b21.ns.mn", "b21.mn", "b21.reg.mn", "b21.ll.mn", 
#                          "b21.ns.sd", "b21.sd", "b21.reg.sd", "b21.ll.sd",
#                          "sigma.ns.mn", "sigma.mn", "sigma.reg.mn", "sigma.ll.mn",
#                          "sigma.ns.sd", "sigma.sd", "sigma.reg.sd", "sigma.ll.sd",
#                          "int.es.ns.mn", "int.es.mn", "int.es.reg.mn", "int.es.ll.mn",
#                          
#                          "b12.mn", "b12.reg.mn", 
#                          "b12.sd", "b12.reg.sd", 
#                          "b22.mn", "b22.reg.mn", 
#                          "b22.sd", "b22.reg.sd",
#                          "slope.es", "slope.es.reg",
#                          
#                          "rho.ns.mn", "rho.mn", 
#                          "rho.ns.25", "rho.ns.975",
#                          "rho.25", "rho.975",
#                          "b11", "b11.ns", "b11.reg", "b11.ll", 
#                          "b21", "b21.ns", "b21.reg", "b21.ll", 
#                          "b12", "b12.reg", 
#                          "b22", "b22.reg", 
#                          "sigma", "sigma.ns", "sigma.reg", "sigma.ll",
#                          "int.es", "int.es.ns", "int.es.reg", "int.es.ll",
#                          "int.es.0", "int.es.0.ns", "int.es.0.reg", "int.es.0.ll",
#                          "slope.es", "slope.es.reg",
#                          "slope.es.0", "slope.es.0.reg",
#                          "rho", "rho.ns", 
#                          "rho.0", "rho.0.ns",
#                          "int.es.perf", "int.es.ns.perf", "int.es.reg.perf", "int.es.ll.perf",
#                          "slope.es.perf", "slope.es.reg.perf",
#                          "rho.perf", "rho.ns.perf")