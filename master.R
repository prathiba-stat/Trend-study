#simulate trend data
source('C:/Users/Computer/Desktop/Single case projects/Trend Analysis/simulate_SCED.R')
source('C:/Users/Computer/Desktop/Single case projects/Trend Analysis/plot_SCED.R')
source('C:/Users/Computer/Desktop/Single case projects/Trend Analysis/fit_SCED_BCP.R')
source('C:/Users/Computer/Desktop/Single case projects/Trend Analysis/fit_BCP_noslope.R')
source('C:/Users/Computer/Desktop/Single case projects/Trend Analysis/fit_BCP_noauto.R')
setwd("C:/Users/Computer/Desktop/Single case projects/Trend Analysis")
coverage <- function(x, y, samples){
  dummy <- 0
  if ((x <= quantile((samples[ , paste0("", y)]), .975)) && 
      (x >= quantile((samples[ , paste0("", y)]), .025)))
  dummy <- 1
  return(dummy)
}
cont.0 <- function(x, y, samples){
  dummy <- 0
  if ((x <= quantile((samples[ , paste0("", y)]), .975)) && 
      (x >= quantile((samples[ , paste0("", y)]), .025)))
    dummy <- 1
  return(dummy)
}
beta <- matrix(c(2, 0, NA, NA), 2, 2, byrow = TRUE)
times <- c(5, 8, 10, 15)
sigmas <- c(1, 2, 5)
int.es <- c(0.5, 1, 2, 5)
slope.es <- c(0, 0.3, 0.5, 1, 2)
autocorrs <- c(0, 0.2, 0.5, 0.8)
full.data <- data.frame(matrix(NA, 4*3*5*5*4*10, 36))
final.res <- data.frame(matrix(NA, 4*3*5*5*4*10, 70))
colnames(full.data) <- c("length", "sigma", "int.es", "slope.es", "rho", "rep",
                         paste0("y", seq(1:30)))
colnames(final.res) <- c("length", "sigma", "int.es", "slope.es", "rho", "rep",
                         "b11.ns.mn", "b11.all.mn","b11.reg.mn",
                         "b11.ns.sd", "b11.all.sd", "b11.reg.sd", 
                         "b21.ns.mn", "b21.mn", "b21.reg.mn", 
                         "b21.ns.sd", "b21.all.sd", "b21.reg.sd", 
                         "sigma.ns.mn", "sigma.all.mn", "sigma.reg.mn", 
                         "sigma.ns.sd", "sigma.all.sd", "sigma.reg.sd",
                         "int.es.ns.mn", "int.es.all.mn", "int.es.reg.mn",
                         
                         "b12.all.mn", "b12.reg.mn", 
                         "b12.all.sd", "b12.reg.sd", 
                         "b22.all.mn", "b22.reg.mn", 
                         "b22.all.sd", "b22.reg.sd",
                         "slope.all.es", "slope.es.reg",
                         
                         "rho.ns.mn", "rho.all.mn", 
                         "rho.ns.25", "rho.ns.975",
                         "rho.all.25", "rho.all.975",
                         "b11.ns", "b11.all", "b11.reg", 
                         "b21.ns", "b21.all", "b21.reg", 
                         "b12.all", "b12.reg", 
                         "b22.all", "b22.reg", 
                         "sigma.ns", "sigma.all", "sigma.reg",
                         "int.es.ns", "int.es.all", "int.es.reg",
                         "int.es.0.ns", "int.es.0.all", "int.es.0.reg",
                         "slope.es.all", "slope.es.reg",
                         "slope.es.0.all", "slope.es.0.reg",
                         "rho.all", "rho.ns", 
                         "rho.0.all", "rho.0.ns")
counter <- 1
for (i in 1:length(times)){
  T <- rep(times[i], 2)
  for (j in 1:length(sigmas)){
    e.sigma <- sigmas[j]
    for (k in 1:length(int.es)){
      beta[2, 1] <- int.es[k] * e.sigma + beta[1, 1]
      for (l in 1:length(slope.es)){
        beta[2,2] <- (slope.es[l] + (beta[1, 1] + (T[1] + (T[2]/2))  * beta[1,2]) - 
                        beta[2, 1])/(T[1] + (T[2]/2))        
        for (m in 1:length(autocorrs)){
          rho <- autocorrs[m]
          for (reps in 1:10){
            y <- simulate_SCED(T, beta, e.sigma, rho)
            full.data[counter, (1:(6 + 2*times[i]))] <- c(times[i], e.sigma, int.es[k], slope.es[l],
                                    rho, reps, y[1,], y[2,])
            plot_SCED(y, paste0("trend", "-",  times[i], "-",  e.sigma, "-",  
                                int.es[k], "-",  slope.es[l], "-", 
                                rho, "-",  reps, ".jpg"))
            results <- fit_SCED_BCP(y)
            results.noslope <- fit_BCP_noslope(y)
            results.reg <- fit_BCP_noauto(y)
            samples <- combine.mcmc(results$mcmc)
            samples.noslope <- combine.mcmc(results.noslope$mcmc)
            samples.reg <- combine.mcmc(results.reg$mcmc)
            
            
            final.res[counter, ] <- c(times[i], e.sigma, int.es[k], slope.es[l],
                                      rho, reps, 
                                      mean(samples.noslope[ , paste0("beta[1,1]")]), 
                                      mean(samples[ , paste0("beta[1,1]")]),
                                      mean(samples.reg[ , paste0("beta[1,1]")]), 
                                      
                                      sd(samples.noslope[ , paste0("beta[1,1]")]),
                                      sd(samples[ , paste0("beta[1,1]")]),
                                      sd(samples.reg[ , paste0("beta[1,1]")]),
                                      
                                      mean(samples.noslope[ , paste0("beta[2,1]")]), 
                                      mean(samples[ , paste0("beta[2,1]")]), 
                                      mean(samples.reg[ , paste0("beta[2,1]")]), 
                                      
                                      sd(samples.noslope[ , paste0("beta[2,1]")]),
                                      sd(samples[ , paste0("beta[2,1]")]),
                                      sd(samples.reg[ , paste0("beta[2,1]")]),
                                      
                                      mean(samples.noslope[ , "sigma"]), 
                                      mean(samples[ , "sigma"]), 
                                      mean(samples.reg[ , "sigma"]), 
                                      
                                      sd(samples.noslope[ , "sigma"]),
                                      sd(samples[ , "sigma"]),
                                      sd(samples.reg[ , "sigma"]),
                                      
                                      mean(samples.noslope[ , "int.es"]), 
                                      mean(samples[ , "int.es"]), 
                                      mean(samples.reg[ , "int.es"]), 
                                      
                                      mean(samples[ , paste0("beta[1,2]")]), 
                                      mean(samples.reg[ , paste0("beta[1,2]")]), 
                                      
                                      sd(samples[ , paste0("beta[1,2]")]),
                                      sd(samples.reg[ , paste0("beta[1,2]")]),
                                      
                                      mean(samples[ , paste0("beta[2,2]")]), 
                                      mean(samples.reg[ , paste0("beta[2,2]")]), 
                                      
                                      sd(samples[ , paste0("beta[2,2]")]),
                                      sd(samples.reg[ , paste0("beta[2,2]")]),
                                      
                                      mean(samples[ , paste0("slope.es")]), 
                                      mean(samples.reg[ , paste0("slope.es")]),
                                      
                                      mean(samples.noslope[ , "rho"]), mean(samples[ , "rho"]), 
                                      
                                      quantile((samples.noslope[ , "rho"]), c(.025, .975)),
                                      quantile((samples[ , "rho"]), c(.025, .975)),
                                      
                                      coverage(beta[1, 1], "beta[1,1]", samples),
                                      coverage(beta[1, 1], "beta[1,1]", samples.noslope),
                                      coverage(beta[1, 1], "beta[1,1]", samples.reg),
                                      
                                      coverage(beta[2, 1], "beta[2,1]", samples),
                                      coverage(beta[2, 1], "beta[2,1]", samples.noslope),
                                      coverage(beta[2, 1], "beta[2,1]", samples.reg),
                                      
                                      coverage(beta[1, 2], "beta[1,2]", samples),
                                      coverage(beta[1, 2], "beta[1,2]", samples.reg),
                                      
                                      coverage(beta[2, 2], "beta[2,2]", samples),
                                      coverage(beta[2, 2], "beta[2,2]", samples.reg),
                                      
                                      coverage(e.sigma, "sigma", samples),
                                      coverage(e.sigma, "sigma", samples.noslope),
                                      coverage(e.sigma, "sigma", samples.reg),
                                      
                                      coverage(int.es, "int.es", samples),
                                      coverage(int.es, "int.es", samples.noslope),
                                      coverage(int.es, "int.es", samples.reg),
                                      
                                      coverage(0, "int.es", samples),
                                      coverage(0, "int.es", samples.noslope),
                                      coverage(0, "int.es", samples.reg),
                                      
                                      coverage(slope.es, "slope.es", samples),
                                      coverage(slope.es, "slope.es", samples.reg),
                                      
                                      coverage(0, "slope.es", samples),
                                      coverage(0, "slope.es", samples.reg),
                                      
                                      coverage(rho, "rho", samples),
                                      coverage(rho, "rho", samples.noslope),
                                      
                                      coverage(0, "rho", samples),
                                      coverage(0, "rho", samples.noslope))
            counter <- counter + 1
          }
        }
      }
    }
  }
}
write.csv(full.data, 'full.data.csv')
write.csv(final.res, 'final.results.csv')




#understand gamma
m <- runif(1, 0, 1)
sd <- runif(1, 0, 1)
ra <- ( m + sqrt( m^2 + 4*sd^2 ) ) / ( 2 * sd^2 )
sh <- 1 + m * ra
gam <- rgamma(1000, shape = sh, rate = ra)
plot(density(gam), main = paste0("mode = ", round(m, digits = 2), 
                                 " sd = ", round(sd, digits = 2)))
