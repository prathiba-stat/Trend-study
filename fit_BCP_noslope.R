# fit a piecewise linear regression model 
# with autocorrelated errors to a single-subject design
#about gamma prior see http://doingbayesiandataanalysis.blogspot.com/2012/08/gamma-likelihood-parameterized-by-mode.html

fit_BCP_noslope <- function(y) {
  require(runjags)
  
  P <- nrow(y)  
  T <- apply(y, 1, function(x) max(which(!is.na(x))))
  beta1 <- mean(y[1,])
  beta2 <- mean(y[2,])
  
  model <- "model {
    for (i in 1:P) {
      yhat[i, 1] <- beta[i, 1]
      y[i, 1] ~ dnorm(yhat[i, 1], tau)
      for (j in 2:T[i]) {
        yhat[i, j] <- beta[i, 1] 
        y[i, j] ~ dnorm(yhat[i, j] + 
                  rho * (y[i, j - 1] - yhat[i, j - 1]), tau)
      }
      beta[i, 1] ~ dnorm(mu.int[i], prec.int)
      mu.int[i] ~ dunif(0, 50)
      
    }
    prec.int ~ dgamma(sh.i, ra.i) #gamma is parametrized by mode and sd
    sh.i <- 1 + m.i * ra.i
    ra.i <- ( m.i + sqrt( m.i^2 + 4*sd.i^2 ) ) / ( 2 * sd.i^2 )
    m.i ~ dunif(0, 1)
    sd.i ~ dunif(0, 1)
    int.es <- (beta[2, 1] - beta[1, 1])/sigma

    sigma ~ dunif(0.1, 5)
    tau <- pow(sigma, -2)
   knot <- T/2
   rho ~ dunif(-1, 1)
  }"

  results <- autorun.jags(
    model = model,
    data = list(y = y, T = T, P = P),
    monitor = c("beta", "sigma", "rho", "prec", "int.es"),
    n.chains = 4,
    startsample = 30000,
    inits = function() {
      list(
       # beta = rbind(rnorm(1, beta1, 1), rnorm(1, beta2, 1)),
        sigma = runif(1, 0.1, 5),
        rho = runif(1, -1, 1)
      )
    }, 
    method = "rjparallel"
  )
  
  
  # combine all chains into a single chain for convenience
  results$draws <- combine.mcmc(results$mcmc)
  
  results
}