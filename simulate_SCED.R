simulate_SCED <- function(T, beta, e.sigma, rho){
  t <- max(T)
  p <- nrow(beta)
  y <- matrix(NA, nrow(beta), t)
  e <- matrix(NA, 1, t)
  for (j in 1:p){
    e[1] <- rnorm(1, 0, e.sigma)
    for (i in 2:T[j]){
      e[i] <- rho*e[i - 1] + rnorm(1, 0, e.sigma)
    }
    y[j,1:T[j]] <- beta[j, 1] + beta[j, 2]*seq(0:(T[j] - 1)) + e[1:T[j]]
    trend <- beta[j, 1] + beta[j, 2]*((seq(1:T[j])) - 1)
  }
  y
}

