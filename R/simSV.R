#' Simulate data for the stochastic volatility model
#' @param T Length of time series 
#' @param param List of parameters
#' @param seed Seed to reproduce simulation
#' @param method Distribtion of error term (only Gaussian so far)
simSV <- function(param, N = 1000, seed = NULL, method = "gaussian"){
  
  # Set seed if specified
  if(!is.null(seed)) set.seed(seed)
  
  phi <- param$phi
  sigma_y <- param$sigma_y
  sigma_h <- param$sigma_h
  
  # Latent process 
  h <- rep(NA, N)
  
  # We assume stationary distribution
  h[1] <- rnorm(1, 0, sigma_h / sqrt(1 - phi^2))
  
  for(t in 2:N){
    h[t] <- phi * h[t - 1] + rnorm(1, 0, sigma_h)
  }
  
  # Observations 
  y <- rep(NA, N)
    if(method == "gaussian"){
      
      y <- exp(h / 2) * rnorm(N, 0, sigma_y)
      
    } else if(method == "t"){
      
      # parameter specific for the t-distribution
      df <- param$df
      
      y <- exp(h / 2) * sigma_y * rt(N, df = df)
    }else if(method == "skew_gaussian"){
      
      # parameter specific for the skew normal distribution
      # use package sn do generate random sample from skew normal distribution
      alpha <- param$alpha
      delta <- alpha / sqrt(1 + alpha^2)
      omega <- 1 / sqrt( 1 - 2 * delta^2 / pi)
      epsilon <- - omega * delta * sqrt(2 / pi)
      y <- exp(h / 2) * sigma_y * sn::rsn(n = N, alpha = alpha, xi = epsilon, omega = omega)
      
      # remove attributes specific for rsn
      attr(y, "family") <- NULL
      attr(y, "parameters") <- NULL
    }
  
  return(data.frame(y = y, h = h))
  
}
