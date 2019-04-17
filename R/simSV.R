#' Simulate data for the stochastic volatility model
#' @param T Length of time series 
#' @param param List of parameters
#' @param seed Seed to reproduce simulation
#' @param method Distribtion of error term (only Gaussian so far)
simSV <- function(param, T = 1000, seed = NULL, method = "gaussian"){
  
  # Set seed if specified
  if(!is.null(seed)) set.seed(seed)
  
  phi <- param$phi
  #sigma_y <- param$sigma_y
  sigma_h <- param$sigma_h
  
  # Latent process 
  h <- rep(NA, T)
  
  # We assume stationary distribution
  h[1] <- rnorm(1, 0, sigma_h / sqrt(1 - phi^2))
  
  for(t in 2:T){
    h[t] <- phi * h[t - 1] + rnorm(1, 0, sigma_h)
  }
  
  # Observations 
  y <- rep(NA, T)
    if(method == "gaussian"){
      # parameter specific for the gaussian case
      sigma_y <- param$sigma_y
      
      y <- exp(h / 2) * rnorm(T, 0, sigma_y)
    } else if(method == "t"){
      # parameter sepcific for the t-distribution
      df <- param$df
      
      y <- exp(h / 2) * rt(T, df = df)
    }
  
  return(y)
  }
  
}