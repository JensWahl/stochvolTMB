#' Simulate data for the stochastic volatility model
#' @param N Length of time series 
#' @param param List of parameters
#' @param seed Seed to reproduce simulation
#' @param model Distribution of error term 
#' @return tibble of dimension \code{N x 2}
#' @export
sim_sv <- function(param, N = 1000, seed = NULL, model = "gaussian"){
  
  # Set seed if specified
  if(!is.null(seed)) set.seed(seed)
  
  phi <- param$phi
  sigma_y <- param$sigma_y
  sigma_h <- param$sigma_h
  
  # Latent process 
  h <- rep(NA, N)
  
  # We assume stationary distribution
  h[1] <- stats::rnorm(1, 0, sigma_h / sqrt(1 - phi^2))
  
  for(t in 2:N){
    h[t] <- phi * h[t - 1] + stats::rnorm(1, 0, sigma_h)
  }
  
  # Observations 
  y <- rep(NA, N)
    if(model == "gaussian"){
      
      y <- exp(h / 2) * stats::rnorm(N, 0, sigma_y)
      
    } else if(model == "t"){
      
      # parameter specific for the t-distribution
      df <- param$df
      
      y <- exp(h / 2) * sigma_y * stats::rt(N, df = df)
    }else if(model == "skew_gaussian"){
      
      # parameter specific for the skew normal distribution
      # use package sn do generate random sample from skew normal distribution
      alpha <- param$alpha
      delta <- alpha / sqrt(1 + alpha^2)
      omega <- 1 / sqrt(1 - 2 * delta^2 / pi)
      xi <- - omega * delta * sqrt(2 / pi)
      y <- exp(h / 2) * sigma_y * sn::rsn(n = N, alpha = alpha, xi = xi, omega = omega)
      
      # remove attributes specific for rsn
      attr(y, "family") <- NULL
      attr(y, "parameters") <- NULL
    } else if(model == "leverage"){
      
      #parameter specific for leverage model
      rho <- param$rho
      for(i in 1:(N - 1)){
        y[i] <- sigma_y * exp(h[i] / 2) * (rho / sigma_h * (h[i + 1] - phi * h[i]) + sqrt(1 - rho^2) * stats::rnorm(1))
      }
      # set last value (not used) to zero
      y[N] <- 0
    } #else if(model == "skew_gaussian_leverage"){
      
    #   alpha <- param$alpha
    #   delta <- alpha / sqrt(1 + alpha^2)
    #   omega <- 1 / sqrt(1 - 2 * delta^2 / pi)
    #   xi <- - omega * delta * sqrt(2 / pi)
    #   
    #   #parameter specific for leverage model
    #   rho <- param$rho
    #   for(i in 1:(N - 1)){
    #     eta <- (h[i+1] - h[i]) / sigma_h
    #     #xi <- sigma_y * exp(h[i] / 2) * (xi + rho * omega * eta)
    #     #omega <- sigma_y * exp(h[i] / 2) * omega * sqrt(1 - rho^2)
    # 
    #     y[i] <- sn::rsn(n = 1, xi = sigma_y * exp(h[i] / 2) * (xi + rho * omega * eta), 
    #                     omega = sigma_y * exp(h[i] / 2) * omega * sqrt(1 - rho^2), alpha)
    #   }
    #   # set last value (not used) to zero
    #   y[N] <- 0
    #   
    #   # remove attributes specific for rsn
    #   attr(y, "family") <- NULL
    #   attr(y, "parameters") <- NULL
    # }
  
  return(tibble::tibble(y = y, h = h))
  
}
