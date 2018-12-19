library(TMB)
library(tidyverse)
compile("src/svNormal.cpp")
dyn.load(dynlib("src/svNormal"))


sv_Sim <- function(param, n){
  
  # Get parameters
  phi <- param$phi
  sigma <- param$sigma
  sigma_y <- param$sigma_y
  
  h <- rep(0, n)
  y <- rep(0, n)
  
  #Assume stationary distribution
  
  h[1] <- rnorm(1, 0, sigma_y / (1 - phi^2))
  
  for(i in 2:n){
    
    h[i] <- phi * h[i - 1] + rnorm(1, 0, sigma)
    
  }
  
  y <- exp(h/2) * rnorm(n, 0, sigma_y)
  
  return(list(y = y, h = h))
}

param <- list(phi = 0.95, sigma = 0.2, sigma_y = 0.36)
n <- 1000

y <- sv_Sim(param, n) 

dat <- list(y = y$y, n = n)
param <- list(log_sigma_y = log(0.33),
              log_sigma = log(0.33),
              phi_logit = 3.5,
              h = rep(0,n))

obj <- MakeADFun(data = dat, parameters = param, random = "h", DLL = "svNormal")

opt <- nlminb(obj$par, obj$fn, obj$gr)

rep <- sdreport(obj)
srep <- summary(rep)

opt_param <- srep[rownames(srep) %in% c("sigma_y", "sigma", "phi"), ]
opt_param

opt_h <- srep[rownames(srep) == "h", ]
ts.plot(opt_h[, 1], col = "red")
ts.plot(y$h)
lines(opt_h[, 1], col = "red")
