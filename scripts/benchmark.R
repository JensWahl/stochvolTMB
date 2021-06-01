library(bench)
library(stochvolTMB)
library(stochvol)

data(spy)

gauss = mark(iterations = 20, 
             stochvolTMB = { # create TMB object
               obj <- get_nll(spy$log_return, model = "gaussian")
               # Optimize nll 
               fit <- stats::nlminb(obj$par, obj$fn, obj$gr)},
             stochvol = svsample(spy$log_return), 
             check = FALSE)


t_dist = mark(iterations = 20, 
                      stochvolTMB = estimate_parameters(spy$log_return, model = "t"), 
                      stochvol = svtsample(spy$log_return), 
                      check = FALSE)

leverage = mark(iterations = 20, 
                      stochvolTMB = estimate_parameters(spy$log_return, model = "leverage"), 
                      stochvol = svlsample(spy$log_return), 
                      check = FALSE)


save(list(gauss = gauss, t_dist = t_dist, leverage = leverage))
data("extrates")



nobs = 5000
model = "gaussian"
dat = sim_sv(nobs = nobs)


gauss2 = mark(iterations = 20, 
             stochvolTMB = { # create TMB object
               obj <- get_nll(dat$y, model = "gaussian")
               # Optimize nll 
               fit <- stats::nlminb(obj$par, obj$fn, obj$gr)},
             stochvol = svsample(dat$y), 
             check = FALSE)






res_tmb = list()
res_mcmc = list()

for(i in 1:(ncol(exrates) - 1)) {
  
  cat("Estimating", names(exrates)[i], "\n")
  y = logret(exrates[, i], demean = TRUE)
  res_tmb[[i]] = estimate_parameters(y, silent = TRUE)
  res_mcmc[[i]] = svsample(y)
}


summary(res_mcmc[[1]], showlatent = F)
res_tmb[[1]]


# compare phi

phi_tmb = rep(NA, 23)
phi_mcmc = rep(NA, 23)

phi_tmb = sapply(res_tmb, function(x) summary(x)[parameter == "phi", estimate])
phi_mcmc = sapply(res_mcmc, function(x) x$summary$para[2, 1])

sigma_tmb = sapply(res_tmb, function(x) summary(x)[parameter == "sigma_y", estimate])
sigma_mcmc = sapply(res_mcmc, function(x) x$summary$para[4, 1])


sigma_h_tmb = sapply(res_tmb, function(x) summary(x)[parameter == "sigma_h", estimate])
sigma_h_mcmc = sapply(res_mcmc, function(x) x$summary$para[3, 1])

