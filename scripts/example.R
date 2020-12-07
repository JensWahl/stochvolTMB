devtools::load_all()
library(tidyverse)
library(stochvol)
library(TMB)
nobs <- 6000

# Gaussian case
param <- list(phi = 0.9, sigma_h = 0.1, sigma_y = 0.05, alpha = -2, rho = -0.7, df = 30)

model <- "t"

# model <- 'skew_gaussian' model <- 't'
dat <- stochvolTMB::sim_sv(param = param, nobs = nobs, model = model, seed = 124)
#dat = svsim(2000, mu = -4, phi = 0.9, nu = 5, sigma = 0.2)
obj <- stochvolTMB::get_nll(dat$y, model = "t")
opt <- stochvolTMB::estimate_parameters(dat$y, model = model, silent = TRUE)
opt2 <- stochvolTMB::estimate_parameters(dat$y, model = "gaussian", silent = TRUE)



opt3 = nlminb(obj$par, obj$fn, obj$gr, control = list(trace = 1))

steps = 100
set.seed(123)
pred = predict(opt, steps = steps, nsim = 10000, include_parameters = TRUE)

est = as.list(sdreport(opt$obj), "Est")
sd = as.list(sdreport(opt$obj), "Std")
plot(1:nobs, est$h, ylim = c(-1, 1), xlim = c(1950, nobs + steps), type = "l")
lines(est$h + 2 * sd$h, col = "red")
lines(est$h - 2 * sd$h, col = "red")

pred_mean = apply(pred$h, 1, mean)
pred_95 = apply(pred$h, 1, quantile, probs = c(0.025, 0.975))
lines((nobs+1):(nobs + steps), pred_mean, lty = 2, col = "red")
lines((nobs+1):(nobs + steps), pred_95[1, ], lty = 2, col = "red")
lines((nobs+1):(nobs + steps), pred_95[2, ], lty = 2, col = "red")


plot(1:nobs, dat$y, ylim = c(-1, 1), xlim = c(1950, nobs + steps), type = "l")

pred_mean = apply(pred$y, 1, mean)
pred_95 = apply(pred$y, 1, quantile, probs = c(0.025, 0.975))
lines((nobs+1):(nobs + steps), pred_mean, lty = 2)
lines((nobs+1):(nobs + steps), pred_95[1, ], lty = 2, col = "red")
lines((nobs+1):(nobs + steps), pred_95[2, ], lty = 2, col = "red")



mod2 = svtsample(dat$y)
pred2 = predict(mod2, steps = steps)
volplot(mod2, forecast = pred2)

pred_mean = apply(pred2$y, 2, mean)
pred_95 = apply(pred2$y, 2, quantile, probs = c(0.025, 0.975))
lines((nobs+1):(nobs + steps), pred_mean, lty = 3, col = "blue")
lines((nobs+1):(nobs + steps), pred_95[1, ], lty = 3, col = "blue")
lines((nobs+1):(nobs + steps), pred_95[2, ], lty = 3, col = "blue")


res = summary(opt, report = "random")
ts.plot(res$estimate)
lines(mod2$summary$latent[, 1] - mod2$summary$para[1,1], col = "red")

plot(res$estimate, mod2$summary$latent[, 1] - mod2$summary$para[1,1])
