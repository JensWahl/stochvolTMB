devtools::load_all()
library(tidyverse)
library(stochvol)
nobs <- 2000

# Gaussian case
param <- list(phi = 0.9, sigma_h = 0.1, sigma_y = 0.2, alpha = -2, rho = -0.7, df = 5)

model <- "t"
# model <- 'skew_gaussian' model <- 't'
dat <- stochvolTMB::sim_sv(param = param, nobs = nobs, model = model, seed = 123)
#dat = svsim(2000, mu = -4, phi = 0.9, nu = 5, sigma = 0.2)
obj <- stochvolTMB::get_nll(dat$y, model = "t", silent = TRUE)
opt <- stochvolTMB::estimate_parameters(dat$y, model = model)
res = residuals(opt)

library(stochvol)

mod2 = svtsample(dat$y)
pred = predict(mod2, steps = 200)
volplot(mod2, forecast = pred)

res = summary(opt, report = "random")
ts.plot(res$estimate)
lines(mod2$summary$latent[, 1] - mod2$summary$para[1,1], col = "red")

plot(res$estimate, mod2$summary$latent[, 1] - mod2$summary$para[1,1])



