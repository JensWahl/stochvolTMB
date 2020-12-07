library(testthat)
library(stochvolTMB)

l <- list()

seed <- 1234
nobs <- 1000
param <- list(phi = 0.9, sigma_h = 0.4, sigma_y = 0.2, alpha = -2, rho = -0.7, df = 5)

l$param <- param
l$N <- nobs
l$seed <- seed

model <- "gaussian"
data <- sim_sv(param = param, nobs = nobs, seed = seed, model = model)
opt <- estimate_parameters(data$y, model = model)
saveRDS(opt, "tests/testthat/test_objects/test_summary.rds")
l[[model]]$par <- opt$fit$par
l[[model]]$objective <- opt$fit$objective
l[[model]]$y <- data$y

model <- "t"
data <- sim_sv(param = param, nobs = nobs, seed = seed, model = model)
opt <- estimate_parameters(data$y, model = model)
l[[model]]$par <- opt$fit$par
l[[model]]$objective <- opt$fit$objective
l[[model]]$y <- data$y


model <- "leverage"
data <- sim_sv(param = param, nobs = nobs, seed = seed, model = model)
opt <- estimate_parameters(data$y, model = model)
l[[model]]$par <- opt$fit$par
l[[model]]$objective <- opt$fit$objective
l[[model]]$y <- data$y

model <- "skew_gaussian"
data <- sim_sv(param = param, nobs = nobs, seed = seed, model = model)
opt <- estimate_parameters(data$y, model = model)
l[[model]]$par <- opt$fit$par
l[[model]]$objective <- opt$fit$objective
l[[model]]$y <- data$y

saveRDS(l, "tests/testthat/test_objects/test_parameter_estimates.rds")
