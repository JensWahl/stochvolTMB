devtools::load_all()
library(tidyverse)

N <- 3000

# Gaussian case
param <- list(phi = 0.9, sigma_h = 0.4, sigma_y = 0.2, df = 5, alpha = -5, rho = -0.7)

method <- "skew_gaussian_leverage"
# method <- 'skew_gaussian' method <- 't'
dat <- stochvolTMB::simSV(param = param, N = N, method = method, seed = 123)
obj <- stochvolTMB::get_nll(dat$y, method)
opt <- stochvolTMB::estimate_parameters(dat$y, method)
plot(opt)


# compare with stochvol package
library(stochvol)
draws <- stochvol::svlsample(dat$y, draws = 2000)
stochvol::paradensplot(draws)
stochvol::paratraceplot(draws)
stochvol::volplot(draws)

mcmc_h <- draws$latent
mcmc_h <- draws$summary$latent
# demean
mcmc_h <- mcmc_h - colMeans(draws$para)[1]
ts.plot(mcmc_h)
tmb_h <- opt$report %>% filter(type == "random") %>% select(estimate)
lines(tmb_h$estimate, col = "red")


newquants <- c(0.025, 0.5, 0.975)
draws <- stochvol::updatesummary(draws, quantiles = newquants)
stochvol::volplot(draws)
lines(exp(tmb_h$estimate/2) * 0.199 * 100, col = "red", ylab = "")

sigma_y <- opt$report %>% dplyr::filter(parameter == "sigma_y") %>% dplyr::select(estimate) %>% as.numeric()


tmb_sd <- opt$report %>% filter(type == "random") %>% mutate(time = 1:n(), h_upper = estimate + 2 * std_error, h_lower = estimate - 
    2 * std_error, volatility = 100 * sigma_y * exp(estimate/2), volatility_upper = 100 * sigma_y * exp(h_upper/2), volatility_lower = 100 * 
    sigma_y * exp(h_lower/2))

lines(tmb_sd$volatility, col = "red")
lines(tmb_sd$volatility_upper, col = "blue")
lines(tmb_sd$volatility_lower, col = "blue")

x <- stochvol
where <- grep("%", dimnames(x$summary$latent)[[2]])
volquants <- t(100 * exp(x$summary$latent[, where, drop = FALSE]/2))  # monotone transformation!
nvolquants <- dim(volquants)[1]
timelen <- dim(volquants)[2]
