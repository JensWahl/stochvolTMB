compile("src/stochvolTMB.cpp")
dyn.load(dynlib("src/stochvolTMB"))


devtools::load_all()
library(tidyverse)

N <- 2000

# Gaussian case
param <- list(phi = 0.9, sigma_h = 0.4, sigma_y = 0.2, alpha = -2, rho = -0.7)

model <- "gaussian"
# model <- 'skew_gaussian' model <- 't'
dat <- stochvolTMB::sim_sv(param = param, N = N, model = model, seed = 123)
obj <- stochvolTMB::get_nll(dat$y, model = "gaussian", silent = TRUE)
opt <- stochvolTMB::estimate_parameters(dat$y, model = model)
plot(opt)

model = "skew_gaussian_leverage"
data = dat$y
param <- list(log_sigma_y = 0,
              log_sigma_h = 0, 
              phi_logit = 2.95,
              df = if(model == "t"){2}else{numeric(0)},
              alpha = if(model %in% c("skew_gaussian", "skew_gaussian_leverage")) {-0} else {numeric(0)},
              rho_logit = if(model %in% c("leverage", "skew_gaussian_leverage")) {-2} else {numeric(0)},
              h = rep(0, length(data)))

data <- list(y = data,
             model = ifelse(model == "gaussian", 0,
                             ifelse(model == "t", 1, 
                                    ifelse(model == "skew_gaussian", 2,
                                           ifelse(model == "leverage", 3,
                                                  ifelse(model == "skew_gaussian_leverage", 4, 5))))))




map = list(alpha = as.factor(NA), rho_logit = as.factor(NA))
obj = TMB::MakeADFun(data, param, random = "h", map = map, DLL = "stochvolTMB")
opt = nlminb(obj$par, obj$fn, obj$gr)
opt$par



X = quantmod::getSymbols("^GSPC", env = NULL, src = "yahoo",
                         from = as.Date("2005-01-01"), to = as.Date("2019-01-01"))

X = X$GSPC.Close

X <- tibble(date = index(X), price = as.vector(X$GSPC.Close))
X <- X %>% 
  mutate(log_return = log(price) - log(lag(price))) %>% 
  na.omit() %>% 
  mutate(log_return = log_return - mean(log_return))
spy <- X
save(spy, file = "data/spy.RData")
opt1 <- stochvolTMB::estimate_parameters(spy$log_return, model = "leverage")
opt2 <- stochvolTMB::estimate_parameters(spy$log_return, model = "t")
opt3 <- stochvolTMB::estimate_parameters(spy$log_return, model = "skew_gaussian")
opt4 <- stochvolTMB::estimate_parameters(spy$log_return, model = "gaussian")

AIC(opt1)
AIC(opt2)
AIC(opt3)
AIC(opt4)



data("exrates")
dat = stochvol::logret(exrates$USD)
opt1 <- stochvolTMB::estimate_parameters(dat, model = "leverage")
opt2 <- stochvolTMB::estimate_parameters(dat, model = "t")
opt3 <- stochvolTMB::estimate_parameters(dat, model = "skew_gaussian")
opt4 <- stochvolTMB::estimate_parameters(dat, model = "skew_gaussian_leverage")
opt5 <- stochvolTMB::estimate_parameters(dat, model = "gaussian")



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
