compile("src/stochvolTMB.cpp")
dyn.load(dynlib("src/stochvolTMB"))


devtools::load_all()
library(tidyverse)

nobs <- 2000

# Gaussian case
param <- list(phi = 0.9, sigma_h = 0.4, sigma_y = 0.2, alpha = -2, rho = -0.7, df = 5)

model <- "t"
# model <- 'skew_gaussian' model <- 't'
dat <- stochvolTMB::sim_sv(param = param, nobs = nobs, model = model, seed = 123)
obj <- stochvolTMB::get_nll(dat$y, model = "gaussian", silent = TRUE, map = list(phi_logit = factor(NA)))
opt <- stochvolTMB::estimate_parameters(dat$y, model = model, map = list(phi_logit = factor(NA)))
res = residuals(opt)


plot(opt)

h = summary(opt, report = "random")

residuals = dat$y / exp(h$estimate / 2)


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


# load data
data("spy")

# estimate parameters 
opt <- estimate_parameters(spy$log_return, model = "gaussian")

# get parameter estimates with standard error
estimates <- summary(opt)

# plot volatility
plot(opt, include_ci = TRUE)
