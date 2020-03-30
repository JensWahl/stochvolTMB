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
obj <- stochvolTMB::get_nll(dat$y, model = "gaussian", silent = TRUE, map = list(phi_logit = factor(NA)))
opt <- stochvolTMB::estimate_parameters(dat$y, model = model, map = list(phi_logit = factor(NA)))
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


# load data
data("spy")

# estimate parameters 
opt <- estimate_parameters(spy$log_return, model = "gaussian")

# get parameter estimates with standard error
estimates <- summary(opt)

# plot volatility
plot(opt, include_ci = TRUE)
