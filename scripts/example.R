devtools::load_all()
library(TMB)
T <- 1000

# Gaussian case
param <- list(phi = 0.9, 
              sigma_h = 0.4,
              sigma_y = 0.2)


data <- stochvolTMB::simSV(param = param, T = T)

opt <- stochvolTMB::optSV(data = data$y, method = "gaussian")
ts.plot(data$h)
lines(data$y, col = "red")


# t distribution

param <- list(phi = 0.9, 
              sigma_h = 0.4,
              sigma_y = 0.2,
              df = 5)

data_t <- simSV(param = param, T = T, method = "t")
ts.plot(data_t$h)
lines(data_t$y, col = "red")

opt <- stochvolTMB::optSV(data = data_t$y, method = "t")

compile("src/stochvolTMB.cpp")
dyn.load(dynlib("src/stochvolTMB"))

method <- "t"
param <- list(log_sigma_y = 0,
              log_sigma_h = 0,
              phi_logit = 2,
              df = if(method == "t"){4}else{numeric(0)},
              h = rep(0, T))
#data <- list(y = data$y)
obj <- MakeADFun(data = list(y = data_t$y, method = 1), param, random = "h", DLL = "stochvolTMB")
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt


