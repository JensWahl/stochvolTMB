devtools::load_all()
library(tidyverse)

N <- 3000

# Gaussian case
param <- list(phi = 0.9, 
              sigma_h = 0.4,
              sigma_y = 0.2,
              df = 5, 
              alpha = 5,
              rho = -0.7)

method <- "leverage"
method <- "skew_gaussian"
#method <- "t"
dat <- stochvolTMB::simSV(param = param, N = N, method = method, seed = 123)

opt <- stochvolTMB::optSV(data = dat$y, method = method)
opt$report

volplot(opt$report)


$ compare with stochvol package
# stochvol <- stochvol::svlsample(data$y)
# stochvol::paradensplot(stochvol)
# stochvol::paratraceplot(stochvol)
# stochvol::volplot(stochvol)
