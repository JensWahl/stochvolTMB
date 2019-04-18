devtools::load_all()
N <- 3000

# Gaussian case
param <- list(phi = 0.9, 
              sigma_h = 0.4,
              sigma_y = 0.2,
              df = 5, 
              alpha = -1)

method <- "skew_gaussian"
#method <- "t"
dat <- stochvolTMB::simSV(param = param, N = N, method = method)

opt <- stochvolTMB::optSV(data = dat$y, method = method)
ts.plot(dat$h)
lines(dat$y, col = "red")

h_est <- opt$rep[rownames(opt$rep) == "h", 1]
lines(h_est, col = "red")


