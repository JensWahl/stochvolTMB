devtools::load_all()
N <- 3000

# Gaussian case
param <- list(phi = 0.9, 
              sigma_h = 0.4,
              sigma_y = 0.2,
              df = 5, 
              alpha = -5)

method <- "skew_gaussian"
#method <- "t"
dat <- stochvolTMB::simSV(param = param, N = N, method = method, seed = 123)

opt <- stochvolTMB::optSV(data = dat$y, method = method)

ts.plot(dat$h)
#lines(dat$y, col = "red")

h_est <- opt$rep[rownames(opt$rep) == "h", 1]
lines(h_est, col = "red")

data <- dat$y

# Starting values for parameters
param <- list(log_sigma_y = 0,
              log_sigma_h = 0, 
              phi_logit = 2,
              df = if(method == "t"){2}else{numeric(0)},
              alpha = if(method == "skew_gaussian"){-5}else{numeric(0)},
              h = rep(0, length(data)))
data <- list(y = data,
             method = ifelse(method == "gaussian", 0,
                             ifelse(method == "t", 1, 
                                    ifelse(method == "skew_gaussian", 2, 3))))

obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB")
opt <- stats::nlminb(obj$par, obj$fn, obj$gr, control = list(trace = TRUE),
                     lower = c(-Inf, -Inf, -Inf, -5), upper = c(Inf, Inf, Inf, -0.5))


prof <- TMB::tmbprofile(obj, "alpha", h = 1e-04, parm.range = c(-5, 5))
plot(prof)
