devtools::load_all()
N <- 3000

# Gaussian case
param <- list(phi = 0.9, 
              sigma_h = 0.4,
              sigma_y = 0.2,
              df = 5, 
              alpha = 5,
              rho = -0.7)

method <- "leverage"
#method <- "skew_gaussian"
#method <- "t"
dat <- stochvolTMB::simSV(param = param, N = N, method = method, seed = 123)

opt <- stochvolTMB::optSV(data = dat$y, method = method)


h_est <- opt$report %>% 
  filter(type == "random")

plot(1:N, h_est$estimate, type = "l")
lines(1:N, dat$h, col = "green")

stochvol <- stochvol::svlsample(data$y)
h_stoch <- stochvol$summary$latent[, 1] - stochvol$summary$para[1,1]
lines(1:N, h_stoch, col = "red")

stochvol::paradensplot(stochvol)
stochvol::paratraceplot(stochvol)
stochvol::volplot(stochvol)
