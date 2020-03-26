library(TMB)
library(sn)
compile("scripts/skew_normal.cpp")
dyn.load(dynlib("src/stochvolTMB.cpp"))
alpha <- 2
n <- 1000
set.seed(342)


delta <- alpha / sqrt(1 + alpha^2)
omega <- 1 / sqrt( 1 - 2 * delta^2 / pi)
epsilon <- -omega * delta * sqrt(2 / pi)

x <- rsn(n, alpha = alpha, xi = epsilon, omega = omega)
attr(x, "family") <- NULL
attr(x, "parameters") <- NULL


dat <- list(x = x, mod = 0)
param <- list(alpha = 2)

obj <- MakeADFun(dat, param, DLL = "skew_normal")
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt$par
rep <- sdreport(obj)
rep
