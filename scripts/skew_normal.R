library(TMB)
library(sn)
compile("scripts/skew_normal.cpp")
dyn.load(dynlib("scripts/skew_normal"))
alpha <- 2
n <- 100
set.seed(342)


delta <- alpha / sqrt(1 + alpha^2)
omega <- 1 / sqrt( 1 - 2 * delta^2 / pi)
epsilon <- -omega * delta * sqrt(2 / pi)

x <- rsn(n, alpha = alpha, xi = epsilon, omega = omega)
attr(x, "family") <- NULL
attr(x, "parameters") <- NULL


dat <- list(x = x, mod = 1)
param <- list(alpha = 2)

obj <- MakeADFun(dat, param, DLL = "skew_normal")
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt$par
rep <- sdreport(obj)
rep
