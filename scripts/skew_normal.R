library(TMB)
library(sn)
compile("scripts/skew_normal.cpp")
dyn.load(dynlib("scripts/skew_normal"))
alpha <- 2
n <- 100
set.seed(342)
x <- rsn(n, alpha = alpha)
attr(x, "family") <- NULL
attr(x, "parameters") <- NULL


dat <- list(x = x, mod = 0)
param <- list(alpha = 2)

obj <- MakeADFun(dat, param, DLL = "skew_normal")
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt$par
rep <- sdreport(obj)
rep
