#' Function for parameter estimation of stochastic volatility model
#'
#'
optSV <- function(data, parameters, method = "gaussion"){
  
  opt <- TMB::MakeADFun(data = data, parameters = parameters, DLL = "sv_likelihood")
  
  return(opt$par)
}