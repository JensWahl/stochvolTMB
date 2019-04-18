#' Function for parameter estimation of stochastic volatility model
#'@param data vector of observations 
#'@param method string specifying distribution of error term in observational equation
#'@return data.frame of parameter estimates and standard error of estimates
#'
optSV <- function(data, method = "gaussian"){
  
  # Starting values for parameters
  param <- list(log_sigma_y = 0,
                log_sigma_h = 0, 
                phi_logit = 2,
                df = if(method == "t"){2}else{numeric(0)},
                h = rep(0, length(data)))
  
  data <- list(y = data)
  
  obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB")
  opt <- stats::nlminb(obj$par, obj$fn, obj$gr)
  
  
  return(opt$par)
}