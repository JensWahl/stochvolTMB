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
                alpha = if(method == "skew_gaussian"){-5}else{numeric(0)},
                rho = if(method == "leverage"){0}else{numeric(0)},
                h = rep(0, length(data)))
  
  data <- list(y = data,
               method = ifelse(method == "gaussian", 0,
                               ifelse(method == "t", 1, 
                                      ifelse(method == "skew_gaussian", 2, 3))))
  
  obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB")
  opt <- stats::nlminb(obj$par, obj$fn, obj$gr, control = list(trace = TRUE))
  rep <- summary(TMB::sdreport(obj))
  
  return(list(opt = opt, rep = rep))
}