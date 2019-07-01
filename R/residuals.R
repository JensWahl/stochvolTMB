#' Calculate one-step-ahead (OSA) residuals for stochastic volatility model.
#' @param obj Output from optSV
#' @return vector of one-step-ahead residuals of length \code{T}, where \code{T} is the number of observations. 
#' @export 

residuals <- function(obj){
  residuals <- TMB::oneStepPredict(opt$obj,observation.name="y", "keep", method="oneStepGeneric")
  return(residuals$residual)
}