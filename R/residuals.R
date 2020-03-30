#' Calculate one-step-ahead (OSA) residuals for stochastic volatility model.
#' @param object A \code{stochvolTMB} object.
#' @return vector of one-step-ahead residuals of length \code{T}, where \code{T} is the number of observations. 
#' @export 

residuals <- function(object){
  residuals <- TMB::oneStepPredict(object$obj, observation.name="y", "keep", method="oneStepGeneric")
  return(residuals$residual)
}