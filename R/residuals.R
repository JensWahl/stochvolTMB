#' Calculate one-step-ahead (OSA) residuals for stochastic volatility model.
#' 
#' @param object A \code{stochvolTMB} object.
#' @oparam conditional Index vector of observations that are fixed during OSA. By default the residuals of the last 100
#' observations are calculated. 
#' @param ... Currently not used. 
#' @return Vector of one-step-ahead residuals. If the model is correctly specified, these should be standard normal.  
#' @export 

residuals <- function(object, conditional = 1:(object$nobs - 100), ...) {
  
residuals <- TMB::oneStepPredict(object$obj, 
                                 observation.name = "y",
                                 data.term.indicator = "keep",
                                 discrete = FALSE,
                                 method = "oneStepGeneric",
                                 conditional = conditional,
                                 parallel = TRUE,
                                 reverse = TRUE)
  
  
    return(residuals$residual)
}