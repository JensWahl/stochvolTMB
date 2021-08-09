#' Calculate one-step-ahead (OSA) residuals for stochastic volatility model.
#' 
#' This function is very time consuming and by default computes the one-step-ahead residual for the last 100 observations. 
#' See the function \link[TMB]{oneStepPredict} and the paper in the references for more details. 
#' 
#' @param object A \code{stochvolTMB} object.
#' @param conditional Index vector of observations that are fixed during OSA. By default the residuals of the last 100
#' observations are calculated. If set to \code{NULL} it will calculate one-step-ahead residuals for all observations. 
#' @param ... Currently not used. 
#' @return Vector of one-step-ahead residuals. If the model is correctly specified, these should be standard normal.  
#' @references \url{https://www.researchgate.net/publication/316581864_Validation_of_ecological_state_space_models_using_the_Laplace_approximation}
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