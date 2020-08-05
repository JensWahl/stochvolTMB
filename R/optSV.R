#' Construct objective function with derivatives using \link[TMB]{MakeADFun}
#'@param data vector of observations.
#'@param model string specifying distribution of error term in observational equation.
#'@param ... additional arguments passed to \link[TMB]{MakeADFun}.
#'@return List of components returned from \link[TMB]{MakeADFun}.
#'@export
#'@keywords internal
get_nll <- function(data, model = "gaussian", ...){

  # Starting values for parameters
  param <- list(log_sigma_y = 0,
                log_sigma_h = 0, 
                phi_logit = 2,
                df = if (model== "t") {2} else {numeric(0)},
                alpha = if (model%in% c("skew_gaussian")) {0} else {numeric(0)},
                rho_logit = if (model%in% c("leverage")) {0} else {numeric(0)},
                h = rep(0, length(data)))
  
  data <- list(y = data,
               model= ifelse(model== "gaussian", 0,
                               ifelse(model== "t", 1, 
                                      ifelse(model== "skew_gaussian", 2,
                                             ifelse(model== "leverage", 3, 4)))))
  
  obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB", ...)
  
  return(obj)
  
}

#' Estimate parameters for the stochastic volatility model
#' 
#' @description 
#' Estimate parameters of a stochastic volatility model with a latent log-volatility following an 
#' autoregressive process of order one with normally distributed noise. The following distributions are implemented for the observed process: 
#' 
#' \itemize{
#' \item Gaussian distribution
#' \item t-distribution
#' \item Leverage: Gaussian distribution with leverage where the noise of the latent process is correlated with the observational distribution
#' \item Skew gaussian distribution
#' }
#' 
#' The parameters is estimated by minimizing the negative log-likelihood (nll) and the latent log-volatility is integrated out by applying the
#' Laplace approximation.
#' 
#' @param data A vector of observations.
#' @param model A character specifying the model. Must be one of the following: "gaussian", "t", "leverage", "skew_gaussian".
#' @param opt.control An optional list of parameters for nlminb.
#' @param ... additional arguments passed to \link[TMB]{MakeADFun}.
#' 
#' @return Object of class \code{stochvolTMB}
#' 
#' @export 
#' 
#' @examples
#' # load data
#' data("spy")
#'
#' # estimate parameters 
#' opt <- estimate_parameters(spy$log_return, model = "gaussian")
#'
#' # get parameter estimates with standard error
#' estimates <- summary(opt)
#'
#' # plot estimated volatility with 95 % confidence interval
#' plot(opt, include_ci = TRUE)
#' 

estimate_parameters <- function(data, model = "gaussian", opt.control = NULL, ...){
  
  if (!is.vector(data)) stop("data needs to be a vector")
  if (!is.character(model)) stop("model has to be a character")
  if (!(model %in% c("gaussian", "skew_gaussian", "t", "leverage"))) stop("This model not implemented")
  
  # create TMB object
  obj = get_nll(data, model, ...)
  
  # Optimize nll 
  fit <- stats::nlminb(obj$par, obj$fn, obj$gr, opt.control)
  
  # Calculate standard error for all parameters (including latent)
  rep <- TMB::sdreport(obj)

  
  opt <- list()
  class(opt) <- "stochvolTMB"

  opt$rep <- rep
  opt$obj <- obj
  opt$fit <- fit

  opt$nobs <- length(data)
  opt$model <- model

  return(opt)
}
  

#' @export
logLik.stochvolTMB <- function(object, ...) {
  val <- -object$fit$objective
  attr(val, "nobs") <- object$nobs
  attr(val, "df") <- length(object$fit$par)
  class(val) <- "logLik"
  val
}


#' Summary tables of model parameters 
#' 
#' Extract parameters, transformed parameters and latent log volatility along with standard error and p-value 
#' 
#' @rdname summary
#' @param object A \code{stochvolTMB} object.
#' @param ... Currently not used. 
#' @param report Parameters to report with uncertainty estimates. Can be any subset of "fixed", "transformed" or "random" (see \link[TMB]{summary.sdreport}). "fixed" 
#' report the parameters on the scale they were estimated, for example are all standard deviations are estimated on log scale. "transformed" 
#' report all transformed parameters, for example estimated standard deviations transformed from log scale by taking the exponential. Lastly, "random"
#' report the estimated latent log-volatility. 
#' @return \code{data.table} with parameter estimates, standard error and approximated p-value.
#' @export 

summary.stochvolTMB <- function(object, ..., report = c("all", "fixed", "transformed", "random")){
  
  # check argument
  report <- match.arg(report, several.ok = TRUE)
  
  if ("all" %in% report) report <- c("fixed", "transformed", "random")
  
  rep <- object$rep
  
  srep_fixed <- srep_report <- srep_random <- NULL
  column_names <- c("estimate", "std_error", "z_value", "p_value")
  
  if ("fixed" %in% report){
    
    srep_fixed <- as.data.table(TMB::summary.sdreport(rep, select = c("fixed"), p.value = TRUE), keep.rownames = "parameter")
    setnames(srep_fixed, 2:5, column_names)
    srep_fixed[, type := "fixed"]

  }
  
  if ("transformed" %in% report){
    
    srep_report <- as.data.table(TMB::summary.sdreport(rep, select = c("report"), p.value = TRUE), keep.rownames = "parameter")
    setnames(srep_report, 2:5, column_names)
    srep_report[, type := "transformed"]

  }
  
  if ("random" %in% report){
    srep_random <- as.data.table(TMB::summary.sdreport(rep, select = c("random"), p.value = TRUE), keep.rownames = "parameter")
    setnames(srep_random, 2:5, column_names)
    srep_random[, type := "random"]
  }
  
  
  srep <- rbindlist(list(srep_report, srep_fixed, srep_random))
  
  if (nrow(srep) == 0){
    warning("no or empty summary selected via 'report = %s'", 
            deparse(report))
  }
  
  return(srep)
  
  
}


