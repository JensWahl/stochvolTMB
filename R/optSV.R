#' Construct objective function with derivatives using \link[TMB]{MakeADFun}
#'@param data vector of observations.
#'@param model string specifying distribution of error term in observational equation.
#'@param ... additional arguments passed to \link[TMB]{MakeADFun}.
#'@return List of components returned from \link[TMB]{MakeADFun}.
#'@export
#'@keywords internal
get_nll <- function(data, model = "gaussian", ...) {

  # Starting values for parameters
  param <- list(log_sigma_y = 0,
                log_sigma_h = 0, 
                phi_logit = 2,
                df = if (model == "t") 4 else numeric(0),
                alpha = if (model %in% c("skew_gaussian")) 0 else numeric(0),
                rho_logit = if (model %in% c("leverage")) 0 else numeric(0),
                h = rep(0, length(data)))
  
  data <- list(y = data,
               model = ifelse(model == "gaussian", 0,
                               ifelse(model == "t", 1, 
                                      ifelse(model == "skew_gaussian", 2,
                                             ifelse(model == "leverage", 3, 4)))))
  
  obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB", ...)
  
  return(obj)
  
}

#' Estimate parameters for the stochastic volatility model
#' 
#' @description 
#' Estimate parameters of a stochastic volatility model with a latent 
#' log-volatility following an 
#' autoregressive process of order one with normally distributed noise. 
#' The following distributions are implemented for the observed process: 
#' 
#' \itemize{
#' \item Gaussian distribution
#' \item t-distribution
#' \item Leverage: Gaussian distribution with leverage where the noise of the latent process is correlated with the
#'  observational distribution
#' \item Skew gaussian distribution
#' }
#' 
#' The parameters is estimated by minimizing the negative log-likelihood (nll) and the latent log-volatility is 
#' integrated out by applying the Laplace approximation.
#' 
#' @param data A vector of observations.
#' @param model A character specifying the model. Must be one of the following:
#'  "gaussian", "t", "leverage", "skew_gaussian".
#' @param opt.control An optional list of parameters for nlminb.
#' @param ... additional arguments passed to \link[TMB]{MakeADFun}.
#' 
#' @return Object of class \code{stochvolTMB}
#' 
#' @export 
#' 
#' @examples
#' \donttest{
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
#' }

estimate_parameters <- function(data, model = "gaussian", opt.control = NULL, ...) {
  
  if (!is.vector(data)) stop("data needs to be a vector")
  if (!is.character(model)) stop("model has to be a character")
  if (!(model %in% c("gaussian", "skew_gaussian", "t", "leverage"))) stop("This model not implemented")
  
  # create TMB object
  obj <- get_nll(data, model, ...)
  
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
#' Extract parameters, transformed parameters and latent log volatility along with standard error, z-value and p-value 
#' 
#' @rdname summary
#' @param object A \code{stochvolTMB} object.
#' @param ... Currently not used. 
#' @param report Parameters to report with uncertainty estimates. Can be any subset of "fixed", "transformed" or 
#' "random" (see \link[TMB]{summary.sdreport}). "fixed" 
#' report the parameters on the scale they were estimated, for example are all standard deviations are estimated
#' on log scale. "transformed" 
#' report all transformed parameters, for example estimated standard deviations transformed from log scale by taking
#'  the exponential. Lastly, "random"
#' report the estimated latent log-volatility. 
#' @return \code{data.table} with parameter estimates, standard error, z-value and approximated p-value.
#' @export 

summary.stochvolTMB <- function(object, ..., report = c("all", "fixed", "transformed", "random")) {
  
  # check argument
  report <- match.arg(report, several.ok = TRUE)
  
  if ("all" %in% report) report <- c("fixed", "transformed", "random")
  
  rep <- object$rep
  
  srep_fixed <- srep_report <- srep_random <- NULL
  column_names <- c("estimate", "std_error", "z_value", "p_value")
  
  if ("fixed" %in% report) {
    
    srep_fixed <- as.data.table(TMB::summary.sdreport(rep, select = c("fixed"), p.value = TRUE),
                                keep.rownames = "parameter")
    
    setnames(srep_fixed, 2:5, column_names)
    srep_fixed[, type := "fixed"]

  }
  
  if ("transformed" %in% report) {
    
    srep_report <- as.data.table(TMB::summary.sdreport(rep, select = c("report"), p.value = TRUE),
                                 keep.rownames = "parameter")
    
    setnames(srep_report, 2:5, column_names)
    srep_report[, type := "transformed"]

  }
  
  if ("random" %in% report) {
    srep_random <- as.data.table(TMB::summary.sdreport(rep, select = c("random"), p.value = TRUE),
                                 keep.rownames = "parameter")
    
    setnames(srep_random, 2:5, column_names)
    srep_random[, type := "random"]
  }
  
  
  srep <- rbindlist(list(srep_report, srep_fixed, srep_random))
  
  if (nrow(srep) == 0) {
    warning("no or empty summary selected via 'report = %s'", 
            deparse(report))
  }
  
  return(srep)
  
  
}




#' @export
print.stochvolTMB <- function(x, ...) {
  rep <- summary(x, report = c("fixed", "transformed"))
  
  cat("\nMaximum likelihood estimates for ", x$model, " model based on ", x$nobs, " observations.\n\n", sep = "")
  cat("Parameters:\n")
  cat("standard deviation of latent variable           sigma_h = ", rep[parameter == "sigma_h", estimate], "\n", sep = "")
  cat("persistence of latent variable                  phi = ", rep[parameter == "phi", estimate], "\n", sep = "")
  cat("standard deviation of observed variable         sigma_y = ", rep[parameter == "sigma_y", estimate], "\n", sep = "")
  if ("df" %in% rep[, parameter])
    cat("degrees of freedom for observed variable        df =", rep[parameter == "df", estimate], "\n")
  if ("rho" %in% rep[, parameter]) 
    cat("leverage effect                                 rho =", rep[parameter == "rho", estimate], "\n")
  if ("alpha" %in% rep[, parameter]) 
    cat("skewness of observed variable                   alpha = ", rep[parameter == "alpha", estimate], "\n", sep = "")
}


#' Predict future returns and future volatilities
#' 
#' Takes a \code{stochvolTMB} object and produces draws from the predictive distribution of the latent volatility 
#' and future log-returns. 
#' 
#' @param object A \code{stochvolTMB} object returned from \code{\link{estimate_parameters}}.
#' @param steps integer specifying number of steps to predict. 
#' @param nsim Number of draws from the predictive distribution.
#' @param ... Not is use. 
#' @return list of simulated values from the predictive distribution of the latent volatilities and log-returns.
#' 
#' @export

predict.stochvolTMB = function(object, steps = 1L, nsim = 1000, ...){
  
  rep = summary(object)
  
  if (object$model != "leverage") rho <- 0 else rho <- rep[parameter == "rho", estimate]
  if (object$model != "t") df <- Inf else df <- rep[parameter == "df", estimate]
  if (object$model != "skew_gaussian") alpha <- 0 else df <- rep[parameter == "alpha", estimate]
  sigma_y <- rep[parameter == "sigma_y", estimate]
  sigma_h <- rep[parameter == "sigma_h", estimate]
  phi <- rep[parameter == "phi", estimate]
  
  # Get last estimated volatility and simulate from its distribution to get nsim starting points
  h_last <- rep[.N, .(estimate, std_error)]
  h_pred <- matrix(0, nrow = steps, ncol = nsim)
  y_pred <- 0 * h_pred
  h_pred[1, ] <- rnorm(nsim, h_last$estimate, h_last$std_error)
  
  for(i in 2:(steps + 1)){
    h_pred[i, ] <- rnorm(nsim, h_pred[i - 1, ], sigma_h)
  }  
  
  
  for(i in 1:(steps + 1)){
    
    if (object$model == "skew_gaussian") {
      
      delta <- alpha / sqrt(1 + alpha^2)
      omega <- 1 / sqrt(1 - 2 * delta^2 / pi)
      xi <- -omega * delta * sqrt(2 / pi)
      y_pred[i, ] <- exp(h_pred[i, ] / 2) * sigma_y * sn::rsn(n = nsim, alpha = alpha, xi = xi, omega = omega)
      
    } else if (object$model == "t") {
      
      y_pred[i, ] <- exp(h_pred[i, ] / 2) * sigma_y * sqrt((df - 2) / df) * stats::rt(nsim, df = df)      

    } else if (object$model == "leverage") {
      
      y_pred[i, ] <- sigma_y * exp(h_pred[i, ] / 2) * (rho / sigma_h * (h[i + 1, ] - phi * h[i, ]) + sqrt(1 - rho^2) * stats::rnorm(nsim))
      
    } else {
      
      y_pred[i, ] <- exp(h_pred[i, ] / 2) * sigma_y * stats::rnorm(nsim)
      
    }
  }
  
  res <- list(y = y_pred, h = h_pred)
  class(res) = "stochvolTMB.predict"
  return(res)
}
