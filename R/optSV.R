#' Construct objective function with derivatives using \link[TMB]{MakeADFun}
#'@param data Vector of observations.
#'@param model String specifying distribution of error term in observational equation.
#'@param ... Additional arguments passed to \link[TMB]{MakeADFun}.
#'@return List of components returned from \link[TMB]{MakeADFun}.
#'@export
#'@keywords internal
get_nll <- function(data, model = "gaussian", ...) {

  # Starting values for parameters
  param <- list(log_sigma_y = 0,
                log_sigma_h = 0, 
                logit_phi = 3,
                log_df_minus_two = if (model == "t") 2 else numeric(0),
                alpha = if (model %in% c("skew_gaussian")) 0 else numeric(0),
                logit_rho = if (model %in% c("leverage")) 0 else numeric(0),
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
  if (!(model %in% c("gaussian", "skew_gaussian", "t", "leverage"))) stop("This model is not implemented")
  
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
#' report the parameters on the scale they were estimated, for example are all standard deviations estimated on 
#' log scale. "transformed" report all transformed parameters, for example estimated standard deviations 
#' transformed from log scale by taking the exponential. Lastly, "random"
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
    cat("degrees of freedom of observed variable        df =", rep[parameter == "df", estimate], "\n")
  if ("rho" %in% rep[, parameter]) 
    cat("leverage effect                                 rho =", rep[parameter == "rho", estimate], "\n")
  if ("alpha" %in% rep[, parameter]) 
    cat("skewness of observed variable                   alpha = ", rep[parameter == "alpha", estimate], "\n", sep = "")
  
  return(invisible(x))
}


#' Predict future returns and future volatilities
#' 
#' Takes a \code{stochvolTMB} object and produces draws from the predictive distribution of the latent volatility 
#' and future log-returns. 
#' 
#' @param object A \code{stochvolTMB} object returned from \code{\link{estimate_parameters}}.
#' @param steps Integer specifying number of steps to predict. 
#' @param nsim Number of draws from the predictive distribution.
#' @param include_parameters  Logical value indicating if fixed parameters 
#' should be simulated from their asymptotic distribution, i.e. 
#' multivariate normal with inverse hessian as covariance matrix. 
#' @param ... Not is use. 
#' @return List of simulated values from the predictive distribution of the latent volatilities and log-returns.
#' @export

predict.stochvolTMB <- function(object, steps = 1L, nsim = 10000, include_parameters = TRUE, ...) {
  
  time <- std_error <- NULL
  
  
  if (!inherits(object, "stochvolTMB")) {
    stop("`object` has to be of class `stochvolTMB`")
  }
  
  if (steps < 1) {
    stop("`steps` has to be greater or equal to 1")
  }
  
  # We need an extra step for the volatility to predict the last log-return
  if (object$model == "leverage") steps <- steps + 1
  
  # Get parameter estimates 
  rep <- summary(object)

  # Simulate parameters
  if (include_parameters) {
    
    sim_parameters <- simulate_parameters(object, nsim = nsim)
    if (object$model != "leverage") rho <- 0 else rho <- sim_parameters[, which(colnames(sim_parameters) == "rho")]
    if (object$model != "t") df <- Inf else df <-  sim_parameters[, which(colnames(sim_parameters) == "df")]
    if (object$model != "skew_gaussian") alpha <- 0 else alpha <-  sim_parameters[, which(colnames(sim_parameters) == "alpha")]
    sigma_y <-  sim_parameters[, which(colnames(sim_parameters) == "sigma_y")]
    sigma_h <- sim_parameters[, which(colnames(sim_parameters) == "sigma_h")]
    phi <- sim_parameters[, which(colnames(sim_parameters) == "phi")]
    
  } else {
    
    if (object$model != "leverage") rho <- 0 else rho <- rep[parameter == "rho", estimate]
    if (object$model != "t") df <- Inf else df <- rep[parameter == "df", estimate]
    if (object$model != "skew_gaussian") alpha <- 0 else df <- rep[parameter == "alpha", estimate]
    sigma_y <- rep[parameter == "sigma_y", estimate]
    sigma_h <- rep[parameter == "sigma_h", estimate]
    phi <- rep[parameter == "phi", estimate]
    
  }
  
  # Get last estimated volatility and simulate from its distribution to get nsim starting points
  h_last <- rep[.N, list(estimate, std_error)]
  h_pred <- matrix(0, nrow = steps, ncol = nsim)
  y_pred <- matrix(0, nrow = steps, ncol = nsim)
  h_start <- stats::rnorm(nsim, h_last$estimate, h_last$std_error)
  
  # Predict volatility 
  for (i in 1:steps) {
    if (i == 1) {
      h_pred[i, ] <- stats::rnorm(nsim, phi * h_start, sigma_h)
    } else {
      h_pred[i, ] <- stats::rnorm(nsim, phi * h_pred[i - 1, ], sigma_h)
    }
  }  
  
  if (object$model == "leverage") steps <- steps - 1
  
  # Predict log-returns
  for (i in 1:steps) {
    
    if (object$model == "skew_gaussian") {
      
      delta <- alpha / sqrt(1 + alpha^2)
      omega <- 1 / sqrt(1 - 2 * delta^2 / pi)
      xi <- -omega * delta * sqrt(2 / pi)
      y_pred[i, ] <- exp(h_pred[i, ] / 2) * sigma_y * sn::rsn(n = nsim, alpha = alpha, xi = xi, omega = omega)
      
    } else if (object$model == "t") {
      
      y_pred[i, ] <- exp(h_pred[i, ] / 2) * sigma_y * sqrt((df - 2) / df) * stats::rt(nsim, df = df)      

    } else if (object$model == "leverage") {
      
      y_pred[i, ] <- sigma_y * exp(h_pred[i, ] / 2) * (rho / sigma_h * (h_pred[i + 1, ] - phi * h_pred[i, ]) + sqrt(1 - rho^2) * stats::rnorm(nsim))
      
    } else {
      
      y_pred[i, ] <- exp(h_pred[i, ] / 2) * sigma_y * stats::rnorm(nsim)
      
    }
  }
  
  if (object$model == "leverage") {
    h_pred <- h_pred[1:steps, , drop = FALSE] 
    y_pred <- y_pred[1:steps, , drop = FALSE]
  }
    
  
  h_exp <- sigma_y * exp(h_pred / 2) 

  res <- list(y = y_pred, h = h_pred, h_exp = h_exp)
  attr(res, "steps") <- steps
  attr(res, "nsim") <- nsim
  class(res) <- "stochvolTMB_predict"
  
  return(res)
}

#' Calculate quantiles based on predictions from the predictive distribution
#' @param object A \code{stochvolTMB_summary} object.
#' @param ... Not used. 
#' @param quantiles A numeric vector specifying which quantiles to calculate. 
#' @param predict_mean bool. Should the mean be predicted? 
#' @return A list of \code{data.table}s. One for \code{y}, \code{h} and \code{h_exp}. 
#' @export
summary.stochvolTMB_predict <- function(object, ..., quantiles = c(0.025, 0.975), predict_mean = TRUE) {
  
  time <- NULL
  
  if (!inherits(object, "stochvolTMB_predict")) {
    stop("`object` should be of class stochvolTMB_predict")
  }
  
  if (anyNA(quantiles)) {
    stop("`quantiles` is NA")
  }
  
  if (length(quantiles) < 1) {
    stop("`quantiles` has to be of length 1 or greater")
  }
  
  if (!all(quantiles <= 1 & quantiles >= 0)) {
    stop("All elements of `quantiles` have to be between 0 and 1")
  }
  
  # If quantile has length 1 it returns a vector, not a matrix. When cast to matrix it will be a one column matrix
  # and should not be transposed
  res_quant <- lapply(object, function(x) {
    
    if (length(quantiles) == 1) {
      data.table(as.matrix(apply(x, 1, stats::quantile, probs = quantiles)))
    } else {
      data.table(t(apply(x, 1, stats::quantile, probs = quantiles)))
    }
  })
  
  lapply(res_quant, setnames, new = paste0("quantile_", quantiles))
  lapply(res_quant, function(x) x[, time := 1:.N])
  lapply(res_quant, data.table::setcolorder, "time")
  
  
  if (predict_mean) {
    res_mean <- lapply(object, function(x) apply(x, 1, mean))
    lapply(1:length(res_quant), function(x) res_quant[[x]][, mean := res_mean[[x]]])
  }
  
  return(res_quant)
  
}
