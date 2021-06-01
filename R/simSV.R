#' Simulate log-returns from a stochastic volatility model
#'
#' @description 
#' This function draws the initial log-volatility (\code{h_t}) from its stationary distribution, meaning that \code{h_0}
#' is drawn from a gaussian distribution with mean zero and standard deviation \code{sigma_h} / \code{sqrt(1 - phi^2)}.
#' \code{h_{t+1}} is then simulated from its conditional distribution given \code{h_t}, 
#' which is N(\code{phi*h_t}, \code{sigma_h}). Log-returns (\code{y_t}) is
#' simulated from its conditional distribution given the latent process \code{h}. If \code{model} = "gaussian", 
#' then \code{y_t} given \code{h_t} is gaussian with mean zero and standard deviation equal to
#' \code{sigma_y*exp(h_t / 2)}. Heavy tail returns can be obtained by simulating from
#' the t-distribution by setting \code{model} = "t". How heavy of a tail is specified by the degree of freedom 
#' parameter \code{df}. Note that the observations are scaled by \code{sqrt((df-2)/2)} so that the error term has 
#' variance equal to one. Asymmetric returns are obtained from the "skew_gaussian" model. How asymmetric is governed by
#' the skewness parameter \code{alpha}. The so called leverage model, where we allow for correlation between 
#' log-returns and volatility can be simulated by setting \code{model} to "leverage" and specifying the
#' correlation parameter \code{rho}.
#'
#' @param nobs Length of time series.
#' @param param List of parameters. This includes the standard deviation of the observations, \code{sigma_y},
#' the standard deviation of the latent volatility process, \code{sigma_h}, the persistence parameter \code{phi}. If
#' \code{model} = "t", the degree of freedom \code{df} must be specified. If \code{model} = "skew_gaussian", 
#' the skewness parameter \code{alpha} must be specified and if \code{model} = "leverage", 
#' the correlation \code{rho} between the latent error term and the observational error has to be specified.
#' @param seed Seed to reproduce simulation.
#' @param model Distribution of error term.
#' @return data.table with columns \code{y} (observations) and \code{h} (latent log-volatility).
#' @export
sim_sv <- function(param = list(phi = 0.9, sigma_y = 0.4, sigma_h = 0.2, df = 4, alpha = -2, rho = -0.7),
                   nobs = 1000L, 
                   seed = NULL, 
                   model = "gaussian") {

  if (!is.list(param)) {
    stop("param has to be a list")
  }
  
  if (param$sigma_y < 0) {
    stop("The standard deviation `sigma_y` is negative")
  }
  
  if (param$sigma_h < 0) {
    stop("The standard deviation `sigma_h` is negative")
  }
  
  if (abs(param$phi) >= 1) {
    stop("Persistence parameter `phi` is not between -1 and 1")
  }
  
  if (!is.numeric(nobs)) {
    stop("`nobs` has to be numeric")
  }
  
  if (nobs < 2) {
    stop("`nobs` has to be greater than 1")
  }
  
  # We need an extra observation to simulate the last y
  if (model == "leverage") nobs <- nobs + 1
  
  # Set seed if specified
  if (!is.null(seed)) set.seed(seed)

  phi <- param$phi
  sigma_y <- param$sigma_y
  sigma_h <- param$sigma_h

  # Latent process
  h <- rep(NA, nobs)

  # We assume stationary distribution
  h[1] <- stats::rnorm(1, 0, sigma_h / sqrt(1 - phi^2))

  for (t in 2:nobs) {
    h[t] <- phi * h[t - 1] + stats::rnorm(1, 0, sigma_h)
  }

  # Observations
  y <- rep(NA, nobs)
  
  if (model == "gaussian") {
    
    y <- exp(h / 2) * stats::rnorm(nobs, 0, sigma_y)
    
  } else if (model == "t") {

    if (param$df <= 2) {
      stop("Degrees of freedom parameter `df` has to be greater than 2")
    }
    # parameter specific for the t-distribution
    df <- param$df
    y <- exp(h / 2) * sigma_y * sqrt((df - 2) / df) * stats::rt(nobs, df = df)
    
  } else if (model == "skew_gaussian") {

    # parameter specific for the skew normal distribution
    # use package sn do generate random sample from skew normal distribution
    alpha <- param$alpha
    delta <- alpha / sqrt(1 + alpha^2)
    omega <- 1 / sqrt(1 - 2 * delta^2 / pi)
    xi <- -omega * delta * sqrt(2 / pi)
    y <- exp(h / 2) * sigma_y * sn::rsn(n = nobs, alpha = alpha, xi = xi, omega = omega)

    # remove attributes specific for rsn
    attr(y, "family") <- NULL
    attr(y, "parameters") <- NULL
    
  } else if (model == "leverage") {
    
    if (abs(param$rho) >= 1) {
      stop("Correlation parameter `rho` is not between -1 and 1")
    }
    # parameter specific for leverage model
    rho <- param$rho
    for (i in 1:(nobs - 1)) {
      y[i] <- sigma_y * exp(h[i] / 2) * (rho / sigma_h * (h[i + 1] - phi * h[i]) + sqrt(1 - rho^2) * stats::rnorm(1))
    }
    # Remove the last value
    y <- y[-nobs]
    h <- h[-nobs]
    
  } else {
    stop(paste0("The model ", model, " has not been implemented"))
  }

  # create data.table with simulated results, add parameters and model as attributes
  dt_sim <- data.table(y = y, h = h)
  attr(dt_sim, "param") <- param
  attr(dt_sim, "model") <- model
  
  return(dt_sim)
}

#' Logit transformation from the real line to (-1, 1). 
#' @param x double 
#' @return double
#' @export
logit <- function(x) (exp(x) - 1) / (1 + exp(x))


#' Simulate from the asymptotic distribution of the parameter estimates 
#' 
#' Sampling is done on the scale the parameters were estimated. The standard deviations are simulated on log-scale 
#' and the persistence is simulated on logit scale. The same is true for the correlation parameter in the leverage model. 
#' 
#' @param object A \code{stochvolTMB} object.
#' @param nsim Number of simulations.
#' @return matrix of simulated values. 
#' @export
simulate_parameters <- function(object, nsim = 1000) {
  
  # Get covariance matrix
  cov_mat <- object$rep$cov.fixed  
  par_est <- object$fit$par
  
  if (!all(names(par_est) %in% colnames(cov_mat))) {
    stop("The name of the estimated parameters is not the same as in the covariance matrix.")
  }
  
  if (!all(names(par_est) == colnames(cov_mat))) {
    # Get same order in mean vector and covariance matrix
    par_est <- par_est[match(names(par_est), colnames(cov_mat))]
  }
  
  par_sim <- MASS::mvrnorm(nsim, mu = par_est, Sigma = cov_mat)
  
  # Transform sample to their original scale 
  par_sim[, grepl("log_sigma", colnames(par_sim))] <- exp(par_sim[, grepl("log_sigma", colnames(par_sim))]) 
  par_sim[, grepl("logit", colnames(par_sim))] <- logit(par_sim[, grepl("logit", colnames(par_sim))]) 
  par_sim[, grepl("log_df_minus_two", colnames(par_sim))] <- exp(par_sim[, grepl("log_df_minus_two", colnames(par_sim))]) + 2 
  
  colnames(par_sim) <- gsub("log_sigma", "sigma", colnames(par_sim))
  colnames(par_sim) <- gsub("logit_", "", colnames(par_sim))
  colnames(par_sim) <- gsub("log_df_minus_two", "df", colnames(par_sim))
  
  return(par_sim)
  
}
