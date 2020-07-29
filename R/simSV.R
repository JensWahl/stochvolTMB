#' Simulate data from the stochastic volatility model
#'
#' \code{sim_sv} simulate observations from a stochastic volatility model.
#'
#' This function draws the initial log-volatility  from its stationary distribution, meaning that \code{h_0}
#' is drawn from a gaussian distribution with mean zero and \code{sigma_h} / \code{sqrt(1 - phi^2)}. \code{h_{t+1}} is then simulated
#' from its conditional distibution given \code{h_t}, which is N(\code{phi*h_t}, \code{sigma_h}). Log-returns (\code{y_t}) is
#' simulated from its conditional distribution given the latent process \code{h}. If \code{model} = "gaussian", then \code{y_t} given \code{h_t}
#' is gaussian with mean zero and standard deviation equal to \code{sigma_y*exp(h_t / 2)}. Heavy tail returns can be obtained by simulating from
#' the t-distribution by setting \code{model} = "t". How heavy of a tail is specified by the degree of freedom parameter \code{df}.
#' Asymmetric returns is obtained from the "skew_gaussian" model. How asymmetric is governed by the skewness parameter \code{alpha}. The so called leverage
#' model, where we allow for correlation between log-returns and volatility can be simulated by setting \code{model} to "leverage" and specifying the
#' correlation parameter \code{rho}.
#'
#' @param nobs Length of time series
#' @param param List of parameters. This includes the standard deviation of the observations, \code{sigma_y},
#' the standard deviation of the latent volatility process, \code{sigma_h}, the persistence parameter \code{phi}. If
#' \code{model} = "t", the degree of freedom \code{df} must be specified. If \code{model} = "skew_gaussian", the skewness
#' parameter \code{alpha} must be specified and if \code{model} = "leverage", the correlation \code{rho} between the latent error
#' term and the observational error has to be spesified.
#' @param seed Seed to reproduce simulation
#' @param model Distribution of error term
#' @return data.table with columns \code{y} (observations) and \code{h} (latent log-volatility)
#' @export
sim_sv <- function(param = list(phi = 0.9, sigma_y = 0.4, sigma_h = 0.2, df = 4, alpha = -2, rho = -0.7), nobs = 1000, seed = NULL, model = "gaussian") {

  if (!is.list(param)){
    stop("param has to be a list")
  }
  
  if (param$sigma_y < 0){
    stop("The standard deviation sigma_y is negative")
  }
  
  if (param$sigma_h < 0){
    stop("The standard deviation sigma_h is negative")
  }
  
  if (!is.numeric(nobs)){
    stop("nobs has to be numeric")
  }
  
  if (nobs < 2){
    stop("nobs has to be greater than 1")
  }
  
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

    # parameter specific for the t-distribution
    df <- param$df

    y <- exp(h / 2) * sigma_y * stats::rt(nobs, df = df)
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
    
    if (abs(param$rho) >= 1){
      stop("Correlation parameter rho is not between -1 and 1")
    }
    # parameter specific for leverage model
    rho <- param$rho
    for (i in 1:(nobs - 1)) {
      y[i] <- sigma_y * exp(h[i] / 2) * (rho / sigma_h * (h[i + 1] - phi * h[i]) + sqrt(1 - rho^2) * stats::rnorm(1))
    }
    # set last value (not used) to zero
    y[nobs] <- 0
  } else {
    stop(paste0("The model ", model, " has not been implemented"))
  }

  # create data.table with simulated results, add parameters and model as attributes
  dt_sim <- data.table(y = y, h = h)
  attr(dt_sim, "param") <- param
  attr(dt_sim, "model") <- model
  
  
  return(dt_sim)
}
