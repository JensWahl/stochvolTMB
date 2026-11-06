#' Calculate one-step-ahead (OSA) residuals for stochastic volatility model.
#'
#' The OSA residual of observation \code{y_t} is \code{qnorm(F_t(y_t))}, where \code{F_t} is the predictive
#' distribution function of \code{y_t} given \code{y_1, ..., y_{t-1}} at the estimated parameters. If the model is
#' correct, the residuals are independent and standard gaussian.
#'
#' Two methods are available:
#' \itemize{
#' \item \code{"grid"} (default): an exact filter on a grid for the log-volatility \code{h_t}. Because \code{h_t}
#'  is one-dimensional, the predictive distribution can be calculated by summation over the grid, without the
#'  Laplace approximation. It calculates the residuals for all observations in a few seconds.
#' \item \code{"tmb"}: \link[TMB]{oneStepPredict} with \code{method = "oneStepGeneric"}. It uses the Laplace
#'  approximation and takes about one to three seconds per observation, so by default only the last 100
#'  observations are used. It is not available for the \code{"skew_t_leverage"} model, where \code{y_t} also enters
#'  the model for \code{h_{t+1}}.
#' }
#'
#' @param object A \code{stochvolTMB} object.
#' @param conditional Index vector of observations that are not predicted. Residuals are returned for the other
#'   observations. If \code{NULL}, the \code{"grid"} method returns residuals for all observations, and the
#'   \code{"tmb"} method uses \code{1:(object$nobs - 100)}. Use \code{integer(0)} to get all residuals with
#'   \code{"tmb"}.
#' @param method \code{"grid"} or \code{"tmb"}. See details.
#' @param grid_size Number of grid points for \code{h_t} in the \code{"grid"} method.
#' @param ... Currently not used.
#' @return Vector of one-step-ahead residuals. If the model is correctly specified, these should be standard normal.
#' @references Thygesen, U. H., Albertsen, C. M., Berg, C. W., Kristensen, K. and Nielsen, A. (2017). Validation
#'   of ecological state space models using the Laplace approximation. Environmental and Ecological Statistics,
#'   24(2), 317-339. \doi{10.1007/s10651-017-0372-4}
#' @export

residuals.stochvolTMB <- function(object, conditional = NULL, method = c("grid", "tmb"), grid_size = 500, ...) {

  method <- match.arg(method)

  if (method == "grid") {
    res <- osa_grid(object, grid_size = grid_size)
    if (!is.null(conditional)) res <- res[setdiff(seq_len(object$nobs), conditional)]
    return(res)
  }

  if (object$model == "skew_t_leverage") {
    stop("method = \"tmb\" is not available for the skew_t_leverage model. Use method = \"grid\".")
  }

  if (is.null(conditional)) conditional <- 1:(object$nobs - 100)

  # With parallel = TRUE, TMB calls openmp() without a DLL name, which fails when several TMB DLLs are loaded
  # (for example after devtools::load_all() next to the installed package).
  parallel <- n_tmb_dlls() <= 1
  if (!parallel) {
    message("Several TMB models are loaded, so the residuals are calculated without parallel processing.")
  }

  residuals <- TMB::oneStepPredict(object$obj,
                                   observation.name = "y",
                                   data.term.indicator = "keep",
                                   discrete = FALSE,
                                   method = "oneStepGeneric",
                                   conditional = conditional,
                                   parallel = parallel,
                                   reverse = TRUE)

  return(residuals$residual)
}

#' Exact one-step-ahead residuals by filtering on a grid for the log-volatility
#'
#' @param object A \code{stochvolTMB} object.
#' @param grid_size Number of grid points.
#' @return Vector of one-step-ahead residuals for all observations.
#' @keywords internal
osa_grid <- function(object, grid_size = 500) {
  prob <- grid_filter(object, grid_size = grid_size)$prob
  stats::qnorm(pmin(pmax(prob, 1e-16), 1 - 1e-16))
}

#' Filter on a grid for the log-volatility
#'
#' Keeps the predictive density of \code{h_t} on a grid. For each observation it calculates the predictive
#' distribution function of \code{y_t}, updates the density with \code{y_t} and moves it one step forward.
#' Grid points with negligible weight are skipped, so the cost per step is small.
#'
#' @param object A \code{stochvolTMB} object.
#' @param grid_size Number of grid points.
#' @param width Half-width of the grid in stationary standard deviations of \code{h_t}.
#' @return A list with \code{prob}, the predictive distribution function at each \code{y_t}; \code{weights}, a list
#'   of length \code{n + 1} where element \code{t} holds the grid indices \code{idx} and the normalized predictive
#'   weights \code{w} of \code{h_t} given \code{y_1, ..., y_{t-1}} (element \code{n + 1} is the forecast); and
#'   \code{scale}, the conditional standard deviation \code{sigma_y * exp(h / 2)} at each grid point.
#' @keywords internal
grid_filter <- function(object, grid_size = 500, width = 8) {

  model <- object$model
  y <- object$obj$env$data$y
  n <- length(y)
  par <- coef(object)
  sigma_y <- par[["sigma_y"]]
  sigma_h <- par[["sigma_h"]]
  phi <- par[["phi"]]
  rho <- if ("rho" %in% names(par)) par[["rho"]] else 0
  c_rho <- sqrt(1 - rho^2)

  sd0 <- sigma_h / sqrt(1 - phi^2)
  g <- seq(-width * sd0, width * sd0, length.out = grid_size)
  s <- sigma_y * exp(g / 2)

  # Given h_t, y_t / s(h_t) has the standardized error distribution of the model. In the leverage model this holds
  # once h_{t+1} is integrated out.
  std <- standard_error_distribution(object)

  # Rows are normalized so that no probability mass is lost at the edges of the grid. A row is all zero only when
  # its mean is far outside the grid, which happens for grid points with negligible weight, so it stays zero.
  transition <- function(mean, sd) {
    m <- stats::dnorm(outer(mean, g, function(a, b) (b - a) / sd))
    total <- rowSums(m)
    m / ifelse(total > 0, total, 1)
  }
  P <- transition(phi * g, sigma_h)

  w <- stats::dnorm(g, 0, sd0)
  w <- w / sum(w)
  prob <- numeric(n)
  weights <- vector("list", n + 1)

  for (t in seq_len(n)) {

    idx <- which(w > 1e-14 * max(w))
    wt <- w[idx] / sum(w[idx])
    weights[[t]] <- list(idx = idx, w = wt)
    z <- y[t] / s[idx]
    prob[t] <- sum(wt * std$cdf(z))

    if (model == "leverage") {
      # y_t depends on h_t and h_{t+1}, so update with their joint density
      eta <- outer(g[idx], g, function(h, h_next) (h_next - phi * h) / sigma_h)
      u <- (z - rho * eta) / c_rho
      w <- colSums(wt * P[idx, , drop = FALSE] * stats::dnorm(u) / (s[idx] * c_rho))
    } else {
      logp <- std$logpdf(z) - log(s[idx])
      post <- wt * exp(logp - max(logp))
      if (model %in% c("skew_gaussian_leverage", "skew_t_leverage")) {
        # The return shock drives the volatility shock, so the transition depends on y_t
        w <- colSums(post * transition(phi * g[idx] + sigma_h * rho * z, sigma_h * c_rho))
      } else {
        w <- colSums(post * P[idx, , drop = FALSE])
      }
    }
    w <- w / sum(w)
  }

  idx <- which(w > 1e-14 * max(w))
  weights[[n + 1]] <- list(idx = idx, w = w[idx] / sum(w[idx]))

  list(prob = prob, weights = weights, scale = s)
}

#' Standardized error distribution of a fitted model
#'
#' The distribution of \code{y_t / (sigma_y * exp(h_t / 2))} given \code{h_t}. It has mean zero and unit variance.
#'
#' @param object A \code{stochvolTMB} object.
#' @return A list of vectorized functions: \code{cdf}, \code{logpdf}, \code{quantile} and \code{partial_mean}, where
#'   \code{partial_mean(z)} is \code{E[Z 1(Z <= z)]}.
#' @keywords internal
standard_error_distribution <- function(object) {

  par <- coef(object)

  switch(object$model,
    t = {
      df <- par[["df"]]
      k <- sqrt((df - 2) / df)
      list(cdf = function(z) stats::pt(z / k, df),
           logpdf = function(z) stats::dt(z / k, df, log = TRUE) - log(k),
           quantile = function(p) k * stats::qt(p, df),
           partial_mean = function(z) {
             u <- z / k
             -k * (df + u^2) / (df - 1) * stats::dt(u, df)
           })
    },
    skew_gaussian = ,
    skew_gaussian_leverage = {
      alpha <- par[["alpha"]]
      delta <- alpha / sqrt(1 + alpha^2)
      omega <- 1 / sqrt(1 - 2 * delta^2 / pi)
      xi <- -omega * delta * sqrt(2 / pi)
      # sn::psn is slow, so the cdf is interpolated from a fine table. E[V 1(V <= v)] for V ~ SN(0, 1, alpha) is
      # -2 dnorm(v) pnorm(alpha v) + sqrt(2 / pi) delta pnorm(v sqrt(1 + alpha^2)).
      zt <- sinh(seq(-asinh(1e3), asinh(1e3), length.out = 8001))
      cdf_table <- stats::approxfun(zt, sn::psn(zt, xi = xi, omega = omega, alpha = alpha), yleft = 0, yright = 1)
      list(cdf = cdf_table,
           logpdf = function(z) {
             v <- (z - xi) / omega
             log(2) + stats::dnorm(v, log = TRUE) - log(omega) + stats::pnorm(alpha * v, log.p = TRUE)
           },
           quantile = function(p) sn::qsn(p, xi = xi, omega = omega, alpha = alpha),
           partial_mean = function(z) {
             v <- (z - xi) / omega
             p_v <- cdf_table(z)
             m_v <- -2 * stats::dnorm(v) * stats::pnorm(alpha * v) + sqrt(2 / pi) * delta * stats::pnorm(v * sqrt(1 + alpha^2))
             xi * p_v + omega * m_v
           })
    },
    skew_t = ,
    skew_t_leverage = {
      df_left <- par[["df_left"]]
      df_right <- par[["df_right"]]
      # No closed form for the partial mean, so integrate z f(z) numerically on a fine grid. The grid reaches far
      # into the tails because they are polynomial.
      zt <- sinh(seq(-asinh(1e4), asinh(1e4), length.out = 20001))
      f <- zt * dskew_t(zt, df_left, df_right)
      pm <- c(0, cumsum(diff(zt) * (f[-1] + f[-length(f)]) / 2))
      list(cdf = function(z) pskew_t(z, df_left, df_right),
           logpdf = function(z) dskew_t(z, df_left, df_right, log = TRUE),
           quantile = function(p) qskew_t(p, df_left, df_right),
           partial_mean = stats::approxfun(zt, pm, yleft = 0, yright = 0))
    },
    # gaussian and leverage
    list(cdf = stats::pnorm,
         logpdf = function(z) stats::dnorm(z, log = TRUE),
         quantile = stats::qnorm,
         partial_mean = function(z) -stats::dnorm(z))
  )
}

#' Number of loaded DLLs that contain a TMB model
#' @return Integer.
#' @keywords internal
n_tmb_dlls <- function() {
  is_tmb <- function(dll) !inherits(try(getNativeSymbolInfo("MakeADFunObject", dll), silent = TRUE), "try-error")
  sum(vapply(getLoadedDLLs(), is_tmb, logical(1)))
}
