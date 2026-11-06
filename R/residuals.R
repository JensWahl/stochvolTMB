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

residuals <- function(object, conditional = NULL, method = c("grid", "tmb"), grid_size = 500, ...) {

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
#' Keeps the predictive density of \code{h_t} on a grid. For each observation it calculates the predictive
#' distribution function of \code{y_t}, updates the density with \code{y_t} and moves it one step forward.
#' Grid points with negligible weight are skipped, so the cost per step is small.
#'
#' @param object A \code{stochvolTMB} object.
#' @param grid_size Number of grid points.
#' @param width Half-width of the grid in stationary standard deviations of \code{h_t}.
#' @return Vector of one-step-ahead residuals for all observations.
#' @keywords internal
osa_grid <- function(object, grid_size = 500, width = 8) {

  parameter <- type <- estimate <- NULL

  report <- summary(object, report = "transformed")
  get_par <- function(name) report[parameter == name, estimate]

  model <- object$model
  y <- object$obj$env$data$y
  n <- length(y)
  sigma_y <- get_par("sigma_y")
  sigma_h <- get_par("sigma_h")
  phi <- get_par("phi")
  rho <- if (model %in% c("leverage", "skew_gaussian_leverage", "skew_t_leverage")) get_par("rho") else 0
  c_rho <- sqrt(1 - rho^2)

  sd0 <- sigma_h / sqrt(1 - phi^2)
  g <- seq(-width * sd0, width * sd0, length.out = grid_size)
  s <- sigma_y * exp(g / 2)

  # Conditional cdf and log density of y_t given h_t on grid points idx, for the models where y_t depends on h_t only
  if (model == "t") {
    df <- get_par("df")
    k <- s * sqrt((df - 2) / df)
    cdf <- function(yt, idx) stats::pt(yt / k[idx], df)
    logpdf <- function(yt, idx) stats::dt(yt / k[idx], df, log = TRUE) - log(k[idx])
  } else if (model %in% c("skew_t", "skew_t_leverage")) {
    df_left <- get_par("df_left")
    df_right <- get_par("df_right")
    cdf <- function(yt, idx) pskew_t(yt / s[idx], df_left, df_right)
    logpdf <- function(yt, idx) dskew_t(yt / s[idx], df_left, df_right, log = TRUE) - log(s[idx])
  } else if (model %in% c("skew_gaussian", "skew_gaussian_leverage")) {
    alpha <- get_par("alpha")
    delta <- alpha / sqrt(1 + alpha^2)
    omega <- s / sqrt(1 - 2 * delta^2 / pi)
    xi <- -omega * delta * sqrt(2 / pi)
    cdf <- function(yt, idx) sn::psn(yt, xi = xi[idx], omega = omega[idx], alpha = alpha)
    logpdf <- function(yt, idx) {
      u <- (yt - xi[idx]) / omega[idx]
      log(2) + stats::dnorm(u, log = TRUE) - log(omega[idx]) + stats::pnorm(alpha * u, log.p = TRUE)
    }
  } else {
    cdf <- function(yt, idx) stats::pnorm(yt / s[idx])
    logpdf <- function(yt, idx) stats::dnorm(yt, 0, s[idx], log = TRUE)
  }

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

  for (t in seq_len(n)) {

    idx <- which(w > 1e-14 * max(w))
    wt <- w[idx] / sum(w[idx])

    if (model == "leverage" && t < n) {
      # y_t depends on h_t and h_{t+1}, so work with their joint predictive density
      joint <- wt * P[idx, , drop = FALSE]
      eta <- outer(g[idx], g, function(h, h_next) (h_next - phi * h) / sigma_h)
      u <- (y[t] - s[idx] * rho * eta) / (s[idx] * c_rho)
      prob[t] <- sum(joint * stats::pnorm(u))
      w <- colSums(joint * stats::dnorm(u) / (s[idx] * c_rho))
    } else {
      # For the leverage model, y_n given h_n is gaussian once h_{n+1} is integrated out
      prob[t] <- sum(wt * cdf(y[t], idx))
      if (t == n) break
      logp <- logpdf(y[t], idx)
      post <- wt * exp(logp - max(logp))
      if (model %in% c("skew_gaussian_leverage", "skew_t_leverage")) {
        # The return shock drives the volatility shock, so the transition depends on y_t
        z <- y[t] / s[idx]
        w <- colSums(post * transition(phi * g[idx] + sigma_h * rho * z, sigma_h * c_rho))
      } else {
        w <- colSums(post * P[idx, , drop = FALSE])
      }
    }
    w <- w / sum(w)
  }

  stats::qnorm(pmin(pmax(prob, 1e-16), 1 - 1e-16))
}

#' Number of loaded DLLs that contain a TMB model
#' @return Integer.
#' @keywords internal
n_tmb_dlls <- function() {
  is_tmb <- function(dll) !inherits(try(getNativeSymbolInfo("MakeADFunObject", dll), silent = TRUE), "try-error")
  sum(vapply(getLoadedDLLs(), is_tmb, logical(1)))
}
