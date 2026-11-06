#' Diagnostic plots based on one-step-ahead residuals
#'
#' Shows three panels that check if the fitted model describes the data:
#' \itemize{
#' \item A Q-Q plot of the one-step-ahead (OSA) residuals against the standard gaussian distribution. If the model
#'  is correct, the points follow the diagonal. Bent tails show that the error distribution is wrong, for example
#'  that the returns have heavier tails or more skewness than the model allows.
#' \item The autocorrelation function (ACF) of the residuals. Bars outside the dashed lines show dependence in the
#'  returns that the model does not capture.
#' \item The ACF of the squared residuals. Bars outside the dashed lines show volatility clustering that the model
#'  does not capture.
#' }
#'
#' By default, the residuals are calculated for all observations with the exact grid method of
#' \code{\link{residuals}}, which takes a few seconds. To reuse residuals, calculate them once with
#' \code{\link{residuals}} and give them in \code{res}.
#'
#' @param x A \code{stochvolTMB} object returned from \code{\link{estimate_parameters}}.
#' @param res Optional vector of OSA residuals from \code{\link{residuals}}. If \code{NULL}, the residuals are
#'   calculated.
#' @param lag_max Maximum lag in the ACF panels.
#' @param ... Arguments passed to \code{\link{residuals}} when \code{res} is \code{NULL}, for example
#'   \code{method} or \code{conditional}.
#' @return ggplot object with the three diagnostic panels.
#' @export
#' @examples
#' \donttest{
#' data(spy)
#' opt <- estimate_parameters(spy$log_return, model = "t", silent = TRUE)
#' plot_diagnostics(opt)
#' }
plot_diagnostics <- function(x, res = NULL, lag_max = 20, ...) {

  panel <- theoretical <- sample <- lag <- acf <- y <- intercept <- slope <- NULL

  if (!inherits(x, "stochvolTMB")) {
    stop("x has to be a stochvolTMB object")
  }

  if (is.null(res)) {
    res <- residuals(x, ...)
  }

  res <- res[is.finite(res)]
  n <- length(res)
  if (n < 3) {
    stop("Need at least 3 finite residuals")
  }

  panels <- c("Q-Q plot of residuals", "ACF of residuals", "ACF of squared residuals")

  qq <- data.table(panel = panels[1], theoretical = stats::qnorm(stats::ppoints(n)), sample = sort(res))

  lag_max <- min(lag_max, n - 1)
  acf_data <- function(z, name) {
    data.table(panel = name, lag = 1:lag_max,
               acf = as.numeric(stats::acf(z, lag.max = lag_max, plot = FALSE)$acf)[-1])
  }
  acfs <- rbindlist(list(acf_data(res, panels[2]), acf_data(res^2, panels[3])))

  # Approximate 95% limits for the ACF of white noise
  band <- stats::qnorm(0.975) / sqrt(n)
  bands <- data.table(panel = rep(panels[2:3], each = 2), y = c(-band, band, -band, band))
  zero <- data.table(panel = panels[2:3], y = 0)
  diagonal <- data.table(panel = panels[1], intercept = 0, slope = 1)

  qq[, panel := factor(panel, levels = panels)]
  acfs[, panel := factor(panel, levels = panels)]
  bands[, panel := factor(panel, levels = panels)]
  diagonal[, panel := factor(panel, levels = panels)]
  zero[, panel := factor(panel, levels = panels)]

  ggplot2::ggplot() +
    ggplot2::geom_abline(data = diagonal, ggplot2::aes(intercept = intercept, slope = slope), color = "grey40") +
    ggplot2::geom_point(data = qq, ggplot2::aes(theoretical, sample), size = 1) +
    ggplot2::geom_hline(data = zero, ggplot2::aes(yintercept = y), color = "grey40") +
    ggplot2::geom_segment(data = acfs, ggplot2::aes(x = lag, xend = lag, y = 0, yend = acf), linewidth = 0.8) +
    ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = y), linetype = 2, color = "steelblue") +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free") +
    ggplot2::labs(title = paste0("One-step-ahead residual diagnostics (", n, " residuals)"), x = "", y = "") +
    ggplot2::theme_bw()
}


#' Plot returns with bands from the fitted model
#'
#' Plots the observed returns together with the central \code{level} interval of the fitted conditional
#' distribution of each return, given the estimated log-volatility \code{h_t}. The interval uses the error
#' distribution of the model, so it is asymmetric for the skew gaussian models and wider for the t model.
#' Returns outside the interval are marked in red.
#'
#' The subtitle shows the share of returns outside the interval. For a good model it is close to
#' \code{1 - level}. The bands use the smoothed estimate of \code{h_t}, which also uses \code{y_t}. The share is
#' therefore an optimistic, in-sample check. Use \code{\link{plot_diagnostics}} for a check based on
#' one-step-ahead residuals.
#'
#' @param x A \code{stochvolTMB} object returned from \code{\link{estimate_parameters}}.
#' @param level Probability covered by the interval.
#' @param dates Vector of length \code{x$nobs}, providing optional dates for labeling the x-axis. The default value
#'   is \code{NULL}; in this case, the axis will be labeled with numbers.
#' @return ggplot object.
#' @export
#' @examples
#' \donttest{
#' data(spy)
#' opt <- estimate_parameters(spy$log_return, model = "skew_gaussian_leverage", silent = TRUE)
#' plot_returns(opt, dates = spy$date)
#' }
plot_returns <- function(x, level = 0.95, dates = NULL) {

  parameter <- estimate <- type <- time <- y <- lower <- upper <- outside <- NULL

  if (!inherits(x, "stochvolTMB")) {
    stop("x has to be a stochvolTMB object")
  }

  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` has to be a number between 0 and 1")
  }

  if (!is.null(dates) && length(dates) != x$nobs) {
    warning("dates is not same length as data and is ignored")
    dates <- NULL
  }

  report <- summary(x)
  get_par <- function(name) report[parameter == name & type == "transformed", estimate]

  # Quantiles of the standardized error (mean zero, unit variance) of each model
  p <- c((1 - level) / 2, (1 + level) / 2)
  q <- switch(x$model,
    t = {
      df <- get_par("df")
      stats::qt(p, df = df) * sqrt((df - 2) / df)
    },
    skew_t = ,
    skew_t_leverage = qskew_t(p, get_par("df_left"), get_par("df_right")),
    skew_gaussian = ,
    skew_gaussian_leverage = {
      alpha <- get_par("alpha")
      delta <- alpha / sqrt(1 + alpha^2)
      omega <- 1 / sqrt(1 - 2 * delta^2 / pi)
      sn::qsn(p, xi = -omega * delta * sqrt(2 / pi), omega = omega, alpha = alpha)
    },
    # gaussian and leverage: given h_t only, the return is gaussian
    stats::qnorm(p)
  )

  scale <- get_par("sigma_y") * exp(report[type == "random", estimate] / 2)
  dt <- data.table(time = if (is.null(dates)) seq_len(x$nobs) else dates,
                   y = x$obj$env$data$y,
                   lower = scale * q[1],
                   upper = scale * q[2])
  dt[, outside := y < lower | y > upper]

  subtitle <- sprintf("%.1f%% of returns outside the %g%% interval (expected %g%%)",
                      100 * dt[, mean(outside)], 100 * level, 100 * (1 - level))

  ggplot2::ggplot(dt, ggplot2::aes(time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = y), linewidth = 0.2, color = "grey30") +
    ggplot2::geom_point(data = dt[outside == TRUE], ggplot2::aes(y = y), color = "firebrick", size = 0.6) +
    ggplot2::labs(title = paste0("Returns with ", 100 * level, "% interval from the ", x$model, " model"),
                  subtitle = subtitle, x = "", y = "") +
    ggplot2::theme_bw()
}
