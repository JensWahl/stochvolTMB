#' One-step-ahead Value at Risk and Expected Shortfall
#'
#' Calculates the one-day Value at Risk (VaR) and Expected Shortfall (ES) of the returns from the predictive
#' distribution of \code{y_t} given \code{y_1, ..., y_{t-1}}, for every observation and for the next day after the
#' data.
#'
#' The predictive distribution is calculated exactly with the same filter on a grid for the log-volatility as
#' \code{\link{residuals.stochvolTMB}}. It is a mixture over \code{h_t} of the conditional distribution of the
#' returns. For a level \eqn{\alpha}, for example 0.99, let \eqn{q_t} be the \eqn{1 - \alpha} quantile of the predictive
#' distribution. Then
#' \deqn{VaR_t = -q_t, \qquad ES_t = -E[y_t \mid y_t \le q_t, y_1, \ldots, y_{t-1}].}
#' Both are reported as positive numbers when they are losses, on the scale of the returns. A return is an exceedance
#' if \eqn{y_t < -VaR_t}.
#'
#' The parameters are fixed at their estimates from the full data, and the uncertainty of the parameters is not
#' included. The values for the observed data are therefore in-sample, and a backtest with
#' \code{\link{backtest_var}} on them is slightly optimistic. The last row (\code{time = nobs + 1}) is the forecast
#' for the next day. For more than one day, use \code{\link{forecast_var_es}}.
#'
#' @param object A \code{stochvolTMB} object returned from \code{\link{estimate_parameters}}.
#' @param level Vector of confidence levels between 0.5 and 1.
#' @param grid_size Number of grid points for the log-volatility.
#' @return A \code{data.table} with one row for each time point and level, and the columns \code{time} (1 to
#'   \code{nobs + 1}), \code{level}, \code{y} (the observed return, \code{NA} for the forecast), \code{var}, \code{es}
#'   and \code{exceedance} (\code{y < -var}).
#' @seealso \code{\link{backtest_var}}, \code{\link{forecast_var_es}}
#' @export
#' @examples
#' \donttest{
#' opt <- estimate_parameters(spy$log_return, model = "skew_t_leverage", silent = TRUE)
#' risk <- var_es(opt, level = c(0.95, 0.99))
#' # Forecast for the next day
#' risk[is.na(y)]
#' backtest_var(risk)
#' }
var_es <- function(object, level = c(0.95, 0.99), grid_size = 500) {

  if (!inherits(object, "stochvolTMB")) {
    stop("`object` has to be of class `stochvolTMB`")
  }
  check_level(level)

  filter <- grid_filter(object, grid_size = grid_size)
  std <- standard_error_distribution(object)
  s <- filter$scale
  y <- c(object$obj$env$data$y, NA)
  n_time <- length(y)

  # Quantile and partial mean of the mixture sum_i w_i * s_i * Z at probability p
  mixture_risk <- function(weights, p, z_p) {
    sc <- s[weights$idx]
    w <- weights$w
    if (diff(range(sc)) < 1e-12 * max(sc)) {
      q <- sc[1] * z_p
    } else {
      # Each component has its p-quantile at s_i * z_p, so the mixture quantile lies between the extremes
      q <- stats::uniroot(function(q) sum(w * std$cdf(q / sc)) - p,
                          lower = max(sc) * z_p, upper = min(sc) * z_p,
                          extendInt = "yes", tol = 1e-10 * max(sc))$root
    }
    c(var = -q, es = -sum(w * sc * std$partial_mean(q / sc)) / p)
  }

  res <- lapply(level, function(lev) {
    p <- 1 - lev
    z_p <- std$quantile(p)
    out <- vapply(filter$weights, mixture_risk, numeric(2), p = p, z_p = z_p)
    data.table(time = seq_len(n_time), level = lev, y = y, var = out["var", ], es = out["es", ])
  })

  res <- rbindlist(res)
  res[, exceedance := y < -var]
  res[]
}


#' Value at Risk and Expected Shortfall for several days ahead
#'
#' Simulates future returns with \code{\link{predict.stochvolTMB}} and calculates the Value at Risk (VaR) and
#' Expected Shortfall (ES) for each day ahead, both of the return on that day and of the cumulative return
#' (the sum of the log-returns) up to that day. The cumulative values for \code{step = 10} are, for example, the
#' 10-day VaR and ES.
#'
#' VaR and ES are reported as positive numbers when they are losses: if \eqn{q} is the \eqn{1 - \alpha} quantile of the
#' simulated returns, then \eqn{VaR = -q} and \eqn{ES = -E[y \mid y \le q]}. By default, the simulation includes the
#' uncertainty of the parameters. The Monte Carlo error decreases with \code{nsim}. For one day ahead,
#' \code{\link{var_es}} is exact and faster.
#'
#' @param object A \code{stochvolTMB} object returned from \code{\link{estimate_parameters}}.
#' @param steps Number of days ahead.
#' @param level Vector of confidence levels between 0.5 and 1.
#' @param nsim Number of simulated paths.
#' @param include_parameters Logical. If \code{TRUE}, the parameters are drawn from their asymptotic distribution
#'   for each path. See \code{\link{predict.stochvolTMB}}.
#' @return A \code{data.table} with the columns \code{step}, \code{level}, \code{var}, \code{es},
#'   \code{var_cumulative} and \code{es_cumulative}.
#' @seealso \code{\link{var_es}}
#' @export
#' @examples
#' \donttest{
#' opt <- estimate_parameters(spy$log_return, model = "skew_t_leverage", silent = TRUE)
#' set.seed(1)
#' forecast_var_es(opt, steps = 10, level = 0.99)
#' }
forecast_var_es <- function(object, steps = 10L, level = c(0.95, 0.99), nsim = 100000L,
                            include_parameters = TRUE) {

  if (!inherits(object, "stochvolTMB")) {
    stop("`object` has to be of class `stochvolTMB`")
  }
  check_level(level)

  pred <- stats::predict(object, steps = steps, nsim = nsim, include_parameters = include_parameters)
  y <- pred$y
  y_cum <- apply(y, 2, cumsum)
  if (steps == 1) y_cum <- matrix(y_cum, nrow = 1)

  risk <- function(x, p) {
    q <- stats::quantile(x, p, names = FALSE)
    c(-q, -mean(x[x <= q]))
  }

  res <- lapply(level, function(lev) {
    p <- 1 - lev
    daily <- apply(y, 1, risk, p = p)
    cumulative <- apply(y_cum, 1, risk, p = p)
    data.table(step = seq_len(steps), level = lev,
               var = daily[1, ], es = daily[2, ],
               var_cumulative = cumulative[1, ], es_cumulative = cumulative[2, ])
  })

  rbindlist(res)
}


#' Backtest Value at Risk and Expected Shortfall
#'
#' Tests if the exceedances of the Value at Risk (returns below \code{-var}) have the expected frequency and are
#' independent over time.
#'
#' Let \eqn{I_t = 1} if \eqn{y_t < -VaR_t} and \eqn{p = 1 - \alpha} the expected exceedance rate.
#' \itemize{
#' \item \strong{Unconditional coverage} (Kupiec 1995): a likelihood ratio test of \eqn{P(I_t = 1) = p}. A small
#'   p-value means too many or too few exceedances.
#' \item \strong{Independence} (Christoffersen 1998): a likelihood ratio test that \eqn{I_t} is a Markov chain with
#'   \eqn{P(I_t = 1 \mid I_{t-1} = 0) = P(I_t = 1 \mid I_{t-1} = 1)}. A small p-value means that exceedances come in
#'   clusters, which happens when the model reacts too slowly to changes in volatility.
#' \item \strong{Conditional coverage}: the sum of the two statistics, with two degrees of freedom.
#' }
#' \code{es_ratio} is the mean of \eqn{-y_t / ES_t} over the exceedances. It is close to one if the Expected
#' Shortfall is correct, and larger than one if the model underestimates the size of the losses in the tail.
#'
#' @param x A \code{data.table} returned from \code{\link{var_es}}. Rows without an observed return are ignored.
#' @return A \code{data.table} with one row for each level and the columns \code{level}, \code{n},
#'   \code{expected} and \code{observed} (number of exceedances), \code{rate} (observed rate), \code{lr_uc},
#'   \code{p_uc}, \code{lr_ind}, \code{p_ind}, \code{lr_cc}, \code{p_cc} and \code{es_ratio}.
#' @references Kupiec, P. H. (1995). Techniques for verifying the accuracy of risk measurement models. The Journal of
#'   Derivatives, 3(2), 73-84. \doi{10.3905/jod.1995.407942}
#'
#'   Christoffersen, P. F. (1998). Evaluating interval forecasts. International Economic Review, 39(4), 841-862.
#'   \doi{10.2307/2527341}
#' @seealso \code{\link{var_es}}
#' @export
backtest_var <- function(x) {

  if (!is.data.table(x) || !all(c("level", "y", "var", "es") %in% names(x))) {
    stop("`x` has to be a data.table returned from var_es()")
  }

  # 0 * log(0) is 0 in the likelihoods below
  xlogy <- function(a, b) ifelse(a == 0, 0, a * log(b))

  res <- lapply(split(x[!is.na(y)], by = "level"), function(d) {
    hit <- as.integer(d$y < -d$var)
    n <- length(hit)
    n1 <- sum(hit)
    p <- 1 - d$level[1]
    rate <- n1 / n

    lr_uc <- -2 * (xlogy(n - n1, 1 - p) + xlogy(n1, p) - xlogy(n - n1, 1 - rate) - xlogy(n1, rate))

    prev <- hit[-n]
    curr <- hit[-1]
    n00 <- sum(prev == 0 & curr == 0)
    n01 <- sum(prev == 0 & curr == 1)
    n10 <- sum(prev == 1 & curr == 0)
    n11 <- sum(prev == 1 & curr == 1)
    pi01 <- n01 / max(n00 + n01, 1)
    pi11 <- n11 / max(n10 + n11, 1)
    pi1 <- (n01 + n11) / (n - 1)
    lr_ind <- -2 * (xlogy(n00 + n10, 1 - pi1) + xlogy(n01 + n11, pi1) -
                      xlogy(n00, 1 - pi01) - xlogy(n01, pi01) - xlogy(n10, 1 - pi11) - xlogy(n11, pi11))
    lr_cc <- lr_uc + lr_ind

    data.table(level = d$level[1], n = n, expected = n * p, observed = n1, rate = rate,
               lr_uc = lr_uc, p_uc = stats::pchisq(lr_uc, 1, lower.tail = FALSE),
               lr_ind = lr_ind, p_ind = stats::pchisq(lr_ind, 1, lower.tail = FALSE),
               lr_cc = lr_cc, p_cc = stats::pchisq(lr_cc, 2, lower.tail = FALSE),
               es_ratio = if (n1 > 0) mean(-d$y[hit == 1] / d$es[hit == 1]) else NA_real_)
  })

  rbindlist(res)
}


check_level <- function(level) {
  if (!is.numeric(level) || length(level) < 1 || anyNA(level) || any(level <= 0.5 | level >= 1)) {
    stop("`level` has to be a vector of numbers between 0.5 and 1")
  }
}


#' Plot returns with the one-step-ahead Value at Risk
#'
#' Plots the returns with \code{-VaR} and \code{-ES} from \code{\link{var_es}} and marks the exceedances, the
#' returns below \code{-VaR}.
#'
#' @param x A \code{data.table} returned from \code{\link{var_es}}.
#' @param level The level to plot. The default is the largest level in \code{x}.
#' @param dates Optional vector of dates with one element for each observed return, used for the x-axis.
#' @return ggplot object.
#' @seealso \code{\link{var_es}}, \code{\link{backtest_var}}
#' @export
#' @examples
#' \donttest{
#' opt <- estimate_parameters(spy$log_return, model = "skew_t_leverage", silent = TRUE)
#' plot_var(var_es(opt, level = 0.99), dates = spy$date)
#' }
plot_var <- function(x, level = NULL, dates = NULL) {

  time <- es <- NULL

  if (!is.data.table(x) || !all(c("time", "level", "y", "var", "es") %in% names(x))) {
    stop("`x` has to be a data.table returned from var_es()")
  }
  if (is.null(level)) level <- max(x$level)
  if (length(level) != 1 || !(level %in% x$level)) {
    stop("`level` has to be one of the levels in `x`")
  }

  # `level` is also a column of x, so find the rows outside of data.table scoping
  rows <- which(x[["level"]] == level & !is.na(x[["y"]]))
  d <- x[rows]
  if (!is.null(dates)) {
    if (length(dates) != nrow(d)) {
      warning("dates is not same length as data and is ignored")
    } else {
      d[, time := dates]
    }
  }
  d[, exceedance := y < -var]

  subtitle <- sprintf("%d exceedances of the %g%% VaR (%.2f%%, expected %g%%)",
                      sum(d$exceedance), 100 * level, 100 * mean(d$exceedance), 100 * (1 - level))

  ggplot2::ggplot(d, ggplot2::aes(time)) +
    ggplot2::geom_line(ggplot2::aes(y = y), linewidth = 0.2, color = "grey50") +
    ggplot2::geom_line(ggplot2::aes(y = -var, color = "VaR"), linewidth = 0.4) +
    ggplot2::geom_line(ggplot2::aes(y = -es, color = "ES"), linewidth = 0.4) +
    ggplot2::geom_point(data = d[d$exceedance], ggplot2::aes(y = y), color = "firebrick", size = 0.8) +
    ggplot2::scale_color_manual(values = c(VaR = "steelblue", ES = "darkorange"),
                                labels = c(VaR = "-VaR", ES = "-ES"), breaks = c("VaR", "ES"), name = NULL) +
    ggplot2::labs(title = paste0("Returns with one-step-ahead ", 100 * level, "% VaR and ES"),
                  subtitle = subtitle, x = "", y = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")
}
