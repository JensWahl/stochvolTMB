#' Skew t distribution of Jones and Faddy
#'
#' Density, distribution function, quantile function and random generation for the skew t distribution of
#' Jones and Faddy (2003), standardized to mean zero and unit variance.
#'
#' Before standardization, the density with parameters \eqn{a, b > 0} is
#' \deqn{f(t) = C \left(1 + \frac{t}{\sqrt{a + b + t^2}}\right)^{a + 1/2}
#'   \left(1 - \frac{t}{\sqrt{a + b + t^2}}\right)^{b + 1/2},
#'   \qquad C = \frac{1}{2^{a + b - 1} B(a, b) \sqrt{a + b}}.}
#' The left tail decays like a Student t with \eqn{2a} degrees of freedom and the right tail like a Student t with
#' \eqn{2b} degrees of freedom. The functions use \code{df_left = 2a} and \code{df_right = 2b}. If they are equal,
#' the distribution is the Student t distribution scaled to unit variance. If \code{df_left < df_right}, the left
#' tail is heavier and the distribution is skewed to the left. Both must be greater than 2 for the variance to
#' exist.
#'
#' The density is smooth everywhere. This is important for the Laplace approximation, which uses the second
#' derivative of the log density.
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Number of draws.
#' @param df_left,df_right Degrees of freedom of the left and the right tail, greater than 2.
#' @param log Logical; if \code{TRUE}, the log density is returned.
#' @return Numeric vector.
#' @references Jones, M. C. and Faddy, M. J. (2003). A skew extension of the t-distribution, with applications.
#'   Journal of the Royal Statistical Society: Series B, 65(1), 159-174. \doi{10.1111/1467-9868.00378}
#' @name skew_t
#' @keywords internal
NULL

# Mean and standard deviation of the unstandardized distribution (Jones and Faddy 2003, section 2)
skew_t_moments <- function(df_left, df_right) {
  a <- df_left / 2
  b <- df_right / 2
  mu <- (a - b) * sqrt(a + b) / 2 * exp(lgamma(a - 0.5) + lgamma(b - 0.5) - lgamma(a) - lgamma(b))
  m2 <- (a + b) / 4 * ((a - b)^2 + a + b - 2) / ((a - 1) * (b - 1))
  list(a = a, b = b, mu = mu, sigma = sqrt(m2 - mu^2))
}

#' @rdname skew_t
dskew_t <- function(x, df_left, df_right, log = FALSE) {
  m <- skew_t_moments(df_left, df_right)
  t <- m$mu + m$sigma * x
  r <- t / sqrt(m$a + m$b + t^2)
  log_c <- -(m$a + m$b - 1) * log(2) - lbeta(m$a, m$b) - 0.5 * log(m$a + m$b)
  d <- log_c + (m$a + 0.5) * log1p(r) + (m$b + 0.5) * log1p(-r) + log(m$sigma)
  if (log) d else exp(d)
}

#' @rdname skew_t
pskew_t <- function(q, df_left, df_right) {
  m <- skew_t_moments(df_left, df_right)
  t <- m$mu + m$sigma * q
  stats::pbeta((1 + t / sqrt(m$a + m$b + t^2)) / 2, m$a, m$b)
}

#' @rdname skew_t
qskew_t <- function(p, df_left, df_right) {
  m <- skew_t_moments(df_left, df_right)
  u <- stats::qbeta(p, m$a, m$b)
  t <- sqrt(m$a + m$b) * (2 * u - 1) / (2 * sqrt(u * (1 - u)))
  (t - m$mu) / m$sigma
}

#' @rdname skew_t
rskew_t <- function(n, df_left, df_right) {
  m <- skew_t_moments(df_left, df_right)
  u <- stats::rbeta(n, m$a, m$b)
  t <- sqrt(m$a + m$b) * (2 * u - 1) / (2 * sqrt(u * (1 - u)))
  (t - m$mu) / m$sigma
}
