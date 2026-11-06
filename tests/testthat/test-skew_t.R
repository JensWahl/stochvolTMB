context("test-skew_t")

test_that("skew t distribution is standardized and consistent", {

  for (tails in list(c(3, 15), c(6, 6), c(20, 4))) {
    dl <- tails[1]; dr <- tails[2]
    f <- function(x) dskew_t(x, dl, dr)
    integral <- function(g) {
      integrate(g, -Inf, 0, rel.tol = 1e-9)$value + integrate(g, 0, Inf, rel.tol = 1e-9)$value
    }

    expect_equal(integral(f), 1, tolerance = 1e-7)
    expect_equal(integral(function(x) x * f(x)), 0, tolerance = 1e-7)
    expect_equal(integral(function(x) x^2 * f(x)), 1, tolerance = 1e-6)

    q <- c(-3, -0.5, 0.4, 2.5)
    expect_equal(pskew_t(q, dl, dr), sapply(q, function(x) integrate(f, -Inf, x, rel.tol = 1e-10)$value),
                 tolerance = 1e-8)

    p <- c(0.001, 0.25, 0.5, 0.9, 0.999)
    expect_equal(pskew_t(qskew_t(p, dl, dr), dl, dr), p, tolerance = 1e-10)
  }

  # Equal tails give the Student t distribution scaled to unit variance
  x <- c(-2, 0.3, 4)
  expect_equal(dskew_t(x, 6, 6), stats::dt(x * sqrt(6 / 4), 6) * sqrt(6 / 4))

  # A heavier left tail gives negative skewness
  set.seed(1)
  z <- rskew_t(1e5, 4, 20)
  expect_lt(mean(z^3), 0)
  expect_equal(mean(z < -3), pskew_t(-3, 4, 20), tolerance = 0.1)

})

test_that("skew_t model recovers the tail parameters", {

  param <- list(phi = 0.97, sigma_h = 0.2, sigma_y = 0.01, df_left = 6, df_right = 10)
  data <- sim_sv(param = param, nobs = 1500, seed = 1, model = "skew_t")
  opt <- estimate_parameters(data$y, model = "skew_t", silent = TRUE)
  est <- summary(opt, report = "transformed")

  expect_equal(opt$fit$convergence, 0)
  expect_true(opt$rep$pdHess)
  expect_lt(est[parameter == "df_left", estimate], est[parameter == "df_right", estimate])

  pred <- predict(opt, steps = 3, nsim = 500)
  expect_equal(dim(pred$y), c(3, 500))
  expect_true(all(is.finite(pred$y)))

  sim_par <- simulate_parameters(opt, nsim = 10)
  expect_true(all(c("df_left", "df_right") %in% colnames(sim_par)))
  expect_true(all(sim_par[, c("df_left", "df_right")] > 2))

})


test_that("skew_t_leverage model recovers tails and leverage", {

  param <- list(phi = 0.95, sigma_h = 0.25, sigma_y = 0.2, df_left = 4, df_right = 10, rho = -0.7)
  data <- sim_sv(param = param, nobs = 1500, seed = 1, model = "skew_t_leverage")
  opt <- estimate_parameters(data$y, model = "skew_t_leverage", silent = TRUE)
  est <- summary(opt, report = "transformed")

  expect_equal(opt$fit$convergence, 0)
  expect_true(opt$rep$pdHess)
  expect_lt(abs(est[parameter == "rho", estimate] - param$rho), 0.15)
  expect_lt(est[parameter == "df_left", estimate], est[parameter == "df_right", estimate])

  pred <- predict(opt, steps = 3, nsim = 500)
  expect_equal(dim(pred$y), c(3, 500))
  expect_true(all(is.finite(pred$y)))

  res <- residuals(opt)
  expect_length(res, opt$nobs)
  expect_lt(abs(sd(res) - 1), 0.1)

})
