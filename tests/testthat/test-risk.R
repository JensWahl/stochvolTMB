context("test-risk")

models <- c("gaussian", "t", "leverage", "skew_gaussian", "skew_gaussian_leverage", "skew_t", "skew_t_leverage")
param <- list(phi = 0.95, sigma_h = 0.25, sigma_y = 0.01, df = 5, alpha = -3, rho = -0.7, df_left = 4,
              df_right = 10)

fit_model <- function(model, nobs = 600) {
  data <- sim_sv(param = param, nobs = nobs, seed = 1, model = model)
  estimate_parameters(data$y, model = model, silent = TRUE)
}

test_that("standardized error distributions are consistent", {

  for (model in models) {
    opt <- fit_model(model, nobs = 300)
    std <- standard_error_distribution(opt)
    dens <- function(z) exp(std$logpdf(z))

    # Mean zero and unit variance
    expect_equal(stats::integrate(function(z) z * dens(z), -Inf, Inf)$value, 0, tolerance = 1e-4)
    expect_equal(stats::integrate(function(z) z^2 * dens(z), -Inf, Inf)$value, 1, tolerance = 1e-3)

    for (p in c(0.01, 0.05, 0.3)) {
      q <- std$quantile(p)
      expect_equal(std$cdf(q), p, tolerance = 1e-4)
      partial <- stats::integrate(function(z) z * dens(z), -Inf, q)$value
      expect_equal(std$partial_mean(q), partial, tolerance = 1e-3)
    }
  }

})

test_that("var_es is consistent with the one-step-ahead residuals", {
  skip_on_cran()

  for (model in models) {
    opt <- fit_model(model)
    risk <- var_es(opt, level = c(0.95, 0.99))

    expect_equal(nrow(risk), 2 * (opt$nobs + 1))
    expect_true(all(risk$var > 0))
    expect_true(all(risk$es > risk$var))
    expect_true(all(risk[level == 0.99, var] > risk[level == 0.95, var]))
    expect_true(is.na(risk[time == opt$nobs + 1, y][1]))

    # y_t < -VaR_t exactly when the predictive distribution function at y_t is below 1 - level
    u <- stats::pnorm(residuals(opt))
    for (lev in c(0.95, 0.99)) {
      hit <- risk[level == lev & !is.na(y), exceedance]
      close <- abs(u - (1 - lev)) < 1e-4
      expect_equal(hit[!close], (u < 1 - lev)[!close])
    }
  }

  expect_error(var_es(opt, level = 0.3))
  expect_error(var_es(opt, level = 1))
  expect_error(var_es(list(), level = 0.99))

})

test_that("forecast_var_es agrees with var_es for one day ahead", {

  for (model in c("gaussian", "leverage", "skew_t_leverage")) {
    opt <- fit_model(model)
    exact <- var_es(opt, level = 0.99)[is.na(y)]
    set.seed(1)
    sim <- forecast_var_es(opt, steps = 5, level = c(0.95, 0.99), nsim = 50000, include_parameters = FALSE)

    expect_equal(nrow(sim), 10)
    expect_equal(sim[step == 1 & level == 0.99, var], exact$var, tolerance = 0.06)
    expect_equal(sim[step == 1 & level == 0.99, es], exact$es, tolerance = 0.06)
    expect_equal(sim[step == 1, var], sim[step == 1, var_cumulative])
    expect_true(all(diff(sim[level == 0.99, var_cumulative]) > 0))
  }

  sim <- forecast_var_es(opt, steps = 1, level = 0.99, nsim = 1000)
  expect_equal(nrow(sim), 1)

})

test_that("predict for the leverage model uses the last return", {

  opt <- fit_model("leverage")
  y <- opt$obj$env$data$y
  pred <- predict(opt, steps = 3, nsim = 100, include_parameters = FALSE)
  expect_equal(dim(pred$y), c(3, 100))

  # A large negative last return increases the next volatility when rho < 0. The data lives in an environment, so
  # change it in place and restore it afterwards.
  env <- opt$obj$env
  n <- length(y)
  env$data$y[n] <- -0.1
  set.seed(1)
  h_down <- predict(opt, steps = 1, nsim = 5000, include_parameters = FALSE)$h
  env$data$y[n] <- 0.1
  set.seed(1)
  h_up <- predict(opt, steps = 1, nsim = 5000, include_parameters = FALSE)$h
  env$data$y[n] <- y[n]
  expect_gt(mean(h_down), mean(h_up))

})

test_that("backtest_var", {

  # One exceedance in 100 days at the 99% level: the observed rate equals the expected rate
  d <- data.table(level = 0.99, y = c(-2, rep(0, 99)), var = 1, es = 1.5)
  bt <- backtest_var(d)
  expect_equal(bt$observed, 1)
  expect_equal(bt$lr_uc, 0)
  expect_equal(bt$es_ratio, 2 / 1.5)

  # Ten exceedances in a row at the 95% level: too many and clustered
  d <- data.table(level = 0.95, y = c(rep(0, 90), rep(-2, 10), rep(0, 100)), var = 1, es = 1.5)
  bt <- backtest_var(d)
  expect_lt(bt$p_ind, 0.001)
  expect_equal(bt$lr_cc, bt$lr_uc + bt$lr_ind)

  # The same exceedances spread out are independent
  d <- data.table(level = 0.95, y = rep(c(-2, rep(0, 19)), 10), var = 1, es = 1.5)
  expect_gt(backtest_var(d)$p_ind, 0.1)

  # No exceedances
  d <- data.table(level = 0.99, y = rep(0, 50), var = 1, es = 1.5)
  bt <- backtest_var(d)
  expect_equal(bt$observed, 0)
  expect_true(is.finite(bt$lr_uc))
  expect_true(is.na(bt$es_ratio))

  # Rows for the forecast are ignored
  opt <- fit_model("gaussian", nobs = 300)
  bt <- backtest_var(var_es(opt, level = c(0.95, 0.99)))
  expect_equal(bt$n, c(300, 300))
  expect_equal(bt$level, c(0.95, 0.99))

  expect_error(backtest_var(data.frame(y = 1)))

})

test_that("plot_var", {

  opt <- fit_model("gaussian", nobs = 300)
  risk <- var_es(opt, level = c(0.95, 0.99))
  expect_is(plot_var(risk), "ggplot")
  expect_is(plot_var(risk, level = 0.95, dates = seq_len(300)), "ggplot")
  expect_warning(plot_var(risk, dates = 1:10))
  expect_error(plot_var(risk, level = 0.9))

})

test_that("compare_models", {

  data <- sim_sv(param = param, nobs = 500, seed = 1, model = "t")
  cm <- compare_models(data$y, models = c("gaussian", "t", "not_a_model"))

  expect_equal(nrow(cm), 3)
  expect_true(all(diff(cm$aic[!is.na(cm$aic)]) >= 0))
  expect_equal(cm$delta_aic[1], 0)
  expect_equal(cm$model[3], "not_a_model")
  expect_false(cm$converged[3])
  expect_match(cm$message[3], "not implemented")
  fits <- attr(cm, "fits")
  expect_equal(names(fits), cm$model)
  expect_equal(cm[model == "t", aic], AIC(fits$t))

})
