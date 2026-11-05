context("test-volplot")

test_that("Test volplot", {
  
  opt <- readRDS("test_objects/test_summary.rds")
  
  plt <- plot(opt)
  expect_is(plt, "ggplot")
  
  plt <- plot(opt, plot_log = FALSE)
  expect_true(all(plt$data$volatility > 0))
  
  expect_error(plot(opt, forecast = 0))
  
})

test_that("plot_returns", {

  opt <- readRDS("test_objects/test_summary.rds")

  plt <- plot_returns(opt)
  expect_is(plt, "ggplot")
  expect_true(all(plt$data$lower < 0 & plt$data$upper > 0))
  expect_equal(nrow(plt$data), opt$nobs)

  # A wider interval has fewer returns outside it
  expect_lt(mean(plot_returns(opt, level = 0.99)$data$outside), mean(plt$data$outside))

  expect_error(plot_returns(opt, level = 1.2))
  expect_warning(plot_returns(opt, dates = 1:10))

  # The interval is asymmetric for the skew gaussian models
  param <- list(phi = 0.9, sigma_h = 0.3, sigma_y = 0.2, alpha = 5, rho = -0.5)
  data <- sim_sv(param = param, nobs = 500, seed = 1, model = "skew_gaussian_leverage")
  opt_skew <- estimate_parameters(data$y, model = "skew_gaussian_leverage", silent = TRUE)
  bands <- plot_returns(opt_skew)$data
  expect_true(all(bands$upper > -bands$lower))

})

test_that("plot_diagnostics", {

  opt <- readRDS("test_objects/test_summary.rds")

  set.seed(1)
  plt <- plot_diagnostics(opt, res = c(stats::rnorm(50), NA), lag_max = 10)
  expect_is(plt, "ggplot")
  expect_error(plot_diagnostics(opt, res = c(1, NA)))

  # Calculate the residuals inside the function
  data <- sim_sv(param = list(phi = 0.9, sigma_h = 0.3, sigma_y = 0.2), nobs = 200, seed = 1)
  opt_small <- estimate_parameters(data$y, model = "gaussian", silent = TRUE)
  plt <- plot_diagnostics(opt_small, lag_max = 5)
  expect_is(plt, "ggplot")
  expect_equal(nrow(plt$layers[[2]]$data), opt_small$nobs)

})


test_that("grid residuals agree with TMB and are standard gaussian", {

  param <- list(phi = 0.9, sigma_h = 0.3, sigma_y = 0.2, df = 5, alpha = -3, rho = -0.7)

  for (model in c("gaussian", "t", "skew_gaussian", "leverage", "skew_gaussian_leverage")) {
    data <- sim_sv(param = param, nobs = 300, seed = 1, model = model)
    opt <- estimate_parameters(data$y, model = model, silent = TRUE)

    res <- residuals(opt)
    expect_length(res, opt$nobs)
    expect_true(all(is.finite(res)))
    expect_lt(abs(mean(res)), 0.2)
    expect_lt(abs(sd(res) - 1), 0.15)

    # Grid is exact; TMB uses the Laplace approximation, so allow a small difference. TMB sometimes returns NaN
    # when its numerical integration fails, so only compare the values it returns.
    invisible(utils::capture.output(res_tmb <- residuals(opt, conditional = 1:297, method = "tmb")))
    ok <- is.finite(res_tmb)
    expect_equal(tail(res, 3)[ok], res_tmb[ok], tolerance = 0.05)
  }

  expect_length(residuals(opt, conditional = 1:250), 50)

})
