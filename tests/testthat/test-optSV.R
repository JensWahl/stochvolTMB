context("test-opt_sv")


test_that("Test estimate_parameters.R", {

  
  # Data --------------------------------------------------------------------
  
  results <- readRDS("test_objects/test_parameter_estimates.rds")
  models <- c("gaussian", "t", "leverage", "skew_gaussian")

# Tests -------------------------------------------------------------------
  opt <- list()
  for (model in models) {
    opt[[model]] <- estimate_parameters(data = results[[model]]$y, model = model, silent = TRUE)$fit$par
  }
  
  expect_known_value(opt, file = "test_objects/estimate_parameters.rds", tolerance = 1e-05)
  
  
  
})

test_that("Test summary.R", {

# Data --------------------------------------------------------------------

  opt <- readRDS("test_objects/test_summary.rds")
  srep <- summary(opt)
  

# Tests -------------------------------------------------------------------
  
  expect_is(srep, "data.table")
  expect_equal(srep[, unique(type)], c("transformed", "fixed", "random"))
  

# Data only random --------------------------------------------------------

  srep <- summary(opt, report = "random")
  expect_equal(srep[, unique(type)], c("random"))
  

# Data with wrong report  -------------------------------------------------

  expect_error(summary(opt, report = "123"))
  
  
})


test_that("Test predict.R", {
  

# Data ------------------------------------------------------------------------------------------------------------

  opt <- readRDS("test_objects/test_summary.rds")

  set.seed(123)
  steps <- 10; nsim <- 10000
  pred <- predict(opt, steps = steps, nsim = nsim, include_parameters = FALSE)

# Tests -----------------------------------------------------------------------------------------------------------

  expect_error(predict(opt, steps = 0))
  expect_named(pred, c("y", "h", "h_exp"))
  expect_equal(dim(pred$y), c(steps, nsim))
  expect_equal(dim(pred$h), c(steps, nsim))
  
  # Test summary(predict)
  srep <- summary(pred, quantiles = c(0.025, 0.975), predict_mean = TRUE)
  expect_equal(length(srep), 3)
  expect_equal(nrow(srep$y), steps)
  expect_equal(nrow(srep$h), steps)
  expect_equal(nrow(srep$h_exp), steps)
  expect_true(all(c("quantile_0.025", "quantile_0.975", "mean") %in% names(srep$y)))
  
  
  # Do not include mean
  srep <- summary(pred, quantiles = c(0.025, 0.975), predict_mean = FALSE)
  expect_true(!"mean" %in% names(srep$y))
  
  # Wrong quantiles
  expect_error(summary(pred, quantiles = c(0.025, 1.975)))
  expect_error(summary(pred, quantiles = NA))
  expect_error(summary(pred, quantiles = NULL))
  
  
})