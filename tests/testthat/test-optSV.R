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
  expect_error(predict(opt, steps = 0))

  

# Tests -----------------------------------------------------------------------------------------------------------
  set.seed(123)
  steps <- 10; nsim <- 10000
  pred <- predict(opt, steps = steps, nsim = nsim, include_parameters = FALSE)
  expect_named(pred, c("y", "h", "h_exp"))
  expect_equal(dim(pred$y), c(steps, nsim))
  expect_equal(dim(pred$h), c(steps, nsim))
  
})