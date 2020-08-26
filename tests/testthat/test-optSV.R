context("test-opt_sv")


test_that("Test estimate_parameters.R", {

  
  # Data --------------------------------------------------------------------
  
  results <- readRDS("test_objects/test_parameter_estimates.rds")
  models <- c("gaussian", "t", "leverage", "skew_gaussian")

# Tests -------------------------------------------------------------------
  opt = list()
  for(model in models){
    opt[[model]] <- estimate_parameters(data = results[[model]]$y, model = model, silent = TRUE)
    expect_s3_class(opt[[model]], "stochvolTMB")
  }
  
  expect_known_value(opt, file = "test_objects/estimate_parameters.rds")
  
  
  
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




