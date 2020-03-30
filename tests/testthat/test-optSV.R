context("test-opt_sv")

## Example data -------

results <- readRDS("test_objects/test_parameter_estimates.rds")
seed <- results$seed
param <- results$param
N <- results$N



test_that("Test estimate_parameters.R", {


  models <- c("gaussian", "t", "leverage", "skew_gaussian")
  
  for(model in models){
    opt <- estimate_parameters(data = results[[model]]$y, model = model)
    expect_equal(opt$fit$par, results[[model]]$par, tolerance = 1e-05)
    expect_equal(opt$fit$objective, results[[model]]$objective, tolerance = 1e-05)
  }
  
})


