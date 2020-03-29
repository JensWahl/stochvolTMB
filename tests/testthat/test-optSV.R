context("test-opt_sv")

## Example data -------
# Gaussian case
results <- readRDS("test_objects/test_parameter_estimates.rds")
seed <- results$seed
param <- results$param
N <- results$N



test_that("Test estimate_parameters.R", {

  # gaussian 
  models <- c("gaussian", "t", "leverage", "skew_gaussian")
  
  for(model in models){
    opt <- estimate_parameters(data = l[[model]]$y, model = model)
    expect_equal(opt$fit$par, results[[model]]$par)
    expect_equal(opt$fit$objective, results[[model]]$objective)
  }
  
})


