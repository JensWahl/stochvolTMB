context("test-sim_sv")

test_that("Test sim_sv", {
# Data --------------------------------------------------------------------

  param <- list(phi = 0.9, 
                sigma_h = 0.4,
                sigma_y = 0.2,
                df = 5, 
                alpha = 5,
                rho = -0.7)
  nobs <- 1000
  models <- c("gaussian", "t", "leverage", "skew_gaussian")
  
  for (model in models) {
    
    data <- sim_sv(param = param, nobs = nobs, model = model)
    
    # Tests -------------------------------------------------------------------
    expect_equal(nrow(data), nobs)
    expect_equal(ncol(data), 2)
    expect_named(data, c("y", "h"))
    expect_type(data$y, "double")
    expect_type(data$h, "double")
    expect_equal(attributes(data)$param, param)
    expect_equal(attributes(data)$model, model)
    
  }
  
  expect_error(sim_sv(param = c(1, 2, 3)))
  expect_error(sim_sv(model = "tmp"))
  
  # Check error if parameters are outside their domain
  param$phi <- 1.1
  expect_error(sim_sv(param, model = "gaussian"))
  
  param$phi <- 0.9
  param$df <- 1.9
  expect_error(sim_sv(param, model = "t"))
  
  param$rho <- -1.2
  expect_error(sim_sv(param, model = "leverage"))
  
  
})

test_that("Test simulate_parameters", {
  

# Data ------------------------------------------------------------------------------------------------------------
  data(spy)
  mod <- estimate_parameters(spy$log_return) 
  sim <- simulate_parameters(mod, nsim = 1000)
  rep <-  summary(mod, report = "transformed")
  
  sim_mean <- as.vector(apply(sim, 2, mean))
  sim_sd <- as.vector(apply(sim, 2, sd))
  mod_est <- rep$estimate
  mod_sd <- rep$std_error
# Tests -----------------------------------------------------------------------------------------------------------

  
  expect_equal(ncol(sim), 3)
  expect_equal(nrow(sim), 1000)
  

  # Simulated values should be equal to estimated
  
  expect_equal(sim_mean, mod_est, tolerance = 1e-03)
  expect_equal(sim_sd, mod_sd, tolerance = 1e-03)
  expect_equal(colnames(sim), rep$parameter)
  
  
  
})