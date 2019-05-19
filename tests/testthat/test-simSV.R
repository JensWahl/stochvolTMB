context("test-simSV")

  ## Example data -------
  # Gaussian case
  param <- list(phi = 0.9, 
                sigma_h = 0.4,
                sigma_y = 0.2,
                df = 5, 
                alpha = 5,
                rho = -0.7)
  N <- 1000
  data <- simSV(param = param, N = N, method = "gaussian")
  
  test_that("data dimensions correct", {
    expect_equal(nrow(data), N)
    expect_equal(ncol(data), 2)
  })
  
  test_that("names correct", {
    expect_named(data, c("y", "h"))
  })
  
  test_that("data types correct", {
    expect_type(data$y, "double")
    expect_type(data$h, "double")
    
  })
