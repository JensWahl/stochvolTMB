context("test-volplot")

test_that("Test volplot", {
  
  opt <- readRDS("test_objects/test_summary.rds")
  
  plt <- plot(opt)
  expect_is(plt, "ggplot")
  
  plt <- plot(opt, plot_log = FALSE)
  expect_true(all(plt$data$volatility > 0))
  
  expect_error(plot(opt, forecast = 0))
  
})