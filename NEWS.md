# stochvolTMB (development version)

* New model `skew_gaussian_leverage` that combines skew-Gaussian returns with the leverage effect. The skew-Gaussian
  return shock drives the volatility shock, `eta_t = rho * z_t + sqrt(1 - rho^2) * e_t`. This order keeps the Laplace
  approximation accurate. The other order (skew noise added to `rho * eta_t`) made the approximation overestimate the
  likelihood, and the fit diverged to `rho = -1` and `|alpha| -> Inf`.
* Breaking: the skew-Gaussian models now estimate the skewness instead of the shape `alpha`. The fixed parameter
  `alpha` is replaced by `logit_skewness`, and the skewness is bounded by `|skewness| < 0.99` (`|alpha| <= 27`).
  `summary(report = "transformed")` reports both `skewness` and `alpha`. Before, the optimizer started at `alpha = 0`,
  where the likelihood is flat (the score is exactly zero), and the fit often stayed at `alpha = 0`.
* The skew-Gaussian density now uses a numerically stable `log(pnorm(x))` instead of `log(1e-5 + pnorm(x))`.
* New model `skew_t`: the skew t distribution of Jones and Faddy (2003), standardized to mean zero and unit variance,
  with separate degrees of freedom for the left and the right tail (`df_left`, `df_right`). Supported by
  `estimate_parameters()`, `sim_sv()`, `predict()`, `residuals()` and `plot_returns()`.
* The degrees of freedom of the `t` and `skew_t` models are capped at 10,000. For larger values the `lgamma`
  differences in the density lost all precision, and the fit of the `t` model could run to `df = 1e14` with a
  meaningless likelihood. At 10,000 degrees of freedom the distribution is gaussian for all practical purposes.
* New `plot_returns()` plots the returns with the central interval of the fitted conditional distribution and marks
  the returns outside it.
* New `plot_diagnostics()` plots a Q-Q plot and the autocorrelation of the one-step-ahead residuals and of the squared
  residuals.
* `residuals()` has a new default method, `method = "grid"`. It calculates the one-step-ahead residuals exactly with a
  filter on a grid for the log-volatility, for all observations in a few seconds. The old method,
  `TMB::oneStepPredict` with the Laplace approximation, takes one to three seconds per observation and is available
  with `method = "tmb"`. With `method = "grid"`, `residuals()` returns residuals for all observations by default.
* `residuals(method = "tmb")` no longer fails with "Multiple TMB models loaded" when several TMB models are loaded,
  for example after `devtools::load_all()`. It then calculates the residuals without parallel processing.
* Plots use `linewidth` instead of the deprecated `size` for lines. `ggplot2` (>= 3.4.0) is required.

# stochvolTMB 0.2.0

* Added `predict` function that makes it possible to simulate future volatility and log-returns. 
* `plot` has a new `forecast` argument that makes it possible to include forecasted volatility. 
* The degrees of freedom for the `t` model is estimated on log scale and shifted by 2 so that it is guaranteed to be 
  greater than 2. This is to ensure that the variance is finite. 
* Changed parameter names from `phi_logit` and `rho_logit` to `logit_phi` and `logit_rho` to be consistent with other 
  parameter names. 
* Updated README.
* Updated documentation. 

# stochvolTMB 0.1.2

* CRAN release 