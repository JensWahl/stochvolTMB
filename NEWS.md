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