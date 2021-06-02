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