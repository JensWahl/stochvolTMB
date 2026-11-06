# Calculate one-step-ahead (OSA) residuals for stochastic volatility model.

The OSA residual of observation `y_t` is `qnorm(F_t(y_t))`, where `F_t`
is the predictive distribution function of `y_t` given
`y_1, ..., y_{t-1}` at the estimated parameters. If the model is
correct, the residuals are independent and standard gaussian.

## Usage

``` r
residuals(
  object,
  conditional = NULL,
  method = c("grid", "tmb"),
  grid_size = 500,
  ...
)
```

## Arguments

- object:

  A `stochvolTMB` object.

- conditional:

  Index vector of observations that are not predicted. Residuals are
  returned for the other observations. If `NULL`, the `"grid"` method
  returns residuals for all observations, and the `"tmb"` method uses
  `1:(object$nobs - 100)`. Use `integer(0)` to get all residuals with
  `"tmb"`.

- method:

  `"grid"` or `"tmb"`. See details.

- grid_size:

  Number of grid points for `h_t` in the `"grid"` method.

- ...:

  Currently not used.

## Value

Vector of one-step-ahead residuals. If the model is correctly specified,
these should be standard normal.

## Details

Two methods are available:

- `"grid"` (default): an exact filter on a grid for the log-volatility
  `h_t`. Because `h_t` is one-dimensional, the predictive distribution
  can be calculated by summation over the grid, without the Laplace
  approximation. It calculates the residuals for all observations in a
  few seconds.

- `"tmb"`:
  [oneStepPredict](https://rdrr.io/pkg/TMB/man/oneStepPredict.html) with
  `method = "oneStepGeneric"`. It uses the Laplace approximation and
  takes about one to three seconds per observation, so by default only
  the last 100 observations are used.

## References

Thygesen, U. H., Albertsen, C. M., Berg, C. W., Kristensen, K. and
Nielsen, A. (2017). Validation of ecological state space models using
the Laplace approximation. Environmental and Ecological Statistics,
24(2), 317-339.
[doi:10.1007/s10651-017-0372-4](https://doi.org/10.1007/s10651-017-0372-4)
