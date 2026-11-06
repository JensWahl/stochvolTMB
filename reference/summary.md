# Summary tables of model parameters

Extract parameters, transformed parameters and latent log volatility
along with standard error, z-value and p-value

## Usage

``` r
# S3 method for class 'stochvolTMB'
summary(object, ..., report = c("all", "fixed", "transformed", "random"))
```

## Arguments

- object:

  A `stochvolTMB` object.

- ...:

  Currently not used.

- report:

  Parameters to report with uncertainty estimates. Can be any subset of
  "fixed", "transformed" or "random" (see
  [summary.sdreport](https://rdrr.io/pkg/TMB/man/summary.sdreport.html)).
  "fixed" report the parameters on the scale they were estimated, for
  example are all standard deviations estimated on log scale.
  "transformed" report all transformed parameters, for example estimated
  standard deviations transformed from log scale by taking the
  exponential. Lastly, "random" report the estimated latent
  log-volatility.

## Value

`data.table` with parameter estimates, standard error, z-value and
approximated p-value.
