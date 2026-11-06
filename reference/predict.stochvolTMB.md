# Predict future returns and future volatilities

Takes a `stochvolTMB` object and produces draws from the predictive
distribution of the latent volatility and future log-returns.

## Usage

``` r
# S3 method for class 'stochvolTMB'
predict(object, steps = 1L, nsim = 10000, include_parameters = TRUE, ...)
```

## Arguments

- object:

  A `stochvolTMB` object returned from
  [`estimate_parameters`](https://jenswahl.github.io/stochvolTMB/reference/estimate_parameters.md).

- steps:

  Integer specifying number of steps to predict.

- nsim:

  Number of draws from the predictive distribution.

- include_parameters:

  Logical value indicating if fixed parameters should be simulated from
  their asymptotic distribution, i.e. multivariate normal with inverse
  hessian as covariance matrix.

- ...:

  Not is use.

## Value

List of simulated values from the predictive distribution of the latent
volatilities and log-returns.
