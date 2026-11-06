# Standardized error distribution of a fitted model

The distribution of `y_t / (sigma_y * exp(h_t / 2))` given `h_t`. It has
mean zero and unit variance.

## Usage

``` r
standard_error_distribution(object)
```

## Arguments

- object:

  A `stochvolTMB` object.

## Value

A list of vectorized functions: `cdf`, `logpdf`, `quantile` and
`partial_mean`, where `partial_mean(z)` is `E[Z 1(Z <= z)]`.
