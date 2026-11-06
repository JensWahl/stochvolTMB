# Calculate quantiles based on predictions from the predictive distribution

Calculate quantiles based on predictions from the predictive
distribution

## Usage

``` r
# S3 method for class 'stochvolTMB_predict'
summary(object, ..., quantiles = c(0.025, 0.975), predict_mean = TRUE)
```

## Arguments

- object:

  A `stochvolTMB_summary` object.

- ...:

  Not used.

- quantiles:

  A numeric vector specifying which quantiles to calculate.

- predict_mean:

  bool. Should the mean be predicted?

## Value

A list of `data.table`s. One for `y`, `h` and `h_exp`.
