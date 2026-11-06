# Exact one-step-ahead residuals by filtering on a grid for the log-volatility

Exact one-step-ahead residuals by filtering on a grid for the
log-volatility

## Usage

``` r
osa_grid(object, grid_size = 500)
```

## Arguments

- object:

  A `stochvolTMB` object.

- grid_size:

  Number of grid points.

## Value

Vector of one-step-ahead residuals for all observations.
