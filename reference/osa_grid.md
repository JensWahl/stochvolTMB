# Exact one-step-ahead residuals by filtering on a grid for the log-volatility

Keeps the predictive density of `h_t` on a grid. For each observation it
calculates the predictive distribution function of `y_t`, updates the
density with `y_t` and moves it one step forward. Grid points with
negligible weight are skipped, so the cost per step is small.

## Usage

``` r
osa_grid(object, grid_size = 500, width = 8)
```

## Arguments

- object:

  A `stochvolTMB` object.

- grid_size:

  Number of grid points.

- width:

  Half-width of the grid in stationary standard deviations of `h_t`.

## Value

Vector of one-step-ahead residuals for all observations.
