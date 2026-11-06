# Filter on a grid for the log-volatility

Keeps the predictive density of `h_t` on a grid. For each observation it
calculates the predictive distribution function of `y_t`, updates the
density with `y_t` and moves it one step forward. Grid points with
negligible weight are skipped, so the cost per step is small.

## Usage

``` r
grid_filter(object, grid_size = 500, width = 8)
```

## Arguments

- object:

  A `stochvolTMB` object.

- grid_size:

  Number of grid points.

- width:

  Half-width of the grid in stationary standard deviations of `h_t`.

## Value

A list with `prob`, the predictive distribution function at each `y_t`;
`weights`, a list of length `n + 1` where element `t` holds the grid
indices `idx` and the normalized predictive weights `w` of `h_t` given
`y_1, ..., y_{t-1}` (element `n + 1` is the forecast); and `scale`, the
conditional standard deviation `sigma_y * exp(h / 2)` at each grid
point.
