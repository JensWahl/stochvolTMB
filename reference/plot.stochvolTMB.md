# Plot the estimated latent volatility process

Displays the estimated latent volatility process over time.

## Usage

``` r
# S3 method for class 'stochvolTMB'
plot(x, ..., include_ci = TRUE, plot_log = TRUE, dates = NULL, forecast = NULL)
```

## Arguments

- x:

  A `stochvolTMB` object returned from
  [estimate_parameters](estimate_parameters.md).

- ...:

  Currently not used.

- include_ci:

  Logical value indicating if volatility should be plotted with
  approximately 95% confidence interval.

- plot_log:

  Logical value indicating if the estimated should be plotted on log or
  original scale. If `plot_log = TRUE` the process h is plotted. If
  `plot_log = FALSE` 100 `sigma_y` exp(`h` / 2) is plotted.

- dates:

  Vector of length ncol(x\$nobs), providing optional dates for labeling
  the x-axis. The default value is NULL; in this case, the axis will be
  labeled with numbers.

- forecast:

  Integer specifying number of steps to forecast.

## Value

ggplot object with plot of estimated estimated volatility.
