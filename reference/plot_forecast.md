# Add predicted volatility.

Adds predicted volatility to the volatility plot.

## Usage

``` r
plot_forecast(p, forecast, include_ci = TRUE)
```

## Arguments

- p:

  ggplot object

- forecast:

  data.table

- include_ci:

  logical value indicating if volatility should be plotted with
  approximately 95% confidence interval.#' @return ggplot object
