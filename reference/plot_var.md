# Plot returns with the one-step-ahead Value at Risk

Plots the returns with `-VaR` and `-ES` from
[`var_es`](https://jenswahl.github.io/stochvolTMB/reference/var_es.md)
and marks the exceedances, the returns below `-VaR`.

## Usage

``` r
plot_var(x, level = NULL, dates = NULL)
```

## Arguments

- x:

  A `data.table` returned from
  [`var_es`](https://jenswahl.github.io/stochvolTMB/reference/var_es.md).

- level:

  The level to plot. The default is the largest level in `x`.

- dates:

  Optional vector of dates with one element for each observed return,
  used for the x-axis.

## Value

ggplot object.

## See also

[`var_es`](https://jenswahl.github.io/stochvolTMB/reference/var_es.md),
[`backtest_var`](https://jenswahl.github.io/stochvolTMB/reference/backtest_var.md)

## Examples

``` r
# \donttest{
opt <- estimate_parameters(spy$log_return, model = "skew_t_leverage", silent = TRUE)
plot_var(var_es(opt, level = 0.99), dates = spy$date)

# }
```
