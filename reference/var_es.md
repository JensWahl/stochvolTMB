# One-step-ahead Value at Risk and Expected Shortfall

Calculates the one-day Value at Risk (VaR) and Expected Shortfall (ES)
of the returns from the predictive distribution of `y_t` given
`y_1, ..., y_{t-1}`, for every observation and for the next day after
the data.

## Usage

``` r
var_es(object, level = c(0.95, 0.99), grid_size = 500)
```

## Arguments

- object:

  A `stochvolTMB` object returned from
  [`estimate_parameters`](https://jenswahl.github.io/stochvolTMB/reference/estimate_parameters.md).

- level:

  Vector of confidence levels between 0.5 and 1.

- grid_size:

  Number of grid points for the log-volatility.

## Value

A `data.table` with one row for each time point and level, and the
columns `time` (1 to `nobs + 1`), `level`, `y` (the observed return,
`NA` for the forecast), `var`, `es` and `exceedance` (`y < -var`).

## Details

The predictive distribution is calculated exactly with the same filter
on a grid for the log-volatility as
[`residuals.stochvolTMB`](https://jenswahl.github.io/stochvolTMB/reference/residuals.stochvolTMB.md).
It is a mixture over `h_t` of the conditional distribution of the
returns. For a level \\\alpha\\, for example 0.99, let \\q_t\\ be the
\\1 - \alpha\\ quantile of the predictive distribution. Then \$\$VaR_t =
-q_t, \qquad ES_t = -E\[y_t \mid y_t \le q_t, y_1, \ldots,
y\_{t-1}\].\$\$ Both are reported as positive numbers when they are
losses, on the scale of the returns. A return is an exceedance if \\y_t
\< -VaR_t\\.

The parameters are fixed at their estimates from the full data, and the
uncertainty of the parameters is not included. The values for the
observed data are therefore in-sample, and a backtest with
[`backtest_var`](https://jenswahl.github.io/stochvolTMB/reference/backtest_var.md)
on them is slightly optimistic. The last row (`time = nobs + 1`) is the
forecast for the next day. For more than one day, use
[`forecast_var_es`](https://jenswahl.github.io/stochvolTMB/reference/forecast_var_es.md).

## See also

[`backtest_var`](https://jenswahl.github.io/stochvolTMB/reference/backtest_var.md),
[`forecast_var_es`](https://jenswahl.github.io/stochvolTMB/reference/forecast_var_es.md)

## Examples

``` r
# \donttest{
opt <- estimate_parameters(spy$log_return, model = "skew_t_leverage", silent = TRUE)
risk <- var_es(opt, level = c(0.95, 0.99))
# Forecast for the next day
risk[is.na(y)]
#>     time level     y        var         es exceedance
#>    <int> <num> <num>      <num>      <num>     <lgcl>
#> 1:  3523  0.95    NA 0.03181108 0.04560319         NA
#> 2:  3523  0.99    NA 0.05359064 0.06893175         NA
backtest_var(risk)
#>    level     n expected observed        rate      lr_uc      p_uc    lr_ind
#>    <num> <int>    <num>    <int>       <num>      <num>     <num>     <num>
#> 1:  0.95  3522   176.10      191 0.054230551 1.29303116 0.2554901 1.3599331
#> 2:  0.99  3522    35.22       34 0.009653606 0.04318348 0.8353802 0.9112984
#>        p_ind     lr_cc      p_cc  es_ratio
#>        <num>     <num>     <num>     <num>
#> 1: 0.2435491 2.6529642 0.2654093 0.9890637
#> 2: 0.3397702 0.9544819 0.6204930 1.0067746
# }
```
