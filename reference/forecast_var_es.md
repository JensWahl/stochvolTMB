# Value at Risk and Expected Shortfall for several days ahead

Simulates future returns with
[`predict.stochvolTMB`](https://jenswahl.github.io/stochvolTMB/reference/predict.stochvolTMB.md)
and calculates the Value at Risk (VaR) and Expected Shortfall (ES) for
each day ahead, both of the return on that day and of the cumulative
return (the sum of the log-returns) up to that day. The cumulative
values for `step = 10` are, for example, the 10-day VaR and ES.

## Usage

``` r
forecast_var_es(
  object,
  steps = 10L,
  level = c(0.95, 0.99),
  nsim = 100000L,
  include_parameters = TRUE
)
```

## Arguments

- object:

  A `stochvolTMB` object returned from
  [`estimate_parameters`](https://jenswahl.github.io/stochvolTMB/reference/estimate_parameters.md).

- steps:

  Number of days ahead.

- level:

  Vector of confidence levels between 0.5 and 1.

- nsim:

  Number of simulated paths.

- include_parameters:

  Logical. If `TRUE`, the parameters are drawn from their asymptotic
  distribution for each path. See
  [`predict.stochvolTMB`](https://jenswahl.github.io/stochvolTMB/reference/predict.stochvolTMB.md).

## Value

A `data.table` with the columns `step`, `level`, `var`, `es`,
`var_cumulative` and `es_cumulative`.

## Details

VaR and ES are reported as positive numbers when they are losses: if
\\q\\ is the \\1 - \alpha\\ quantile of the simulated returns, then
\\VaR = -q\\ and \\ES = -E\[y \mid y \le q\]\\. By default, the
simulation includes the uncertainty of the parameters. The Monte Carlo
error decreases with `nsim`. For one day ahead,
[`var_es`](https://jenswahl.github.io/stochvolTMB/reference/var_es.md)
is exact and faster.

## See also

[`var_es`](https://jenswahl.github.io/stochvolTMB/reference/var_es.md)

## Examples

``` r
# \donttest{
opt <- estimate_parameters(spy$log_return, model = "skew_t_leverage", silent = TRUE)
set.seed(1)
forecast_var_es(opt, steps = 10, level = 0.99)
#>      step level        var         es var_cumulative es_cumulative
#>     <int> <num>      <num>      <num>          <num>         <num>
#>  1:     1  0.99 0.05284419 0.06827803     0.05284419    0.06827803
#>  2:     2  0.99 0.05408404 0.07018714     0.07544342    0.09725807
#>  3:     3  0.99 0.05342402 0.06941009     0.09417109    0.11927875
#>  4:     4  0.99 0.05437557 0.07234534     0.11113296    0.14179133
#>  5:     5  0.99 0.05413629 0.07197306     0.12424638    0.16123284
#>  6:     6  0.99 0.05355018 0.07096271     0.13704527    0.17910890
#>  7:     7  0.99 0.05425692 0.07263728     0.15025210    0.19660826
#>  8:     8  0.99 0.05310403 0.07119998     0.15934538    0.20872529
#>  9:     9  0.99 0.05418832 0.07378119     0.17189793    0.22574334
#> 10:    10  0.99 0.05326162 0.07256297     0.18142237    0.24064329
# }
```
