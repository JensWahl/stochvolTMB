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
#>  1:     1  0.99 0.05321239 0.06872867     0.05321239    0.06872867
#>  2:     2  0.99 0.05466494 0.07102813     0.07654920    0.09854347
#>  3:     3  0.99 0.05392145 0.07070557     0.09661490    0.12275207
#>  4:     4  0.99 0.05404943 0.07126835     0.11197686    0.14337785
#>  5:     5  0.99 0.05398812 0.07171376     0.12566563    0.16325650
#>  6:     6  0.99 0.05388871 0.07200359     0.13942413    0.17975980
#>  7:     7  0.99 0.05431752 0.07300936     0.15104218    0.19599194
#>  8:     8  0.99 0.05406979 0.07313807     0.16243718    0.21102743
#>  9:     9  0.99 0.05427650 0.07360896     0.17503939    0.22806235
#> 10:    10  0.99 0.05287490 0.07258450     0.18626579    0.24215530
# }
```
