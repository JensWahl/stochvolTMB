# Backtest Value at Risk and Expected Shortfall

Tests if the exceedances of the Value at Risk (returns below `-var`)
have the expected frequency and are independent over time.

## Usage

``` r
backtest_var(x)
```

## Arguments

- x:

  A `data.table` returned from
  [`var_es`](https://jenswahl.github.io/stochvolTMB/reference/var_es.md).
  Rows without an observed return are ignored.

## Value

A `data.table` with one row for each level and the columns `level`, `n`,
`expected` and `observed` (number of exceedances), `rate` (observed
rate), `lr_uc`, `p_uc`, `lr_ind`, `p_ind`, `lr_cc`, `p_cc` and
`es_ratio`.

## Details

Let \\I_t = 1\\ if \\y_t \< -VaR_t\\ and \\p = 1 - \alpha\\ the expected
exceedance rate.

- **Unconditional coverage** (Kupiec 1995): a likelihood ratio test of
  \\P(I_t = 1) = p\\. A small p-value means too many or too few
  exceedances.

- **Independence** (Christoffersen 1998): a likelihood ratio test that
  \\I_t\\ is a Markov chain with \\P(I_t = 1 \mid I\_{t-1} = 0) = P(I_t
  = 1 \mid I\_{t-1} = 1)\\. A small p-value means that exceedances come
  in clusters, which happens when the model reacts too slowly to changes
  in volatility.

- **Conditional coverage**: the sum of the two statistics, with two
  degrees of freedom.

`es_ratio` is the mean of \\-y_t / ES_t\\ over the exceedances. It is
close to one if the Expected Shortfall is correct, and larger than one
if the model underestimates the size of the losses in the tail.

## References

Kupiec, P. H. (1995). Techniques for verifying the accuracy of risk
measurement models. The Journal of Derivatives, 3(2), 73-84.
[doi:10.3905/jod.1995.407942](https://doi.org/10.3905/jod.1995.407942)

Christoffersen, P. F. (1998). Evaluating interval forecasts.
International Economic Review, 39(4), 841-862.
[doi:10.2307/2527341](https://doi.org/10.2307/2527341)

## See also

[`var_es`](https://jenswahl.github.io/stochvolTMB/reference/var_es.md)
