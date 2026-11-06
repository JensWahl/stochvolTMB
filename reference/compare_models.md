# Fit several models and compare them

Fits each model with
[`estimate_parameters`](https://jenswahl.github.io/stochvolTMB/reference/estimate_parameters.md)
and returns a table ranked by AIC.

## Usage

``` r
compare_models(
  data,
  models = c("gaussian", "t", "leverage", "skew_gaussian", "skew_gaussian_leverage",
    "skew_t", "skew_t_leverage"),
  ...
)
```

## Arguments

- data:

  A vector of observations.

- models:

  Character vector of models. See
  [`estimate_parameters`](https://jenswahl.github.io/stochvolTMB/reference/estimate_parameters.md).
  The default is all models.

- ...:

  Additional arguments passed to
  [`estimate_parameters`](https://jenswahl.github.io/stochvolTMB/reference/estimate_parameters.md).
  `silent = TRUE` is used unless you set it.

## Value

A `data.table` with one row for each model, ordered by AIC, and the
columns `model`, `n_par`, `log_lik`, `aic`, `bic`, `delta_aic` (the
difference to the smallest AIC), `converged`, `pd_hessian` and
`message`. The fitted models are in the attribute `"fits"`, a named list
of `stochvolTMB` objects.

## Details

The table also shows if the optimizer converged and if the Hessian is
positive definite. A model that fails one of these checks has unreliable
estimates and standard errors, even when its AIC is small. A model that
gives an error is reported with `NA` values and the error message.

## Examples

``` r
# \donttest{
comparison <- compare_models(spy$log_return, models = c("gaussian", "t", "leverage"))
comparison
#>       model n_par  log_lik       aic       bic delta_aic converged pd_hessian
#>      <char> <int>    <num>     <num>     <num>     <num>    <lgcl>     <lgcl>
#> 1: leverage     4 11808.43 -23608.85 -23584.18    0.0000      TRUE       TRUE
#> 2:        t     4 11729.85 -23451.69 -23427.02  157.1599      TRUE       TRUE
#> 3: gaussian     3 11718.29 -23430.57 -23412.07  178.2778      TRUE       TRUE
#>                     message
#>                      <char>
#> 1: relative convergence (4)
#> 2: relative convergence (4)
#> 3: relative convergence (4)
best <- attr(comparison, "fits")[[comparison$model[1]]]
# }
```
