# Construct objective function with derivatives using [MakeADFun](https://rdrr.io/pkg/TMB/man/MakeADFun.html)

Construct objective function with derivatives using
[MakeADFun](https://rdrr.io/pkg/TMB/man/MakeADFun.html)

## Usage

``` r
get_nll(data, model = "gaussian", ...)
```

## Arguments

- data:

  Vector of observations.

- model:

  String specifying distribution of error term in observational
  equation.

- ...:

  Additional arguments passed to
  [MakeADFun](https://rdrr.io/pkg/TMB/man/MakeADFun.html).

## Value

List of components returned from
[MakeADFun](https://rdrr.io/pkg/TMB/man/MakeADFun.html).
