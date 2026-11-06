# Extract parameter estimates and their covariance matrix

Extract parameter estimates and their covariance matrix

## Usage

``` r
# S3 method for class 'stochvolTMB'
coef(object, scale = c("natural", "estimation"), ...)

# S3 method for class 'stochvolTMB'
vcov(object, scale = c("natural", "estimation"), ...)
```

## Arguments

- object:

  A `stochvolTMB` object.

- scale:

  `"natural"` (default) returns the parameters on their natural scale,
  for example `sigma_y`, `phi` and `rho`. Their covariance matrix comes
  from the delta method. `"estimation"` returns the parameters on the
  scale they were estimated, for example `log_sigma_y` and `logit_phi`,
  with the inverse Hessian as covariance matrix.

- ...:

  Not used.

## Value

`coef` returns a named vector. `vcov` returns a named matrix.

## Examples

``` r
# \donttest{
opt <- estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)
coef(opt)
#>      sigma_y      sigma_h          phi          rho 
#>  0.008338412  0.273443559  0.967721215 -0.748695259 
sqrt(diag(vcov(opt)))
#>      sigma_y      sigma_h          phi          rho 
#> 0.0004163314 0.0182641070 0.0043681868 0.0322487815 
coef(opt, scale = "estimation")
#> log_sigma_y log_sigma_h   logit_phi   logit_rho 
#>   -4.786882   -1.296660    4.110221   -1.939959 
# }
```
