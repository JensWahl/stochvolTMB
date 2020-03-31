
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stochvolTMB

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build
Status](https://travis-ci.org/JensWahl/stochvolTMB.svg?branch=master)](https://travis-ci.org/JensWahl/stochvolTMB)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`stochvolTMB` is a package for fitting stochastic volatility (SV) models to time series data. Four distributions for the observational error are implemented: 
* **Gaussian** - The classic SV model with Gaussian noise 
* **t** - t-distributed noise for heavy tail returns 
* **Leverage** - Taking correlation between returns and volatility into account 
* **Skew-Gaussian** - skew-gaussian distributed noise for asymmetric returns

It is build on [Template Model Builder](https://github.com/kaskr/adcomp) for fast and efficent estimation. The latent volatility is integrated out of the likelihood using the Laplace approximation and automatic differentiation (AD) is used for accurate evaluation of derivatives. 

## Installation

You can install stochvolTMB from github by running

``` r
# install.packages("devtools")
devtools::install_github("JensWahl/stochvolTMB")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JensWahl/stochvolTMB")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(stochvolTMB)
#> 
#> Attaching package: 'stochvolTMB'
#> The following object is masked from 'package:stats':
#> 
#>     residuals
#> The following object is masked from 'package:utils':
#> 
#>     demo
# load data
data("spy")

# estimate parameters 
opt <- estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)

# get parameter estimates with standard error
estimates <- summary(opt)
estimates
#>         parameter     estimate    std_error    z_value      p_value
#>    1: log_sigma_y -4.805444178 0.0893661590 -53.772527 0.000000e+00
#>    2: log_sigma_h -1.503114690 0.0854421060 -17.592201 2.826823e-69
#>    3:   phi_logit  4.547489989 0.2246000006  20.247061 3.770740e-91
#>    4:     sigma_y  0.008185065 0.0007314678  11.189918 4.568554e-29
#>    5:     sigma_h  0.222436260 0.0190054225  11.703831 1.218261e-31
#>   ---                                                              
#> 3524:           h  1.667727247 0.4128180027   4.039861 5.348295e-05
#> 3525:           h  1.703562545 0.4134933195   4.119928 3.789914e-05
#> 3526:           h  1.605958817 0.4577756293   3.508179 4.511855e-04
#> 3527:           h  1.529107961 0.4981658247   3.069476 2.144348e-03
#> 3528:           h  1.478040218 0.5338428602   2.768680 5.628383e-03
#>              type
#>    1:       fixed
#>    2:       fixed
#>    3:       fixed
#>    4: transformed
#>    5: transformed
#>   ---            
#> 3524:      random
#> 3525:      random
#> 3526:      random
#> 3527:      random
#> 3528:      random

## plot estimated volatility with 95 % confidence interval
plot(opt, include_ci = TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" />

By running `demo()` you start a shiny application where you can visually
inspect the effect of choosing different models and parameter settings.
