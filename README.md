
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stochvolTMB

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R build
status](https://github.com/JensWahl/stochvolTMB/workflows/R-CMD-check/badge.svg)](https://github.com/JensWahl/stochvolTMB/actions)
<!-- badges: end -->

`stochvolTMB` is a package for fitting stochastic volatility (SV) models
to time series data. It is inspired by the package
[stochvol](https://github.com/gregorkastner/stochvol), but parameter
estimates are obtained through optimization and not MCMC, leading to
significant speed up. It is built on [Template Model
Builder](https://github.com/kaskr/adcomp) for fast and efficient
estimation. The latent volatility is integrated out of the likelihood
using the Laplace approximation and automatic differentiation (AD) is
used for accurate evaluation of derivatives.

Four distributions for the observational error are implemented:

  - **Gaussian** - The classic SV model with Gaussian noise
  - **t** - t-distributed noise for heavy tail returns
  - **Leverage** - Extending the **Gaussian** model by allowing observed
    returns to be correlated with the latent volatility
  - **Skew-Gaussian** - Skew-Gaussian distributed noise for asymmetric
    returns

## Installation

You can install `stochvolTMB` from github by running

``` r
# install.packages("devtools")
devtools::install_github("JensWahl/stochvolTMB")
```

## Example

The main function for estimating parameters is `estimate_parameters`:

``` r
library(stochvolTMB, warn.conflicts = FALSE)

# load s&p500 data from 2005 to 2018
data(spy)

# estimate parameters in a gaussian model
opt <- estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)
#> Warning in checkMatrixPackageVersion(): Package version inconsistency detected.
#> TMB was built with Matrix version 1.2.18
#> Current Matrix version is 1.2.17
#> Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package

# get parameter estimates with standard error
estimates <- summary(opt)
head(estimates, 10)
#>       parameter     estimate    std_error     z_value      p_value
#>  1: log_sigma_y -4.805444178 0.0893661590 -53.7725268 0.000000e+00
#>  2: log_sigma_h -1.503114690 0.0854421060 -17.5922009 2.826823e-69
#>  3:   phi_logit  4.547489989 0.2246000006  20.2470613 3.770740e-91
#>  4:     sigma_y  0.008185065 0.0007314678  11.1899181 4.568554e-29
#>  5:     sigma_h  0.222436260 0.0190054225  11.7038314 1.218261e-31
#>  6:         phi  0.979034580 0.0046594721 210.1170609 0.000000e+00
#>  7:           h -0.378840079 0.5151780523  -0.7353576 4.621218e-01
#>  8:           h -0.440335544 0.5104392407  -0.8626601 3.883244e-01
#>  9:           h -0.485412499 0.5047402313  -0.9617076 3.361965e-01
#> 10:           h -0.512076392 0.4968052069  -1.0307388 3.026633e-01
#>            type
#>  1:       fixed
#>  2:       fixed
#>  3:       fixed
#>  4: transformed
#>  5: transformed
#>  6: transformed
#>  7:      random
#>  8:      random
#>  9:      random
#> 10:      random

## plot estimated volatility with 95 % confidence interval
plot(opt, include_ci = TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Benchmark

An comparison of `stochvol` and `stochvolTMB`:

``` r
library(stochvol)
#> Loading required package: coda
library(stochvolTMB)
library(microbenchmark)
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.6.2

data(spy)

# 
# ct <- microbenchmark(
#   stochvol_gauss = {mod1 <- svsample(spy$log_return, quiet = TRUE)},
#   stochvol_lev = {mod1 <- svlsample(spy$log_return, quiet = TRUE)},
#   stochvolTMB_gauss  = {mod2 <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE)},
#   stochvolTMB_gauss  = {mod2 <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE)},
#   times = 2L
# )
# 
# autoplot(ct, log = FALSE) + stat_summary(fun.y = median, geom = 'point', size = 2)
```

## Shiny app

By running `demo()` you start a shiny application where you can visually
inspect the effect of choosing different models and parameter
configurations

``` r
demo()
```

![](man/figures/shinyApp.png)
