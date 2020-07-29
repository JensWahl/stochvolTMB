
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stochvolTMB

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build
Status](https://travis-ci.org/JensWahl/stochvolTMB.svg?branch=master)](https://travis-ci.org/JensWahl/stochvolTMB)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

`stochvolTMB` is a package for fitting stochastic volatility (SV) models
to time series data. Four distributions for the observational error are
implemented:

  - **Gaussian** - The classic SV model with Gaussian noise
  - **t** - t-distributed noise for heavy tail returns
  - **Leverage** - Taking correlation between returns and volatility
    into account
  - **Skew-Gaussian** - skew-gaussian distributed noise for asymmetric
    returns

It is built on [Template Model Builder](https://github.com/kaskr/adcomp)
for fast and efficient estimation. The latent volatility is integrated
out of the likelihood using the Laplace approximation and automatic
differentiation (AD) is used for accurate evaluation of derivatives.

## Installation

You can install stochvolTMB from github by running

``` r
# install.packages("devtools")
devtools::install_github("JensWahl/stochvolTMB")
```

## Example

The main function for estimating parameters is `estimate_parameters`:

``` r
library(stochvolTMB, warn.conflicts = FALSE)

# load s&p500 data from 2005 to 2018
data("spy")

# estimate parameters in a gaussian model
opt <- estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)

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

By running `demo()` you start a shiny application where you can visually
inspect the effect of choosing different models and parameter settings.

``` r
demo()
```

![](man/figures/shinyApp.png)
