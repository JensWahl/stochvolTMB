
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

# find the best model using AIC 
AIC(estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE),
    estimate_parameters(spy$log_return, model = "t", silent = TRUE),
    estimate_parameters(spy$log_return, model = "skew_gaussian", silent = TRUE),
    estimate_parameters(spy$log_return, model = "leverage", silent = TRUE))
#> Warning in checkMatrixPackageVersion(): Package version inconsistency detected.
#> TMB was built with Matrix version 1.2.18
#> Current Matrix version is 1.2.17
#> Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
#>                                                                             df
#> estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)       3
#> estimate_parameters(spy$log_return, model = "t", silent = TRUE)              4
#> estimate_parameters(spy$log_return, model = "skew_gaussian", silent = TRUE)  4
#> estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)       4
#>                                                                                   AIC
#> estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)      -23430.57
#> estimate_parameters(spy$log_return, model = "t", silent = TRUE)             -23451.69
#> estimate_parameters(spy$log_return, model = "skew_gaussian", silent = TRUE) -23440.87
#> estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)      -23608.85

# The leverage model stand out with an AIC for below the other models
opt <- estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)

# get parameter estimates with standard error
estimates <- summary(opt)
head(estimates, 10)
#>       parameter     estimate    std_error     z_value       p_value
#>  1: log_sigma_y -4.786880902 0.0499293705 -95.8730474  0.000000e+00
#>  2: log_sigma_h -1.296652044 0.0667927998 -19.4130512  5.986433e-84
#>  3:   phi_logit  4.110208379 0.1375465458  29.8823090 3.341130e-196
#>  4:   rho_logit -1.939946750 0.1467666528 -13.2178987  6.916940e-40
#>  5:     sigma_y  0.008338425 0.0004163323  20.0282918  3.121840e-89
#>  6:     sigma_h  0.273445746 0.0182642070  14.9716736  1.124552e-50
#>  7:         phi  0.967720808 0.0043682334 221.5359687  0.000000e+00
#>  8:         rho -0.748692587 0.0322489934 -23.2159986 3.138829e-119
#>  9:           h -0.536253306 0.5182212112  -1.0347961  3.007641e-01
#> 10:           h -0.207810839 0.4245275458  -0.4895108  6.244801e-01
#>            type
#>  1:       fixed
#>  2:       fixed
#>  3:       fixed
#>  4:       fixed
#>  5: transformed
#>  6: transformed
#>  7: transformed
#>  8: transformed
#>  9:      random
#> 10:      random

## plot estimated volatility with 95 % confidence interval
plot(opt, include_ci = TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Comparison of stochvolTMB and stochvol

A quick comparison of `stochvolTMB` and `stochvol` shows that
`stochvolTMB` is 10-50x times faster than `stochvol`:

``` r
library(stochvol)
#> Loading required package: coda
library(stochvolTMB)


# load s&p500 data from 2005 to 2018
data(spy)

system.time(stochvol_gauss <- svsample(spy$log_return, quiet = T))
#>    user  system elapsed 
#>  20.333   2.716  27.139
system.time(stochvolTMB_gauss  <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE))
#>    user  system elapsed 
#>   5.432   0.104   5.792

system.time(stochvol_lev <- svlsample(spy$log_return, quiet = T))
#>    user  system elapsed 
#> 208.407   6.994 236.095
system.time(stochvolTMB_lev  <- estimate_parameters(spy$log_return, "leverage", silent = TRUE))
#>    user  system elapsed 
#>  12.046   0.293  13.965
```

We can compare the parameter estimates of the two methods. Note that the
parameter `exp(mu/2)` and `sigma` from `stochvol` is the same as
`sigma_y` and `sigma_h` from `stochvolTMB`. Both methods give almost
identical results.

``` r

stochvol_gauss$summary$para
#>                   mean           sd           5%          50%          95%
#> mu        -9.612369978 0.1882305957 -9.914775270 -9.614371461 -9.301626207
#> phi        0.978114436 0.0049302166  0.969390919  0.978434868  0.985729252
#> sigma      0.230453924 0.0201704851  0.199187372  0.229672233  0.265333016
#> exp(mu/2)  0.008215421 0.0007793415  0.007031272  0.008170822  0.009553831
#> sigma^2    0.053515819 0.0093920028  0.039675609  0.052749334  0.070401609
#>                 ESS
#> mu        7049.2730
#> phi        352.0666
#> sigma      168.1086
#> exp(mu/2) 7049.2730
#> sigma^2    168.1086
summary(stochvolTMB_gauss, report = "transformed")
#>    parameter    estimate    std_error   z_value      p_value        type
#> 1:   sigma_y 0.008185065 0.0007314678  11.18992 4.568554e-29 transformed
#> 2:   sigma_h 0.222436260 0.0190054225  11.70383 1.218261e-31 transformed
#> 3:       phi 0.979034580 0.0046594721 210.11706 0.000000e+00 transformed

stochvol_lev$summary$para
#>                   mean          sd           5%         50%          95%
#> mu        -9.568223536 0.105255717 -9.744873801 -9.56470987 -9.395951612
#> phi        0.966877192 0.004069966  0.959723754  0.96712169  0.973220120
#> sigma      0.276640595 0.016872604  0.250343104  0.27581482  0.305919303
#> rho       -0.722614648 0.031438025 -0.770007375 -0.72547112 -0.663230598
#> exp(mu/2)  0.008373118 0.000440111  0.007654689  0.00837625  0.009113706
#> sigma^2    0.076814675 0.009425402  0.062671670  0.07607382  0.093586620
#>                 ESS
#> mu        548.91425
#> phi       175.79791
#> sigma      62.11424
#> rho        48.91482
#> exp(mu/2) 548.91425
#> sigma^2    62.11424
summary(stochvolTMB_lev, report = "transformed")
#>    parameter     estimate    std_error   z_value       p_value        type
#> 1:   sigma_y  0.008338425 0.0004163323  20.02829  3.121840e-89 transformed
#> 2:   sigma_h  0.273445746 0.0182642070  14.97167  1.124552e-50 transformed
#> 3:       phi  0.967720808 0.0043682334 221.53597  0.000000e+00 transformed
#> 4:       rho -0.748692587 0.0322489934 -23.21600 3.138829e-119 transformed
```

## Shiny app

By running `demo()` you start a shiny application where you can visually
inspect the effect of choosing different models and parameter
configurations

``` r
demo()
```

![](man/figures/shinyApp.png)
