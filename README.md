
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

# The leverage model stand out with an AIC for below the other models
opt <- estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)

# get parameter estimates with standard error
estimates <- summary(opt)
head(estimates, 10)

## plot estimated volatility with 95 % confidence interval
plot(opt, include_ci = TRUE)
```

## Comparison of stochvolTMB and stochvol

A quick comparison of `stochvolTMB` and `stochvol` shows that
`stochvolTMB` is 10-50x times faster than `stochvol`:

``` r
library(stochvol)
library(stochvolTMB)
library(microbenchmark)
library(ggplot2)

# load s&p500 data from 2005 to 2018
data(spy)

system.time(stochvol_gauss <- svsample(spy$log_return, quiet = T))
system.time(stochvolTMB_gauss  <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE))

system.time(stochvol_lev <- svlsample(spy$log_return, quiet = T))
system.time(stochvolTMB_lev  <- estimate_parameters(spy$log_return, "leverage", silent = TRUE))
   
```

We can compare the parameter estimates of the two methods. Note that the
parameter `exp(mu/2)` and `sigma` from `stochvol` is the same as
`sigma_y` and `sigma_h` from `stochvolTMB`. Both methods give almost
identical results.

``` r

stochvol_gauss$summary$para
summary(stochvolTMB_gauss, report = "transformed")

stochvol_lev$summary$para
summary(stochvolTMB_lev, report = "transformed")
```

## Shiny app

By running `demo()` you start a shiny application where you can visually
inspect the effect of choosing different models and parameter
configurations

``` r
demo()
```

![](man/figures/shinyApp.png)
