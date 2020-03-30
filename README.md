# stochvolTMB
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build Status](https://travis-ci.org/JensWahl/stochvolTMB.svg?branch=master)](https://travis-ci.org/JensWahl/stochvolTMB)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`stochvolTMB` is a package for fitting stochastic volatility (SV) models to time series data. Four distributions for the observational error are implemented: 
* **Gaussian** - The classic SV model with Gaussian noise 
* **t** - t distributed noise for heavy tail returns 
* **Leverage** - Taking correlation between returns and volatility into account 
* **Skew-Gaussian** - skew-gaussian distributed noise for asymmetric returns

It is build on [Template Model Builder](https://github.com/kaskr/adcomp) for fast and efficent estimation. The latent volatility is integrated out of the likelihood using the Laplace approximation and automatic differentiation (AD) is used for accurate evaluation of derivatives. 

## Installation

The package can be installed from github by running 
```
devtools::install_github("JensWahl/stochvolTMB")
```

## Example 

The main function for estimating parameters is `estimate_parameters`: 

```
# load data
data("spy")

# estimate parameters 
opt <- estimate_parameters(spy$log_return, model = "gaussian")

# get parameter estimates with standard error
estimates <- summary(opt)

## plot estimated volatility with 95 % confidence interval
plot(opt, include_ci = TRUE)
```

```
