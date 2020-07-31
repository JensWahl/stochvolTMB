
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

gaussian <- estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)
#> Warning in checkMatrixPackageVersion(): Package version inconsistency detected.
#> TMB was built with Matrix version 1.2.18
#> Current Matrix version is 1.2.17
#> Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
t_dist <- estimate_parameters(spy$log_return, model = "t", silent = TRUE)
skew_gaussian <- estimate_parameters(spy$log_return, model = "skew_gaussian", silent = TRUE)
leverage <- estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)

AIC(gaussian, t_dist, skew_gaussian, leverage)
#>               df       AIC
#> gaussian       3 -23430.57
#> t_dist         4 -23451.69
#> skew_gaussian  4 -23440.87
#> leverage       4 -23608.85

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
plot(opt, include_ci = TRUE, dates = spy$date)
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
#>  22.026   2.859  31.908
system.time(stochvolTMB_gauss  <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE))
#>    user  system elapsed 
#>   5.965   0.164   7.321

system.time(stochvol_lev <- svlsample(spy$log_return, quiet = T))
#>    user  system elapsed 
#> 200.417   6.142 221.042
system.time(stochvolTMB_lev  <- estimate_parameters(spy$log_return, "leverage", silent = TRUE))
#>    user  system elapsed 
#>  11.060   0.183  11.532
```

We can compare the parameter estimates of the two methods. Note that the
parameter `exp(mu/2)` and `sigma` from `stochvol` is the same as
`sigma_y` and `sigma_h` from `stochvolTMB`. Both methods give almost
identical results.

``` r

knitr::kable(stochvol_gauss$summary$para[, 1:2])
```

|           |        mean |        sd |
| --------- | ----------: | --------: |
| mu        | \-9.6097634 | 0.1882512 |
| phi       |   0.9778929 | 0.0048834 |
| sigma     |   0.2322102 | 0.0195871 |
| exp(mu/2) |   0.0082261 | 0.0007778 |
| sigma^2   |   0.0543052 | 0.0092344 |

``` r
knitr::kable(summary(stochvolTMB_gauss, report = "transformed"))
```

| parameter |  estimate | std\_error |  z\_value | p\_value | type        |
| :-------- | --------: | ---------: | --------: | -------: | :---------- |
| sigma\_y  | 0.0081851 |  0.0007315 |  11.18992 |        0 | transformed |
| sigma\_h  | 0.2224363 |  0.0190054 |  11.70383 |        0 | transformed |
| phi       | 0.9790346 |  0.0046595 | 210.11706 |        0 | transformed |

``` r

knitr::kable(stochvol_lev$summary$para)
```

|           |        mean |        sd |          5% |         50% |         95% |       ESS |
| --------- | ----------: | --------: | ----------: | ----------: | ----------: | --------: |
| mu        | \-9.5711917 | 0.1006060 | \-9.7414524 | \-9.5724753 | \-9.4104730 | 479.41675 |
| phi       |   0.9667975 | 0.0044653 |   0.9591423 |   0.9670062 |   0.9736509 | 145.40795 |
| sigma     |   0.2777112 | 0.0186322 |   0.2453792 |   0.2779335 |   0.3097675 |  38.19010 |
| rho       | \-0.7242179 | 0.0343941 | \-0.7681707 | \-0.7291629 | \-0.6635408 |  39.89568 |
| exp(mu/2) |   0.0083597 | 0.0004203 |   0.0076678 |   0.0083438 |   0.0090478 | 479.41675 |
| sigma^2   |   0.0774707 | 0.0103764 |   0.0602109 |   0.0772470 |   0.0959559 |  38.19010 |

``` r
knitr::kable(summary(stochvolTMB_lev, report = "transformed"))
```

| parameter |    estimate | std\_error |   z\_value | p\_value | type        |
| :-------- | ----------: | ---------: | ---------: | -------: | :---------- |
| sigma\_y  |   0.0083384 |  0.0004163 |   20.02829 |        0 | transformed |
| sigma\_h  |   0.2734457 |  0.0182642 |   14.97167 |        0 | transformed |
| phi       |   0.9677208 |  0.0043682 |  221.53597 |        0 | transformed |
| rho       | \-0.7486926 |  0.0322490 | \-23.21600 |        0 | transformed |

## Shiny app

By running `demo()` you start a shiny application where you can visually
inspect the effect of choosing different models and parameter
configurations

``` r
demo()
```

![](man/figures/shinyApp.png)
