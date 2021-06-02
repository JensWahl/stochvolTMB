
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stochvolTMB

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/stochvolTMB)](https://cran.r-project.org/package=stochvolTMB)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/stochvolTMB?color=blue)](https://r-pkg.org/pkg/stochvolTMB)
[![R build
status](https://github.com/JensWahl/stochvolTMB/workflows/R-CMD-check/badge.svg)](https://github.com/JensWahl/stochvolTMB/actions)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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

To install the current stable release from CRAN, use

``` r
install.packages("stochvolTMB")
```

To install the current development version, use

``` r
# install.packages("remotes")
remotes::install_github("JensWahl/stochvolTMB")
```

If you would also like to build and view the vignette locally, use

``` r
remotes::install_github("JensWahl/stochvolTMB", dependencies = TRUE, build_vignettes = TRUE)
```

## Example

The main function for estimating parameters is `estimate_parameters`:

``` r
library(stochvolTMB, warn.conflicts = FALSE)

# load s&p500 data from 2005 to 2018
data(spy)

# find the best model using AIC 
gaussian <- estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)
t_dist <- estimate_parameters(spy$log_return, model = "t", silent = TRUE)
skew_gaussian <- estimate_parameters(spy$log_return, model = "skew_gaussian", silent = TRUE)
leverage <- estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)

# the leverage model stands out with an AIC far below the other models
AIC(gaussian, t_dist, skew_gaussian, leverage)
#>               df       AIC
#> gaussian       3 -23430.57
#> t_dist         4 -23451.69
#> skew_gaussian  4 -23440.87
#> leverage       4 -23608.85

# get parameter estimates with standard error
estimates <- summary(leverage)
head(estimates, 10)
#>       parameter     estimate    std_error     z_value       p_value        type
#>  1:     sigma_y  0.008338412 0.0004163314  20.0283029  3.121144e-89 transformed
#>  2:     sigma_h  0.273443559 0.0182641070  14.9716359  1.125191e-50 transformed
#>  3:         phi  0.967721215 0.0043681868 221.5384240  0.000000e+00 transformed
#>  4:         rho -0.748695259 0.0322487815 -23.2162340 3.121690e-119 transformed
#>  5: log_sigma_y -4.786882463 0.0499293427 -95.8731319  0.000000e+00       fixed
#>  6: log_sigma_h -1.296660043 0.0667929683 -19.4131220  5.978190e-84       fixed
#>  7:   logit_phi  4.110221202 0.1375467861  29.8823500 3.337032e-196       fixed
#>  8:   logit_rho -1.939958912 0.1467670249 -13.2179481  6.912403e-40       fixed
#>  9:           h -0.536254072 0.5182192669  -1.0348015  3.007616e-01      random
#> 10:           h -0.207811236 0.4245258952  -0.4895137  6.244781e-01      random

# plot estimated volatility with 95 % confidence interval
plot(leverage, include_ci = TRUE, dates = spy$date)
```

<img src="man/figures/README-example-1.png" width="100%" />

Given the estimated parameters we can simulate future volatility and
log-returns using `predict`.

``` r
# simulate future prices with or without parameter uncertainty
pred = predict(leverage, steps = 10)

# Calculate the mean, 2.5% and 97.5% quantiles from the simulations
pred_summary = summary(pred, quantiles = c(0.025, 0.975), predict_mean = TRUE)

print(pred_summary)
#> $y
#>     time quantile_0.025 quantile_0.975          mean
#>  1:    1    -0.03530596     0.03757225  3.300051e-04
#>  2:    2    -0.03767249     0.03780191  8.017874e-06
#>  3:    3    -0.03712980     0.03661862 -5.433106e-05
#>  4:    4    -0.03640837     0.03654651 -7.468461e-05
#>  5:    5    -0.03703312     0.03599856 -7.068530e-05
#>  6:    6    -0.03645971     0.03538847 -1.403050e-04
#>  7:    7    -0.03529392     0.03606813 -2.398350e-05
#>  8:    8    -0.03555084     0.03557015  1.071788e-04
#>  9:    9    -0.03534408     0.03496998 -3.276668e-04
#> 10:   10    -0.03519005     0.03556894 -2.169624e-05
#> 
#> $h
#>     time quantile_0.025 quantile_0.975     mean
#>  1:    1    0.405236349       2.480368 1.443921
#>  2:    2    0.269916662       2.512637 1.394228
#>  3:    3    0.125157945       2.542889 1.349591
#>  4:    4   -0.001972121       2.577818 1.305138
#>  5:    5   -0.121634732       2.619858 1.264353
#>  6:    6   -0.198890223       2.648988 1.222401
#>  7:    7   -0.303325082       2.661555 1.185383
#>  8:    8   -0.394986458       2.684556 1.148738
#>  9:    9   -0.460562584       2.690121 1.112372
#> 10:   10   -0.541738912       2.696165 1.081266
#> 
#> $h_exp
#>     time quantile_0.025 quantile_0.975       mean
#>  1:    1    0.010031249     0.02899963 0.01777729
#>  2:    2    0.009438124     0.02959231 0.01746646
#>  3:    3    0.008884923     0.02984871 0.01718800
#>  4:    4    0.008293327     0.03067192 0.01694098
#>  5:    5    0.007875292     0.03116576 0.01672356
#>  6:    6    0.007534590     0.03135049 0.01642636
#>  7:    7    0.007113160     0.03177229 0.01622501
#>  8:    8    0.006840445     0.03203538 0.01599191
#>  9:    9    0.006597910     0.03216773 0.01576753
#> 10:   10    0.006292585     0.03234347 0.01559918

# plot predicted volatility with 0.025 and 0.975 quantiles
plot(leverage, include_ci = TRUE, forecast = 50, dates = spy$d) +
  ggplot2::xlim(c(spy[.N, date] - 150, spy[.N, date] + 50))
#> Warning: Removed 3419 row(s) containing missing values (geom_path).

#> Warning: Removed 3419 row(s) containing missing values (geom_path).

#> Warning: Removed 3419 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Shiny app

By running `demo()` you start a shiny application where you can visually
inspect the effect of choosing different models and parameter
configurations

``` r
demo()
```

![](man/figures/shinyApp.png)
