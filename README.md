
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stochvolTMB

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/stochvolTMB)](https://cran.r-project.org/package=stochvolTMB)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/stochvolTMB?color=blue)](https://r-pkg.org/pkg/stochvolTMB)
[![R
buildstatus](https://github.com/JensWahl/stochvolTMB/workflows/R-CMD-check/badge.svg)](https://github.com/JensWahl/stochvolTMB/actions)
[![License:
GPLv3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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
#>  1:    1    -0.03811074     0.03725858 -1.605944e-04
#>  2:    2    -0.03613254     0.03807427  3.905524e-04
#>  3:    3    -0.03689599     0.03697331 -1.170203e-04
#>  4:    4    -0.03578552     0.03655235  1.860492e-04
#>  5:    5    -0.03547748     0.03626548  1.165658e-04
#>  6:    6    -0.03661546     0.03555252 -1.898271e-04
#>  7:    7    -0.03495112     0.03523121  1.582960e-04
#>  8:    8    -0.03567302     0.03514370  3.110404e-05
#>  9:    9    -0.03521115     0.03470416 -3.077596e-05
#> 10:   10    -0.03391924     0.03442448 -3.941700e-05
#> 
#> $h
#>     time quantile_0.025 quantile_0.975     mean
#>  1:    1    0.411180931       2.509780 1.450119
#>  2:    2    0.255810550       2.557694 1.401863
#>  3:    3    0.104986137       2.586031 1.354629
#>  4:    4    0.004664263       2.619603 1.310351
#>  5:    5   -0.120600217       2.660227 1.268050
#>  6:    6   -0.209691395       2.670629 1.227245
#>  7:    7   -0.306100384       2.657083 1.189820
#>  8:    8   -0.392420025       2.711739 1.153733
#>  9:    9   -0.469962600       2.714449 1.115334
#> 10:   10   -0.537930702       2.720761 1.079353
#> 
#> $h_exp
#>     time quantile_0.025 quantile_0.975       mean
#>  1:    1    0.010082253     0.02956799 0.01790438
#>  2:    2    0.009515924     0.03051188 0.01757710
#>  3:    3    0.008796049     0.03044210 0.01725803
#>  4:    4    0.008247518     0.03108103 0.01699651
#>  5:    5    0.007821668     0.03170945 0.01673759
#>  6:    6    0.007420323     0.03200784 0.01647688
#>  7:    7    0.007167451     0.03213425 0.01628790
#>  8:    8    0.006796787     0.03246457 0.01610369
#>  9:    9    0.006584324     0.03277949 0.01578397
#> 10:   10    0.006352601     0.03281460 0.01566125

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
