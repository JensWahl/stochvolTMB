# Simulate log-returns from a stochastic volatility model

This function draws the initial log-volatility (`h_t`) from its
stationary distribution, meaning that `h_0` is drawn from a gaussian
distribution with mean zero and standard deviation `sigma_h` /
`sqrt(1 - phi^2)`. `h_{t+1}` is then simulated from its conditional
distribution given `h_t`, which is N(`phi*h_t`, `sigma_h`). Log-returns
(`y_t`) is simulated from its conditional distribution given the latent
process `h`. If `model` = "gaussian", then `y_t` given `h_t` is gaussian
with mean zero and standard deviation equal to `sigma_y*exp(h_t / 2)`.
Heavy tail returns can be obtained by simulating from the t-distribution
by setting `model` = "t". How heavy of a tail is specified by the degree
of freedom parameter `df`. Note that the observations are scaled by
`sqrt((df-2)/2)` so that the error term has variance equal to one.
Asymmetric returns are obtained from the "skew_gaussian" model. How
asymmetric is governed by the skewness parameter `alpha`. The so called
leverage model, where we allow for correlation between log-returns and
volatility can be simulated by setting `model` to "leverage" and
specifying the correlation parameter `rho`. In the
"skew_gaussian_leverage" model the return shock `z_t` is skew gaussian
with mean zero, unit variance and shape `alpha`, and it drives the
volatility shock:
`h_{t+1} = phi * h_t + sigma_h * (rho * z_t + sqrt(1 - rho^2) * e_t)`,
where `e_t` is standard gaussian.

## Usage

``` r
sim_sv(
  param = list(phi = 0.9, sigma_y = 0.4, sigma_h = 0.2, df = 4, alpha = -2, rho = -0.7),
  nobs = 1000L,
  seed = NULL,
  model = "gaussian"
)
```

## Arguments

- param:

  List of parameters. This includes the standard deviation of the
  observations, `sigma_y`, the standard deviation of the latent
  volatility process, `sigma_h`, the persistence parameter `phi`. If
  `model` = "t", the degree of freedom `df` must be specified. If
  `model` = "skew_gaussian", the shape parameter `alpha` must be
  specified (see the section on skew gaussian models in
  [`estimate_parameters`](estimate_parameters.md) for how `alpha`
  relates to the skewness) and if `model` = "leverage", the correlation
  `rho` between the latent error term and the observational error has to
  be specified.

- nobs:

  Length of time series.

- seed:

  Seed to reproduce simulation.

- model:

  Distribution of error term.

## Value

data.table with columns `y` (observations) and `h` (latent
log-volatility).
