# Simulate from the asymptotic distribution of the parameter estimates

Sampling is done on the scale the parameters were estimated. The
standard deviations are simulated on log-scale and the persistence is
simulated on logit scale. The same is true for the correlation parameter
in the leverage model.

## Usage

``` r
simulate_parameters(object, nsim = 1000)
```

## Arguments

- object:

  A `stochvolTMB` object.

- nsim:

  Number of simulations.

## Value

matrix of simulated values.
