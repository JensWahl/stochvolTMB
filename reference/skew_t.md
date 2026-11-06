# Skew t distribution of Jones and Faddy

Density, distribution function, quantile function and random generation
for the skew t distribution of Jones and Faddy (2003), standardized to
mean zero and unit variance.

## Usage

``` r
dskew_t(x, df_left, df_right, log = FALSE)

pskew_t(q, df_left, df_right)

qskew_t(p, df_left, df_right)

rskew_t(n, df_left, df_right)
```

## Arguments

- x, q:

  Vector of quantiles.

- df_left, df_right:

  Degrees of freedom of the left and the right tail, greater than 2.

- log:

  Logical; if `TRUE`, the log density is returned.

- p:

  Vector of probabilities.

- n:

  Number of draws.

## Value

Numeric vector.

## Details

Before standardization, the density with parameters \\a, b \> 0\\ is
\$\$f(t) = C \left(1 + \frac{t}{\sqrt{a + b + t^2}}\right)^{a + 1/2}
\left(1 - \frac{t}{\sqrt{a + b + t^2}}\right)^{b + 1/2}, \qquad C =
\frac{1}{2^{a + b - 1} B(a, b) \sqrt{a + b}}.\$\$ The left tail decays
like a Student t with \\2a\\ degrees of freedom and the right tail like
a Student t with \\2b\\ degrees of freedom. The functions use
`df_left = 2a` and `df_right = 2b`. If they are equal, the distribution
is the Student t distribution scaled to unit variance. If
`df_left < df_right`, the left tail is heavier and the distribution is
skewed to the left. Both must be greater than 2 for the variance to
exist.

The density is smooth everywhere. This is important for the Laplace
approximation, which uses the second derivative of the log density.

## References

Jones, M. C. and Faddy, M. J. (2003). A skew extension of the
t-distribution, with applications. Journal of the Royal Statistical
Society: Series B, 65(1), 159-174.
[doi:10.1111/1467-9868.00378](https://doi.org/10.1111/1467-9868.00378)
