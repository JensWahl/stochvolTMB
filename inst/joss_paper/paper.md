---
title: 'stochvolTMB: An R-package for likelihood estimation of stochastic volatility models'
tags:
- R
- maximum likelihood
- laplace approximation
- volatility
- finance
date: "8 December 2020"
output: rticles::joss_article
authors:
- name: Jens Christian Wahl
  orcid: 0000-0002-3812-5111
  affiliation: 1
affiliations:
 - name: Norwegian Computing Center
   index: 1
year: 2020
formatted_doi: XX.XXXXX/joss.XXXXX
bibliography: paper.bib
citation_author: Wahl
journal: JOSS
---


# Summary

Stochastic volatility (SV) models are often used to model financial returns that exhibit time-varying and autocorrelated 
variance. The first SV model was introduced by @Taylor1982 and models the logarithm of the variance as a latent autoregressive process of 
order one. Parameter estimation of stochastic volatility models can be challenging and a variety of methods have been 
proposed, such as simulated likelihood [@Liesenfeld2006], quasi-maximum likelihood [@Harvey1994] and 
Markov Chain Monte Carlo methods (MCMC) [@Shepard1998; @Kastner2016]. `stochvolTMB` estimates
the parameters using maximum likelihood, similar to @Skaug2014. The latent variables are integrated out using Laplace approximation. 
The models are implemented in `C++` using the `R`-package [@rCore] `TMB` [@TMB2016] for fast and efficient estimation. `TMB` utilizes 
the `Eigen` library [@Eigen2010] for numerical linear algebra and `CppAD` [@CppAD2005] for automatic differentiation of 
the negative log-likelihood. 


# Statement of need
The `stochvolTMB` `R`-package makes it easy for users to do inference, plotting and forecasting of volatility. The `R`-package `stochvol` [@Kastner2016] also performs inference for stochastic volatility models, but differs from `stochvolTMB` since it performs Bayesian inference using MCMC and not maximum likelihood. 
By using optimization instead of simulations one can obtain substantial speed up depending on the data, model, number of observations and number of MCMC samples.  


# Implementation

`stochvolTMB` implements stochastic volatility models of the form

\begin{equation}
    \begin{aligned}
        y_t &= \sigma_y e^{h_t/2} \epsilon_t, \quad t = 1, \dots, T, \\
        h_{t+1} &= \phi h_{t} + \sigma_h \eta_t, \quad t = 1, \dots, T-1, \\
        \eta_t &\stackrel{\text{iid}}{\sim} \mathcal{N}(0,1), \\
        \epsilon_t &\stackrel{\text{iid}} {\sim}  F, \\
        h_1 &\sim \mathcal{N} \bigg (0, \frac{\sigma_h}{\sqrt{(1 - \phi^2)}} \bigg ),
    \end{aligned}
\end{equation}

where $y_t$ is the observed log return for day $t$, $h_t$ is the logarithm of the conditional variance of day $t$ and is modelled as an AR(1) process, $\boldsymbol{\theta} = (\phi, \sigma_y, \sigma_h)$ is a vector of the fixed parameters and $F$ denotes the distribution of $\epsilon_t$. 
Four distributions are implemented for $\epsilon_t$: (1) The standard Gaussian distribution; (2) The $t$-distribution with $\nu$ degrees of freedom; 
(3) The skew-Gaussian distribution with skewness parameter $\alpha$; and (4) The leverage model where $(\epsilon_t, \eta_t)$ are both standard Gaussian with correlation
coefficient $\rho$. The last three distributions add an additional fixed parameter to $\boldsymbol{\theta}$. `stochvolTMB` also supports generic functions such as `plot`, `summary`, `predict` and `AIC`. The plotting is 
implemented using `ggplot2` [@ggplot2] and data processing utilizes the `R`-package `data.table` [@datatableRpackage]. 

The parameter estimation is done in an iterative two-step procedure: (1) Optimize the joint negative log-likelihood 
with respect to the latent log-volatility $\boldsymbol{h} = (h_1, \ldots, h_T)$ holding $\boldsymbol{\theta}$ fixed, and (2) Optimize 
the Laplace approximation of the joint negative log-likelihood w.r.t $\boldsymbol{\theta}$ holding $\boldsymbol{h}$ fixed. This procedure is iterated until convergence. 
Standard deviations for the log-volatility and the fixed parameters are obtained using a generalized delta-method [@TMB2016].


# Example 

As an example we compare the different models on log returns for the S&P index from 2005 to 2018:


```r
library(stochvolTMB)
data(spy)
gaussian = estimate_parameters(spy$log_return, model = "gaussian")
t_dist = estimate_parameters(spy$log_return, model = "t")
skew_gaussian = estimate_parameters(spy$log_return, model = "skew_gaussian")
leverage = estimate_parameters(spy$log_return, model = "leverage")
```

To compare competing models we can use model selection tools such as AIC (@akaike1998):


```r
AIC(gaussian,
    t_dist,
    skew_gaussian,
    leverage)
```

```
##               df       AIC
## gaussian       3 -23430.57
## t_dist         4 -23451.69
## skew_gaussian  4 -23440.87
## leverage       4 -23608.85
```

The leverage model is preferred in this example. Notice that the Gaussian model performs the worst and shows the
importance of having more flexible distributions, even after controlling for the volatility. We can plot the estimated log-volatility with 95% confidence interval


```r
plot(leverage, plot_log = FALSE, dates = spy$date)
```

![](paper_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

Future volatility can be simulated from the estimated model. Parameter uncertainty of the fixed effects is by default included and is obtained by simulating parameter values from the asymptotic distribution, i.e. a multivariate Gaussian distribution using the observed Fisher information matrix (inverse Hessian of the negative log-likelihood) as the covariance matrix. 


```r
set.seed(123)
# plot predicted volatility with 95% confidence interval
plot(leverage, plot_log = FALSE, forecast = 50, dates = spy$date) +
  ggplot2::xlim(c(tail(spy$date, 1) - 150, tail(spy$date, 1) + 50))
```

![](paper_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

```r
prediction = predict(leverage, steps = 5)
summary(prediction, quantiles = c(0.025, 0.975))
```

```
## $y
##    time quantile_0.025 quantile_0.975          mean
## 1:    1    -0.03663416     0.03752972  1.102221e-04
## 2:    2    -0.03666747     0.03596978 -2.848990e-05
## 3:    3    -0.03602083     0.03644617  8.530205e-05
## 4:    4    -0.03780105     0.03685205 -4.208489e-05
## 5:    5    -0.03651973     0.03629376  7.000557e-05
## 
## $h
##    time quantile_0.025 quantile_0.975     mean
## 1:    1    0.404072293       2.487583 1.442077
## 2:    2    0.268454334       2.533374 1.394570
## 3:    3    0.121924793       2.589508 1.347677
## 4:    4   -0.009923701       2.610516 1.304630
## 5:    5   -0.134679921       2.654572 1.259413
## 
## $h_exp
##    time quantile_0.025 quantile_0.975       mean
## 1:    1    0.010053253     0.02915122 0.01776023
## 2:    2    0.009335712     0.02994720 0.01749173
## 3:    3    0.008764370     0.03068317 0.01724951
## 4:    4    0.008291696     0.03101963 0.01698012
## 5:    5    0.007747370     0.03151893 0.01668318
```





# References
