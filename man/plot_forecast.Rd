% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volplot.R
\name{plot_forecast}
\alias{plot_forecast}
\title{Add predicted volatility.}
\usage{
plot_forecast(p, forecast, include_ci = TRUE)
}
\arguments{
\item{p}{ggplot object}

\item{forecast}{data.table}

\item{include_ci}{logical value indicating if volatility should be plotted
with approximately 95\% confidence interval.#' @return ggplot object}
}
\description{
Adds predicted volatility to the volatility plot.
}
\keyword{internal}
