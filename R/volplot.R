#' Plot the estimated latent volatility process
#'
#' Displays the estimated latent volatility process over time.
#'
#' @param x A \code{stochvolTMB} object returned from \link{estimate_parameters}.
#' @param ... Currently not used.
#' @param include_ci Logical value indicating if volatility should be plotted
#'   with approximately 95\% confidence interval.
#' @param plot_log Logical value indicating if the estimated should be plotted
#'   on log or original scale. If \code{plot_log = TRUE} the process h is
#'   plotted. If \code{plot_log = FALSE} 100 \code{sigma_y} exp(\code{h} / 2) is
#'   plotted.
#' @param dates Vector of length ncol(x$nobs), providing optional dates for
#'   labeling the x-axis. The default value is NULL; in this case, the axis will
#'   be labeled with numbers.
#' @param forecast Integer specifying number of steps to forecast. 
#' @return ggplot object with plot of estimated estimated volatility.
#' @export
plot.stochvolTMB <- function(x, ..., 
                             include_ci = TRUE, 
                             plot_log = TRUE, 
                             dates = NULL, 
                             forecast = NULL
                             ) {
  
  parameter <- estimate <- type <- h_upper <- h_lower <- n <- std_error <- volatility <- NULL
  volatility_upper <- volatility_lower <- time <- quantile_0.025 <- quantile_0.975 <- NULL
  
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  
  if (!inherits(x, "stochvolTMB")) {
    stop("x has to be a stochvolTMB object")
  }
  
  if (!is.null(dates)) {
    if (length(dates) != x$nobs) {
      warning("dates is not same length as data and is ignored")
      dates <- NULL
    }
  }
  
  report <- summary(x)
    
  # Get sigma_y 
  sigma_y <- report[parameter == "sigma_y", estimate]

  report <- report[type == "random"][, ":=" (time = 1:.N, 
                                             h_upper = estimate + 2 * std_error,
                                             h_lower = estimate - 2 * std_error)]

  
  if (!is.null(dates)) report[, time := dates]
  
  if (!is.null(forecast)) {
    
    pred <- predict(x, steps = forecast, include_parameters = TRUE)
    pred_summary <- summary(pred, quantiles = c(0.025, 0.975), predict_mean = TRUE)
    
    last_date <- report[.N, time]
    lapply(pred_summary, function(x) x[, time := seq(last_date + 1, last_date + forecast, 1)])
  }
  
  # set theme 
  ggplot2::theme_set(ggplot2::theme_bw())
  
  if (plot_log) {
    
    p <- ggplot2::ggplot(report, ggplot2::aes(time, estimate)) +
      ggplot2::geom_line(size = 0.8, color = "black") + 
      ggplot2:: ggtitle("Estimated log volatility") + 
      ggplot2::xlab("") +
      ggplot2::ylab("")
    
    if (include_ci) {
      p <- p + 
        ggplot2::geom_line(ggplot2::aes(time, h_upper), color = "grey", size = 0.3) + 
        ggplot2::geom_line(ggplot2::aes(time, h_lower), color = "grey", size = 0.3) + 
        ggplot2::geom_ribbon(ggplot2::aes(time, ymax = h_upper, ymin = h_lower), fill = "grey", alpha = 0.1) + 
        ggplot2::ggtitle("Estimated log volatility with 95% confidence interval")
      
      
      if (!is.null(forecast)) {
        p <- p + ggplot2::geom_line(data = pred_summary$h, 
                                    ggplot2::aes(x = time, y = quantile_0.025), 
                                    size = 0.3, linetype = 2, col = "grey") + 
          ggplot2::geom_line(data = pred_summary$h, 
                             ggplot2::aes(x = time, y = quantile_0.975), size = 0.3, linetype = 2, col = "grey") + 
          ggplot2::geom_line(data = pred_summary$h, 
                           ggplot2::aes(x = time, y = mean), size = 0.8, linetype = 2, col = "black")
      }
    }
    
    if (!is.null(forecast)) {
      p <- plot_forecast(p, pred_summary$h, include_ci = include_ci)
    }
   
    
 
  } else {
    # Transform from log volatility
    report[, ":=" (volatility = sigma_y * exp(estimate / 2),
                   volatility_upper =  sigma_y * exp(h_upper / 2),
                   volatility_lower =  sigma_y * exp(h_lower / 2))]
    
    p <- ggplot2::ggplot(report, ggplot2::aes(time, volatility)) + ggplot2::geom_line(size = 0.8, color = "black") + 
      ggplot2::ggtitle("Estimated volatility") + 
      ggplot2::xlab("") +
      ggplot2::ylab("")
    
    if (include_ci) {
      p <- p + 
        ggplot2::geom_line(ggplot2::aes(time, volatility_upper), color = "grey", size = 0.3) + 
        ggplot2::geom_line(ggplot2::aes(time, volatility_lower), color = "grey", size = 0.3) + 
        ggplot2::geom_ribbon(ggplot2::aes(time, ymax = volatility_upper, ymin = volatility_lower), 
                             fill = "grey", alpha = 0.1) + 
        ggplot2::ggtitle("Estimated volatility with 95% confidence interval")
      
    } 
    
    if (!is.null(forecast)) {
      p <- plot_forecast(p, pred_summary$h_exp, include_ci = include_ci)
    }
    
  }
  
  return(p)
}

#' Add predicted volatility. 
#' 
#' Adds predicted volatility to the volatility plot. 
#' 
#' @param p ggplot object 
#' @param forecast data.table 
#' @param include_ci logical value indicating if volatility should be plotted
#'   with approximately 95\% confidence interval.#' @return ggplot object 
#' @keywords internal
#' @export

plot_forecast <- function(p, forecast, include_ci = TRUE) {
  
  time <- quantile_0.025 <- quantile_0.975 <- NULL
  
  p <-  p + 
   ggplot2::geom_line(data = forecast, ggplot2::aes(x = time, y = mean), size = 0.8, linetype = 2, col = "black")
 
 
 if (include_ci) {
  p <- p + 
    ggplot2::geom_line(data = forecast, ggplot2::aes(x = time, y = quantile_0.025), 
                       size = 0.8, linetype = 2, col = "grey") + 
    ggplot2::geom_line(data = forecast, ggplot2::aes(x = time, y = quantile_0.975),
                       size = 0.8, linetype = 2, col = "grey") 
 }
  
 return(p)
 
}
