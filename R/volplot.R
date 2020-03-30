#' Plot estimated latent volatility process
#' 
#' Displays the estimated latent volatility process over time. 
#' 
#' @param x A \code{stochvolTMB} object returned form \link{estimate_parameters}.
#' @param ... Currently not used. 
#' @param include_ci logical value indicating if volatility should be plotted with approximately 95 \% confidence intervall. 
#' @param plot_log logical value indicating if the estimated should be plotted on log or original scale. If \code{plot_log = TRUE} the process h is plottet. 
#' If \code{plot_log = FALSE} 100 \code{sigma_y} exp(\code{h} / 2) is plotted. 
#' @return ggplot object with plot of estimated estimated volatility.
#' @export 
plot.stochvolTMB <- function(x, ..., include_ci = TRUE, plot_log = TRUE){
  
  parameter <- estimate <- type <- h_upper <- h_lower <- n <- std_error <- volatility <- NULL
  volatility_upper <- volatility_lower <- time <- NULL
  
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  
  if(!inherits(x, "stochvolTMB")){
    stop("x has to be a stochvolTMB object")
  }
  
  report <- summary(x)
    
  # Get sigma_y 
  sigma_y <- report %>% 
    dplyr::filter(parameter == "sigma_y") %>% 
    dplyr::pull(estimate)
  
  report <- report %>% 
    dplyr::filter(type == "random") %>% 
    dplyr::mutate(time = 1:dplyr::n(),
                  h_upper = estimate + 2 * std_error,
                  h_lower = estimate - 2 * std_error)
  
  # set theme 
  ggplot2::theme_set(ggplot2::theme_bw())
  
  if(plot_log){
    
    p <- ggplot2::ggplot(report, ggplot2::aes(time, estimate)) +
      ggplot2::geom_line(size = 0.8, color = "black") + 
      ggplot2:: ggtitle("Estimated log volatility") + 
      ggplot2::xlab("") +
      ggplot2::ylab("")
    
    if(include_ci){
      p <- p + 
        ggplot2::geom_line(ggplot2::aes(time, h_upper), color = "grey", size = 0.3) + 
        ggplot2::geom_line(ggplot2::aes(time, h_lower), color = "grey", size = 0.3) + 
        ggplot2::geom_ribbon(ggplot2::aes(time, ymax = h_upper, ymin = h_lower), fill = "grey", alpha= 0.1) + 
        ggplot2::ggtitle("Estimated log volatility with 95 % confidence intervall")
      
    } 
  }else {
    # Transform from log volatility
    report <- report %>%
      dplyr::mutate(volatility = 100 * sigma_y * exp(estimate / 2),
                    volatility_upper =  100 * sigma_y * exp(h_upper / 2),
                    volatility_lower =  100 * sigma_y * exp(h_lower / 2))
    
    p <- ggplot2::ggplot(report, ggplot2::aes(time, volatility)) + ggplot2::geom_line(size = 0.8, color = "black") + 
      ggplot2::ggtitle("Estimated volatility") + 
      ggplot2::xlab("") +
      ggplot2::ylab("")
    
    if(include_ci){
      p <- p + 
        ggplot2::geom_line(ggplot2::aes(time, volatility_upper), color = "grey", size = 0.3) + 
        ggplot2::geom_line(ggplot2::aes(time, volatility_lower), color = "grey", size = 0.3) + 
        ggplot2::geom_ribbon(ggplot2::aes(time, ymax = volatility_upper, ymin = volatility_lower), fill = "grey", alpha= 0.1) + 
        ggplot2::ggtitle("Estimated volatility with 95 % confidence intervall")
      
    } 
  }
  
  return(p)
  
  
}