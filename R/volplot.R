#' Function for plotting log volatiliy
#' @param report tibble returned from optSV
#' @param include_ci logical. Include +- two times standard error
#' @param plot_log logical. Plot estimated log volatility. If false, exp(h / 2) is plotted.
#' @return ggplot object
#' @export
volplot <- function(report, include_ci = TRUE, plot_log = TRUE){
  
  stopifnot(is.data.frame(report))
  
  # Get sigma_y 
  sigma_y <- report %>% 
    dplyr::filter(parameter == "sigma_y") %>% 
    dplyr::select(estimate) %>%
    as.numeric()
  
  report <- report %>% 
    dplyr::filter(type == "random") %>% 
    dplyr::mutate(time = 1:n(),
           h_upper = estimate + 2 * std_error,
           h_lower = estimate - 2 * std_error)
  
  # set theme 
  ggplot2::theme_set(ggplot2::theme_bw())
  
  if(plot_log){
    
    p <- ggplot2::ggplot(report, aes(time, estimate)) + geom_line(size = 0.5, color = "black") + 
      ggplot2:: ggtitle("Estimated log volatility") + 
      ggplot2::xlab("") +
      ggplot2::ylab("")
    
    if(include_ci){
      p <- p + 
        ggplot2::geom_line(aes(time, h_upper), color = "grey", size = 0.3) + 
        ggplot2::geom_line(aes(time, h_lower), color = "grey", size = 0.3) + 
        ggplot2::geom_ribbon(aes(time, ymax = h_upper, ymin = h_lower), fill = "grey", alpha= 0.1) + 
        ggplot2::ggtitle("Estimated log volatility with 95 % confidence intervall")
        
    } 
  }else {
    # Transform from log volatility
    report <- report %>%
      dplyr::mutate(volatility = sigma_y * exp(estimate),
             volatility_upper =  sigma_y * exp(h_upper),
             volatility_lower =  sigma_y * exp(h_lower))
    
    p <- ggplot2::ggplot(report, aes(time, volatility)) + geom_line(size = 0.5, color = "black") + 
      ggplot2::ggtitle("Estimated volatility") + 
      ggplot2::xlab("") +
      ggplot2::ylab("")
    
    if(include_ci){
      p <- p + 
        ggplot2::geom_line(aes(time, volatility_upper), color = "grey", size = 0.3) + 
        ggplot2::geom_line(aes(time, volatility_lower), color = "grey", size = 0.3) + 
        ggplot2::geom_ribbon(aes(time, ymax = volatility_upper, ymin = volatility_lower), fill = "grey", alpha= 0.1) + 
        ggplot2::ggtitle("Estimated volatility with 95 % confidence intervall")
      
    } 
  }
  
  return(p)
  
  
}