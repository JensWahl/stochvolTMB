#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(stochvolTMB)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    data <- reactive({
        
        param <- list(phi = input$phi,
                      sigma_h = input$sigma_h,
                      sigma_y = input$sigma_y, 
                      df = input$nu,
                      alpha = input$alpha,
                      rho = input$rho)
        
        # Simulate data 
        data_sim <- sim_sv(param = param, N = input$nobs, model = input$model)
        data_sim$t <- 1:input$nobs
        
        return(data_sim)
    })
    

    estimate <- reactive({
        # Estimate parameters
        estimate <- estimate_parameters(data = data()$y, model = input$model)

    })
        
    #})

    output$simPlot <- renderPlot({

        #if(is.null(values$data_sim)) return(NULL)

        p1 <- ggplot(data(), aes(x = t, y = y)) +
            geom_line() +
            labs(title = "Observations",
                 xlab = "",
                 ylab = "y")

        p2 <- ggplot(data(), aes(x = t, y = h)) +
            geom_line() +
            labs(title = "Latent process",
                 xlab = "",
                 ylab = "h")

        grid.arrange(p1, p2, nrow = 2)


    })
    
    output$estPlot <- renderPlot({

        #if(is.null(values$estimates)) return(NULL)
        if("plot_log" %in% input$plot_settings) plot_log <- TRUE else plot_log <- FALSE
        if("include_ci" %in% input$plot_settings) include_ci <- TRUE else include_ci <- FALSE

        plot(estimate(), include_ci = include_ci, plot_log = plot_log)


    })

    output$est_vs_sim <- renderPlot({

        #$if(is.null(values$estimates)) return(NULL)
        h_est <- summary(estimate(), report = "random")
        
        data <- data() %>%
            transmute(estimated = h_est$estimate,
                   true = h,
                   t = 1:n()) %>%
            gather(key, value, -t)


        ggplot(data) +
            geom_line(aes(t, value, color = key)) +
            labs(title = "Estimated vs true latent process",
                 xlab = "",
                 ylab = "h") +
            theme(legend.position="bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 15))


    })
    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # 
    # })

})
