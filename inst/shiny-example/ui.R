#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stochvolTMB)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("stochvolTMB"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("model", "Select model",
                        choices = c("Gaussian" = "gaussian",
                                       "Student-t" = "t",
                                       "Skew-Gaussian" = "skew_gaussian",
                                       "Gaussian leverage" = "leverage")),
            h4("Set parameter values"),
            sliderInput("phi", withMathJax("Dispersion parameter \\(\\phi\\)"), -0.99, 0.99, 0.9, step = 0.05),
            sliderInput("sigma_y", "Observational error \\(\\sigma_y\\)", -5, 5, 0.2, step = 0.1),
            sliderInput("sigma_h", "State error \\(\\sigma_h\\)", -5, 5, 0.4, step = 0.1),
            sliderInput("nu", "degrees of freedom \\(\\nu\\) (only for Student-t)", 3, 20, 5, step = 1),
            sliderInput("alpha", "Skewness parameter \\(\\alpha\\) (only for Skew-Gaussian)", -5, 5, -1, step = 0.1),
            sliderInput("rho", "Leverage parameter \\(\\rho\\) (only for Gaussian leverage)", -0.99, 0.99, -0.6, step = 0.05),
            sliderInput("nobs", "Number of observations", 100, 10000, 3000, step = 100)
         ),
        

        # Show a plot of the generated distribution
        mainPanel(
            checkboxGroupInput("plot_settings", "Plot settings",
                               c("Plot with 95% confindence intervall" = "include_ci",
                                 "Plot log volatility" = "plot_log"),
                               inline = TRUE),
            #h3("Simulated data"),
            plotOutput("sim_y"),
            plotOutput("sim_h"),
            #br(),
            shinycssloaders::withSpinner(plotOutput("estPlot"), 8),
            #h3("Estimated process")
            plotOutput("est_vs_sim")
            
        )
    )
))
