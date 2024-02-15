library(shiny)
library(ggplot2)
library(latex2exp)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Logistic Curve"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "left",

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      numericInput(inputId = "beta0",
                  label = withMathJax("$$\\text{Intercept}\\hspace{.25cm}\\beta_0$$"),
                  min = -100,
                  max = 100,
                  value = 1),
      # Input: Slider for the number of bins ----
      numericInput(inputId = "beta1",
                  label = withMathJax("$$\\text{Slope}\\hspace{.25cm}\\beta_1$$"),
                  min = -100,
                  max = 100,
                  value = 1),
      numericInput(inputId = "x1",
                   label = withMathJax("$$\\text{Value for }X_1$$"),
                   min = -100,
                   max = 100,
                   value = 1),
      numericInput(inputId = "beta2",
                   label = withMathJax("$$\\text{Slope}\\hspace{.25cm}\\beta_2$$"),
                   min = -100,
                   max = 100,
                   value = 1),
      numericInput(inputId = "x2",
                   label = withMathJax("$$\\text{Value for }X_2$$"),
                   min = -100,
                   max = 100,
                   value = 1),

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("X1", fluidRow(
          splitLayout(cellWidths = c("45%", "45%"), plotOutput(outputId = "distPlot1"), plotOutput(outputId = "logOdds1"))
        )),
        tabPanel("X2", fluidRow(
          splitLayout(cellWidths = c("45%", "45%"), plotOutput(outputId = "distPlot2"), plotOutput(outputId = "logOdds2"))
        ))
      ),
      
      fluidRow(
        splitLayout(cellwidts = c("50%", "50%"), withMathJax("$$\\text{Logit: }\\frac{\\pi}{1-\\pi}=\\beta_0+\\beta_1 x_1 + \\beta_2 x_2$$"), textOutput(outputId = "logOddsValue") )
      ),
      fluidRow(
        splitLayout(cellwidths = c("50%", "50%"), withMathJax("$$\\text{Probability: }\\pi = \\frac{e^{\\beta_0+\\beta_1 x+ \\beta_2 x_2}}{1+e^{\\beta_0+\\beta_1 x+ \\beta_2 x_2}}$$"), textOutput(outputId = "probabilityValue"))
        
      )
      
    ),
  ),

)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$distPlot1 <- renderPlot({
    center <- -1*input$beta0/input$beta1
    
    
    x_values <- seq(center - 5, center + 5, length.out = 100)
    
    # Logistic function formula
    logistic_function <- function(x, a, c) {
      y <- 1 / (1 + exp(-(a * (x) + c)))
      return(y)
    }
    
    y_values <- logistic_function(x_values, input$beta1, input$beta0)

    plot_data <- data.frame(x = x_values, y = y_values)
    ggplot(plot_data, aes(x, y)) +
      geom_line() +
      labs(title = "Probability Curve",
           x = "x1",
           y = expression(pi))

  })
  
  output$logOdds1 <- renderPlot({
    center <- -1*input$beta0/input$beta1
    
    x_values <- seq(center - 5, center + 5, length.out = 100)
    
    logistic_function <- function(x, a, c) {
      y <- a * (x) + c
      return(y)
    }
    y_values <- logistic_function(x_values, input$beta1, input$beta0)
    
    plot_data <- data.frame(x = x_values, y = y_values)
    ggplot(plot_data, aes(x, y)) +
      geom_line() +
      labs(title = "Log Odds Curve",
           x = "x1",
           y = TeX("$log\\frac{\\pi}{1-\\pi}$")) +
      ylim(-30, 30)
  })
  
  output$distPlot2 <- renderPlot({
    center <- -1*input$beta0/input$beta2
    
    
    x_values <- seq(center - 5, center + 5, length.out = 100)
    
    # Logistic function formula
    logistic_function <- function(x, a, c) {
      y <- 1 / (1 + exp(-(a * (x) + c)))
      return(y)
    }
    
    y_values <- logistic_function(x_values, input$beta2, input$beta0)
    
    plot_data <- data.frame(x = x_values, y = y_values)
    ggplot(plot_data, aes(x, y)) +
      geom_line() +
      labs(title = "Probability Curve",
           x = "x2",
           y = expression(pi))
    
  })
  
  output$logOdds2 <- renderPlot({
    center <- -1*input$beta0/input$beta2
    
    x_values <- seq(center - 5, center + 5, length.out = 100)
    
    logistic_function <- function(x, a, c) {
      y <- a * (x) + c
      return(y)
    }
    y_values <- logistic_function(x_values, input$beta2, input$beta0)
    
    plot_data <- data.frame(x = x_values, y = y_values)
    ggplot(plot_data, aes(x, y)) +
      geom_line() +
      labs(title = "Log Odds Curve",
           x = "x2",
           y = TeX("$log\\frac{\\pi}{1-\\pi}$")) +
      ylim(-30, 30)
  })
  
  output$logOddsValue <- renderText({
    
    x1_input <- input$x1
    x2_input <- input$x2
    beta0 <- input$beta0
    beta1 <- input$beta1
    beta2 <- input$beta2
    
    
    log_odds <- beta0 + beta1 * x1_input + beta2 * x2_input
    paste("Log Odds:", log_odds)
  })
  
  output$probabilityValue <- renderText({
    
    x1_input <- input$x1
    x2_input <- input$x2
    beta0 <- input$beta0
    beta1 <- input$beta1
    beta2 <- input$beta2
    
    probability <- exp(beta0 + beta1 * x1_input + beta2 * x2_input) / (1 + exp(beta0 + beta1 * x1_input + beta2 * x2_input))
    paste("Probability:", probability)
  })

}
shinyApp(ui = ui, server = server)

