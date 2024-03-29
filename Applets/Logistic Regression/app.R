library(shiny)
library(ggplot2)
library(latex2exp)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Logistic Curve"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "right",
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Slider for the number of bins ----
                  numericInput(inputId = "beta0",
                               label = TeX("$\\text{Intercept}\\hspace{.25cm}\\beta_0$"),
                               min = -100,
                               max = 100,
                               value = 1),
                  # Input: Slider for the number of bins ----
                  numericInput(inputId = "beta1",
                               label = TeX("$\\text{Slope}\\hspace{.25cm}\\beta_1$"),
                               min = -100,
                               max = 100,
                               value = 1),
                  numericInput(inputId = "x1",
                               label = TeX("$\\text{Value for }X_1$"),
                               min = -100,
                               max = 100,
                               value = 1),
                  
                ),
                #Logit Form ----
                mainPanel(TeX("$\\text{Logit Form: }\\frac{\\pi}{1-\\pi}=\\beta_0+\\beta_1 x \\hspace{1cm} \\text{Probability Form: }\\pi = \\frac{e^{\\beta_0+\\beta_1 x}}{1+e^{\\beta_0+\\beta_1 x}}$")),
                
  ),
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Histogram ----
    plotOutput(outputId = "distPlot"),
    
    plotOutput(outputId = "logOdds"),
    textOutput(outputId = "logOddsValue"),
    textOutput(outputId = "probabilityValue")
    
  )
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
  
  output$distPlot <- renderPlot({
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
           x = "x",
           y = expression(pi))
    
  })
  
  output$logOdds <- renderPlot({
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
           x = "x",
           y = TeX("$log\\frac{\\pi}{1-\\pi}$")) +
      ylim(-30, 30)
  })
  
  output$logOddsValue <- renderText({
    center <- -1*input$beta0/input$beta1
    
    x_input <- input$x1
    beta0 <- input$beta0
    beta1 <- input$beta1
    
    log_odds <- beta0 + beta1 * x_input
    paste("Log Odds:", log_odds)
  })
  
  output$probabilityValue <- renderText({
    center <- -1*input$beta0/input$beta1
    
    x_input <- input$x1
    beta0 <- input$beta0
    beta1 <- input$beta1
    
    probability <- exp(beta0 + beta1 * x_input) / (1 + exp(beta0 + beta1 * x_input))
    paste("Probability:", probability)
  })
  
}
shinyApp(ui = ui, server = server)