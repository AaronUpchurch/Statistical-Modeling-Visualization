library(shiny)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Logistic Curve"),

  #Logit Form ----
  mainPanel(withMathJax("$$\\text{Logit Form: }\\frac{\\pi}{1-\\pi}=\\beta_0+\\beta_1 x \\hspace{1cm} \\text{Probability Form: }\\pi = \\frac{e^{\\beta_0+\\beta_1 x}}{1+e^{\\beta_0+\\beta_1 x}}$$")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "beta0",
                  label = withMathJax("$$\\text{Intercept}\\hspace{.25cm}\\beta_0$$"),
                  min = -5,
                  max = 5,
                  value = 0),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "beta1",
                  label = withMathJax("$$\\text{Slope}\\hspace{.25cm}\\beta_1$$"),
                  min = -5,
                  max = 5,
                  value = 0),

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      
      plotOutput(outputId = "logOdds")

    )
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
    
    x_values <- seq(-5, 5, length.out = 100)
    
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
    x_values <- seq(-5, 5, length.out = 100)
    
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
           y = "log odds") +
      ylim(-30, 30)
  })

}
shinyApp(ui = ui, server = server)

