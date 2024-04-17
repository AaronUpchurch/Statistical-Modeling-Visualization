library(shiny)
library(ggplot2)
library(latex2exp)
library(DT)
library(jsonlite)
library(officer)


INITIAL = TRUE

#----------------------------------------------------
# TODO - CODE BELOW FOR LOGISTIC APPLET:
#----------------------------------------------------

ui <- fluidPage(


  titlePanel("Logistic Curve"),

  sidebarLayout(position = "left",

    sidebarPanel(
                 

      numericInput(inputId = "beta0",
                  label = withMathJax("$$\\text{Intercept}\\hspace{.25cm}\\alpha$$"),
                  min = -100,
                  max = 100,
                  value = 1),

      numericInput(inputId = "beta1",
                  label = withMathJax("$$\\text{Slope}\\hspace{.25cm}\\beta$$"),
                  min = -100,
                  max = 100,
                  value = 1),
      checkboxInput("activate_prob", "Find Odds Ratio + Probability"),
      uiOutput("prob_ui"),
      checkboxInput("activate_perc", "Find Percentile"),
      uiOutput("perc_ui"),

    ),
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), uiOutput("prob"), uiOutput("logit"))
      ),
        fluidRow(
          splitLayout(cellWidths = c("45%", "45%"), plotOutput(outputId = "distPlot1"), plotOutput(outputId = "logOdds1"))
        ),
      fluidRow(
        column(3, uiOutput(outputId = "logOddsValue")),
        # column(1, div(style = "margin-top: 10px;", uiOutput("logOdds_ui"))),
        column(3, uiOutput(outputId = "betaVal"), style = 'border-left: 1px solid', offset = 3),
        column(1, div(style = "margin-top: 10px;", actionButton("info_betaVal", icon("info"))), offset = 1)
      ),
      fluidRow(
        column(3, uiOutput(outputId = "probabilityValue")),
        column(1, div(style = "margin-top: 10px;", uiOutput("logProb_ui")), offset = 2),
        column(3, uiOutput(outputId = "eBeta"), style = 'border-left: 1px solid'),
        column(1, div(style = "margin-top: 10px;", actionButton("info_eBeta", icon("info"))), offset = 1)
      ),
      fluidRow(
        column(3, uiOutput(outputId = "percentileValue")),
                    column(1, div(style = "margin-top: 10px;", uiOutput("logPerc_ui")), offset = 2),
                    column(3, uiOutput(outputId = "eBetaminus"), style = 'border-left: 1px solid'),
        column(1, div(style = "margin-top: 10px;", actionButton("info_eBetaminus", icon("info"))), offset = 1)
        ),
      
    ),
  ),

)


server <- function(input, output) {

  output$logPerc_ui <- renderUI({
    if (input$activate_perc)
    {
      actionButton("info_perc", icon("info"))
    }
  })
  
  output$logProb_ui <- renderUI({
    if (input$activate_prob)
    {
      actionButton("info_prob", icon("info"))
    }
  })
  # 
  # output$logOdds_ui <- renderUI({
  #   if (input$activate_prob)
  #   {
  #     actionButton("info_odds", icon("info"))
  #   }
  # })
  
  
  output$betaVal <- renderUI({
    req(input$beta1)
    withMathJax(paste("$$ \\text{Value of } \\beta: ", input$beta1, "$$"))
  })
  output$eBeta <- renderUI({
    req(input$beta1)
    withMathJax(paste("$$ \\text{Value of } e^\\beta: ", round(exp(input$beta1), 3), "$$"))
  })
  output$eBetaminus <- renderUI({
    req(input$beta1)
    withMathJax(paste("$$ \\text{Value of } e^\\beta -1: ", round(exp(input$beta1), 3) - 1, "$$"))
  })
  
  output$logit <- renderUI({
    req(input$beta0, input$beta1)
    withMathJax(paste("$$\\text{Logit: log}\\left 
                      ( \\frac{\\pi}{1-\\pi} \\right )=\\alpha+\\beta x =", input$beta0, "+", input$beta1, "x$$")) 
  })
  output$prob <- renderUI({
    req(input$beta0, input$beta1)
    withMathJax(paste("$$\\text{Probability: }\\pi = \\frac{e^{\\alpha+\\beta x}}{1+e^{\\alpha+\\beta x}} = \\frac{e^{" ,
    input$beta0, "+", input$beta1, "x}}{1+e^{", input$beta0, "+", input$beta1, "x}} $$")) 
  })
  
  output$prob_ui <- renderUI({
    if (input$activate_prob) {
      numericInput("x1",
                   label = withMathJax("$$\\text{Value for X}$$"),
                   min = -100,
                   max = 100,
                   value = 1)
    }
  })
  output$perc_ui <- renderUI({
    if (input$activate_perc) {
      numericInput("percVal",
                   label = withMathJax("$$\\text{Input Percentile}$$"),
                   min = 0,
                   max = 1,
                   value = .5,
                   step = .1)
    }
  })
  
  output$distPlot1 <- renderPlot({
    
    req(input$beta1, input$beta0)
    
    if(input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double"))
    {
      x_values <- seq(input$x1 - 20, input$x1 + 20, length.out = 100)
    }
    else
    {
      # center <- -1*input$beta0/input$beta1
      
      getPercentile <- function(a, c, percentile)
      {
        if (a == 0)
        {
          return(0)
        }
        returnVal <- (-log((1/percentile) - 1) - c)/a
        return(returnVal)
      }
      
      first <- getPercentile(input$beta1, input$beta0, 0.0001)
      last <- getPercentile(input$beta1, input$beta0, 0.9999)
      
      
      x_values <- seq(first, last, length.out = 100)
    }
    
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
           y = expression(pi)) +
      {if(input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double"))geom_point(aes(x = input$x1, y = logistic_function(input$x1, input$beta1, input$beta0)), colour = "red")} +
      {if(input$activate_perc & (typeof(input$percVal) == "integer" | typeof(input$percVal) == "double"))geom_hline(yintercept = input$percVal, linetype = "dashed")}

  })
  
  output$logOdds1 <- renderPlot({
    
    req(input$beta1, input$beta0)

    if(input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double"))
    {
      x_values <- seq(input$x1 - 20, input$x1 + 20, length.out = 100)
    }
    else
    {
      center <- -1*input$beta0/input$beta1
      
      getPercentile <- function(a, c, percentile)
      {
        if (a == 0)
        {
          return(0)
        }
        returnVal <- (-log((1/percentile) - 1) - c)/a
        return(returnVal)
      }
      
      first <- getPercentile(input$beta1, input$beta0, 0.0001)
      last <- getPercentile(input$beta1, input$beta0, 0.9999)
      
      x_values <- seq(first, last, length.out = 100)
    }
    
    
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
      {if(input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double"))geom_point(aes(x = input$x1, y = logistic_function(input$x1, input$beta1, input$beta0)), colour = "red")} + 
      ylim(min(y_values), max(y_values)) +
      {if(input$activate_perc & (typeof(input$percVal) == "integer" | typeof(input$percVal) == "double"))geom_hline(yintercept = log(input$percVal/(1-input$percVal)), linetype = "dashed")}
  })

  output$logOddsValue <- renderUI({
    req(input$beta0, input$beta1)
  if (input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double"))
  {
    x1_input <- input$x1
    beta0 <- input$beta0
    beta1 <- input$beta1
    
    
    log_odds <- beta0 + beta1 * x1_input
    withMathJax(paste("$$\\text{Log Odds: }", signif(log_odds, 4), "$$"))
  }
  })
  
  output$probabilityValue <- renderUI({
    req(input$beta0, input$beta1)
    if (input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double"))
    {
      x1_input <- input$x1
      beta0 <- input$beta0
      beta1 <- input$beta1
      
      
      probability <- exp(beta0 + beta1 * x1_input) / (1 + exp(beta0 + beta1 * x1_input))
      withMathJax(paste("$$\\text{Probability: }", signif(probability, 4), "$$"))
    }
  })
  output$percentileValue <- renderUI({
    req(input$beta0, input$beta1)
    if (input$activate_perc)
    {
      perc_input <- input$percVal
      beta0 <- input$beta0
      beta1 <- input$beta1
      
      x_val <- (log(perc_input / (1 - perc_input)) - beta0) / beta1
      
      withMathJax(paste("$$\\text{X-Val @ Percentile: }", signif(x_val, 3), "$$"))
    }
  })
  
  observeEvent(input$info_betaVal, {
    req(input$beta1)
    showModal(modalDialog(
      title = HTML("Interpretation of &beta;"),
      easyclose = TRUE,
      HTML(paste("Simply put, the log odds of success additively changes by &beta; (", input$beta1, 
                 " in this case) for each 1-unit increase in the value of X.<br><br> We can show this general relationship using the equation for the log odds: ")),
           withMathJax(paste("$$log[odds(x+a)] - log[odds(x)] = [\\alpha + \\beta(x+a)] - [\\alpha + \\beta x] = \\beta a = ", input$beta1, "a$$")),
           HTML(paste("This implies that for an increase (a) in the value of X, the log of the odds ratio of success additively increases by", input$beta1, "* (a)<br><br> 
                      Please note that the log odds ratio refers to: ")),
      withMathJax(paste("$$log(\\frac{\\pi}{1-\\pi})$$"))
           
      
    ))
  })
  
  observeEvent(input$info_eBetaminus, {
    req(input$beta1)
    temp <- signif(exp(input$beta1) - 1, 3)
    insertText <- ""
    if (temp > 0)
    {
      insertText <- paste("increases by", signif(exp(input$beta1) - 1, 3) * 100,"%")
    }
    else if (temp < 0)
    {
      insertText <- paste("decreases by", signif(exp(input$beta1) - 1, 3) * -100, "%")
    }
    else
    {
      insertText <- "remain the same"
    }
    showModal(modalDialog(
      title = HTML("Interpretation of e<sup>&beta;</sup> - 1"),
      easyclose = TRUE,
      HTML(paste("Compared to simply &beta;, e<sup>&beta;</sup> - 1 refers to the percent change of the odds ratio. 
                 Simply put, the odds of success change by a factor of 100*(e<sup>&beta;</sup> - 1)% 
                 for each 1-unit increase in the value of X. In this case, for each 1-unit increase in the value of X, the odds ratio", insertText, "<br><br> We can show this general relationship using the equation for the odds ratio: ")),
      withMathJax(paste("$$odds(x+a) - odds(x) = e^{\\alpha + \\beta(x+a)} - e^{\\alpha + \\beta x} = e^{\\alpha + \\beta x} (e^{\\beta a} - 1)$$")),
        withMathJax(paste("$$= odds(x) (e^{\\beta a} - 1) = odds(x) (e^{", input$beta1, "a} - 1)$$")),
      HTML(paste("This implies that for an increase (a) in the value of X, the odds ratio of success changes by 100*(e<sup>&beta;a</sup> - 1)%"))
      
      
    ))
  })
  
  observeEvent(input$info_eBeta, {
    req(input$beta1)
    temp <- signif(exp(input$beta1) - 1, 3)
    insertText <- ""
    if (temp > 0)
    {
      insertText <- paste("increases by a factor of", signif(exp(input$beta1), 3))
    }
    else if (temp < 0)
    {
      insertText <- paste("decreases by a factor of", signif(exp(input$beta1), 3) * -1)
    }
    else
    {
      insertText <- "remain the same"
    }
    showModal(modalDialog(
      title = HTML("Interpretation of e<sup>&beta;</sup>"),
      easyclose = TRUE,
      HTML(paste("&beta; refers to the multiplicative change of the odds ratio. 
                 Simply put, the odds of success change by a factor of &beta; 
                 for each 1-unit increase in the value of X. In this case, for each 1-unit increase in the value of X, the odds ratio", insertText, "<br><br> We can show this general relationship using the equation for the odds ratio: ")),
      withMathJax(paste("$$odds(x+a)= e^{\\alpha + \\beta(x+a)} = e^{\\alpha + \\beta x} (e^{\\beta a})$$")),
      withMathJax(paste("$$= odds(x) (e^{\\beta a}) = odds(x) (e^{", input$beta1, "a})$$")),
      HTML(paste("This implies that for an increase (a) in the value of X, the odds ratio of success changes by a factor of e<sup>&beta;a</sup>"))
      
      
    ))
  })
  
  observeEvent(input$info_prob, {
    
    req(input$beta0, input$beta1, input$x1)
    if (input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double"))
    {
      x1_input <- input$x1
      beta0 <- input$beta0
      beta1 <- input$beta1
      
      
      probability <- exp(beta0 + beta1 * x1_input) / (1 + exp(beta0 + beta1 * x1_input))
      showModal(modalDialog(
        title = HTML("Interpretation of Probability"),
        easyClose = TRUE,
        HTML(paste("This represents the chance of success given the value of X inputted. Using this value, we can show that at the X-value ", input$x1, 
                   ", the rate of change in the probability per unit change in X is shown to be:")),
                   withMathJax(paste("$$\\beta\\pi(1-\\pi) = ", input$beta1, "(", signif(probability, 3), ")( 1-", signif(probability, 3), ") = ", signif(input$beta1*probability*(1-probability), 3), "$$")),
        plotOutput(outputId = "oddsPlot2")
        
      ))
    }
    
  })
  
  output$oddsPlot2 <- renderPlot({
    
    req(input$beta1, input$beta0, input$x1)
    
    x1_input <- input$x1
    beta0 <- input$beta0
    beta1 <- input$beta1
    
    
    probability <- exp(beta0 + beta1 * x1_input) / (1 + exp(beta0 + beta1 * x1_input))
    
    if(input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double"))
    {
      x_values <- seq(input$x1 - 20, input$x1 + 20, length.out = 100)
    }
    else
    {
      # center <- -1*input$beta0/input$beta1
      
      getPercentile <- function(a, c, percentile)
      {
        if (a == 0)
        {
          return(0)
        }
        returnVal <- (-log((1/percentile) - 1) - c)/a
        return(returnVal)
      }
      
      first <- getPercentile(input$beta1, input$beta0, 0.0001)
      last <- getPercentile(input$beta1, input$beta0, 0.9999)
      
      
      x_values <- seq(first, last, length.out = 100)
    }
    
    # Logistic function formula
    logistic_function <- function(x, a, c) {
      y <- 1 / (1 + exp(-(a * (x) + c)))
      return(y)
    }
    
    
    
    y_values <- logistic_function(x_values, input$beta1, input$beta0)
    
    x_point <- x1_input
    
    slope = input$beta1 * probability * (1-probability)
    y_point = logistic_function(x1_input, input$beta1, input$beta0)
    
    line_length = 5
    
    x_endpoint1 <- x_point - line_length/2
    x_endpoint2 <- x_point + line_length/2
    y_endpoint1 <- y_point - slope * (line_length/2)
    y_endpoint2 <- y_point + slope * (line_length/2)
    
    text_x <- (x_endpoint1 + x_endpoint2) / 2 
    text_y <- (y_endpoint1 + y_endpoint2) / 2
    
    plot_data <- data.frame(x = x_values, y = y_values)
    ggplot(plot_data, aes(x, y)) +
      geom_line() +
      labs(title = "Probability Curve",
           x = "x",
           y = expression(pi)) +
      geom_segment(aes(x = x_endpoint1, y = y_endpoint1, xend = x_endpoint2, yend = y_endpoint2),
                   linetype = "dashed") + 
      geom_text(aes(x = text_x, y = text_y+.1, label = paste("Slope:", signif(slope, 3))),
                hjust = 1.5, vjust = .5, colour = "darkblue") +
      {if(input$activate_prob & (typeof(input$x1) == "integer" | typeof(input$x1) == "double")) 
        geom_point(aes(x = input$x1, y = logistic_function(input$x1, input$beta1, input$beta0)), colour = "red")}
      # {if(input$activate_perc & (typeof(input$percVal) == "integer" | typeof(input$percVal) == "double"))geom_hline(yintercept = input$percVal, linetype = "dashed")}
    
  })
  
  
  
  
  
  observeEvent(input$info_perc, {
    req(input$beta0, input$beta1)
    showModal(modalDialog(
      title = HTML("Interpretation of Percentile"),
      easyClose = TRUE,
      HTML(paste("This represents the value of X needed to provide the inputted chance of success. Note that to find the value of X where we have a 50% chance of success, we can use the formula: ")),
      withMathJax(paste("$$\\frac{-\\alpha}{\\beta} = \\frac{-", input$beta0, "}{", input$beta1, "} = ", signif(-input$beta0/input$beta1, 3), "$$"))
    ))
    
    
  })
  
               # info_perc
               # info_prob
               # info_odds (log odds)
  

}


shinyApp(ui = ui, server = server)


#- Lots of e^B, e^B - 1, B, each calculation + explanation of definition.
