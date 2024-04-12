library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("STA 3180: Statistical Modeling Visualization Applets", align = "center")),
  h5("By",
  tags$a(href="https://www.linkedin.com/in/aaron-upchurch/", 
         "Aaron Upchurch"),
  "and",
  tags$a(href="https://www.linkedin.com/in/max-logalbo/", 
         "Max LoGalbo"),align="center"),
  fluidRow(
    column(6,align="center",
           h3("Logistic Regression"),
           tags$a(href="https://aaronupchurch.shinyapps.io/logistic_regression/",img(src='image.jpg',width=400)),
           
           h3("Cross Validation"),
           tags$a(href=" https://aaronupchurch.shinyapps.io/cross_validation/",img(src='cross_validation.png',width=400, style = "border: 2px solid black;")),
           
           ),
    column(6,align="center",
           h3("Contigency Table",align="center"),
           tags$a(href="https://aaronupchurch.shinyapps.io/contingency_table/",img(src='image.jpg',width=400)),
           
           h3("Bootstrapping",align="center"),
           tags$a(href=" https://aaronupchurch.shinyapps.io/bootstrapping/",img(src='image.jpg',width=400)),
           
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

    
  
  
}

library(shiny)


shinyApp(ui = ui, server = server)