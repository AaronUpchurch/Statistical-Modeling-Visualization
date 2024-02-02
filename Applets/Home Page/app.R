library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STA 3180: Statistical Modeling Visualizations!"),
  sidebarLayout(position = "right",
                sidebarPanel("By: Aaron Upchurch & Max LoGalbo"),
                mainPanel(
                  h3("Applets"),
                  h5(a("Tutorial", href = "http://127.0.0.1:7446/Tutorial/Index.html")),
                  h5(a("Logistic Regression", href = "http://127.0.0.1:7446/Logistic Regression/Index.html")),
                  h5(a("Boostrapping", href =       "http://127.0.0.1:7446/Bootstrapping/Index.html")),
                  h5(a("Cross Validation", href = "http://127.0.0.1:7446/Cross Validation/Index.html")),
                  h5(a("Decision Trees", href = "http://127.0.0.1:7446/Decision Trees/Index.html"))
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
  
  
  
  
}

library(shiny)


shinyApp(ui = ui, server = server)