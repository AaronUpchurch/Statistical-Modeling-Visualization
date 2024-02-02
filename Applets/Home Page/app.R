library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STA 3180: Statistical Modeling Visualizations"),
  sidebarLayout(position = "right",
                sidebarPanel("By: Aaron Upchurch & Max LoGalbo"),
                mainPanel(
                  h3("Applets"),
                  h5(a("Tutorial", href = "/Statistical-Modeling-Visualization/Tutorial")),
                  h5(a("Logistic Regression", href = "/Statistical-Modeling-Visualization/Logistic Regression")),
                  h5(a("Boostrapping", href =       "/Statistical-Modeling-Visualization/Bootstrapping")),
                  h5(a("Cross Validation", href = "/Statistical-Modeling-Visualization/Cross Validation")),
                  h5(a("Decision Trees", href = "/Statistical-Modeling-Visualization/Decision Trees"))
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