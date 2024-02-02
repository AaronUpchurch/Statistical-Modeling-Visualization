library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STA 3180: Statistical Modeling Visualizations!"),
  sidebarLayout(position = "right",
                sidebarPanel("By: Aaron Upchurch & Max LoGalbo"),
                mainPanel(
                  h3("Applets"),
                  h5(a("Tutorial", href = "/Tutorial/Index.html")),
                  h5(a("Logistic Regression", href = "/Logistic Regression/Index.html")),
                  h5(a("Boostrapping", href =       "/Bootstrapping/Index.html")),
                  h5(a("Cross Validation", href = "/Cross Validation/Index.html")),
                  h5(a("Decision Trees", href = "/Decision Trees/Index.html"))
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