library(shiny)

source(test.R)

acc <- run_ordinary_models()

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Cross Validation"),

    mainPanel(
      h3("Applets")

    )
  )


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  
}

library(shiny)

# See above for the definitions of ui and server


shinyApp(ui = ui, server = server)