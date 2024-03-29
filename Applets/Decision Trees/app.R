library(shiny)
library(shinydashboard)

ui <- fluidPage(

    valueBoxOutput("random")
  
)

server <- function(input, output, session) {
  output$random <- renderValueBox({
    invalidateLater(1000, session)
    h4(sample(1:100, 1))
  })
}

shinyApp(ui = ui, server = server)