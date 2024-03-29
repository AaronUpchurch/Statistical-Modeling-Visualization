library(shiny)
library(shinyjs)

# Define UI for application
ui <- fluidPage(
  shinyjs::useShinyjs(), # Ensure shinyjs is activated
  sidebarLayout(
    sidebarPanel(
      actionButton("button1", "Button 1"),
      actionButton("button2", "Button 2")
    ),
    mainPanel(
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # When button1 is pressed, trigger button2 click event
  observeEvent(input$button1, {
    shinyjs::click("button2")
  })
  
  # Output for button 1
  observeEvent(input$button1, {
    print("Button 1 Pressed")
  })
  
  # Output for button 2
  observeEvent(input$button2, {
    print("Button 2 Pressed")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
