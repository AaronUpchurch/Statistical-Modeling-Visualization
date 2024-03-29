library(shiny)

# Define UI for application
ui <- fluidPage(
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
server <- function(input, output,session) {
  
  goToButton2 <- reactiveValues(value = F)
  
  # Output for button 1
  observeEvent(input$button1,{
    print("Button 1 Pressed")
    goToButton2$value <- T
  })
  
  # Output for button 2
  observeEvent(input$button2,{
    print("Button 2 Pressed")
  })
  
  observe({
    if(goToButton2$value){
    print("Button 2 Pressed")
    goToButton2$value <- F
    
  }})
}

# Run the application
shinyApp(ui = ui, server = server)
