shinyApp(
  ui = shinyUI(
    fluidPage(
      fluidRow(
        actionButton("goButton", "", icon = icon("play-circle")),
        tags$button(
          id = "reset_button",
          class="btn action-button",
          icon("close")
          
        ),
        tags$button(
          id = "web_button",
          class = "btn action-button",
          tags$img(src = "http://images.all-free-download.com/images/graphicthumb/button_play_89677.jpg",
                   height = "50px")
        )
      ),
      fluidRow(
        textOutput("text")
      )
    )
  ),
  server = function(input, output, session){
    out <- reactiveVal("Nothing")
    observeEvent(input$goButton,{
      out("Go Pushed")
    })
    observeEvent(input$reset_button,{
      out("Resetted")
    })
    observeEvent(input$web_button,{
      out("From the web")
    })
    output$text <- renderText({out()})
  }
)