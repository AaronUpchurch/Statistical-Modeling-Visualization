library(shiny)

titanic_df <- read.csv("datasets/titanic.csv")
titanic_df$LastName <- sapply(strsplit(titanic_df$Name,","),'[',1)


ui <- fluidPage(
  
  titlePanel(h1("Cross Validation Applet", align = "center")),
  h5("By Aaron Upchurch", align = "center"),

    fluidRow(
      sidebarPanel(
        
        h5(strong("1. Create Non CV Model")),
               actionButton("runModel", "Run Model"),
               actionButton("run100Model", "Run 100 Model"),

        h5(strong("2. Create CV Model")),
               actionButton("runCVModel", "Run CV Models"),
               actionButton("run100CVModel", "Run 100 CV Models")


      ),
      column(4,
             h4("No Cross Validation", align = "center"),
             
             h5(strong("Dataset")),
             tableOutput("modelData"),
             
             h5(strong("Model Parameters")),
             textOutput("modelParams"),
             
             h5(strong("Model Accuracy")),
             textOutput("modelAccuracy")
             
             
             ),
      column(4,
             h4("Cross Fold Validation",align = "center"))
    )
  )


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
 
  

  

  output$modelData <- renderTable(colnames = F,bordered = T,{
      matrix(data = titanic_df$LastName[1:36], nrow = 6)
    })
  
  model <- reactiveValues(
    params = c(1,2,3),
    accuracy = 0
  )
  
  output$modelParams <- renderText({
    paste("ln(p/(1-p)) = ",model$params[0],sep = "")
  })
  output$modelAccuracy <- renderText({
    paste("Model Accuracy = ",model$accuracy,"%",sep="")
  })
  
  observeEvent(input$runModel,
               {
                 m <- glm(Survived ~ ., data = titanic_df, family = "binomial")
                 model$params <- summary(m)$coefficients[,1]
                 
                 print(mean(titanic_df$Survived == round(predict.glm(m,type="response"))))
                 model$accuracy <- 100
               })
  
  observeEvent(input$runModel,
               {
                 model$params <- c(2,3,4)
                 model$accuracy <- 100
               })
  
  
  
}




shinyApp(ui = ui, server = server)