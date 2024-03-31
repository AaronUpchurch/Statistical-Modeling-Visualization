# Imports
library(shiny)
library(shinyalert)


# Dataset Preprocessing
titanic_df <- read.csv("datasets/titanic.csv")
titanic_df$Initials <- strsplit(titanic_df$Name, ", ")[[1]]
titanic_df <- na.omit(titanic_df)
titanic_df <- titanic_df[sample(1:nrow(titanic_df),size=144),]


clean_titanic <- titanic_df[1:10,c("Initials","Sex","Age","Pclass","Survived")]
clean_titanic <- apply(clean_titanic,
                       MARGIN=2,
                       FUN=function(x){
                         paste0('<span style="font-size:14px">', x, '</span>')
                       })
clean_titanic <- apply(clean_titanic,
                 MARGIN=2,
                 FUN = function(x){
                   paste0("<td>",x,"</td>")
                 })
clean_titanic <- apply(clean_titanic,
                 MARGIN=1,
                 FUN=function(x){
                   paste("<tr>",x[1],x[2],x[3],x[4],x[5],"</tr>")
                 })
clean_titanic <- toString(clean_titanic)
clean_titanic <- paste0("<center>
                        <table border='1'>
                           <tr>
                              <th> <span style=\"font-size:14px\"> Initials </span> </th>
                              <th> <span style=\"font-size:14px\"> Sex </span> </th>
                              <th> <span style=\"font-size:14px\"> Age </span> </th>
                              <th> <span style=\"font-size:14px\"> Pclass </span> </th>
                              <th> <span style=\"font-size:14px\"> Survived </span> </th>
                           </tr>",
        
                           clean_titanic,
                           
                           "</table>
                            </center>")


clean_titanic <- gsub(',', '', clean_titanic)

# User Interface
ui <- fluidPage(
  
  useShinyalert(),
  
  
  # Title
  titlePanel(h1("Cross Validation Applet", align = "center")),
  h5("By Aaron Upchurch", align = "center"),

    fluidRow(
      
      # Control Sidebar
      sidebarPanel(width = 4,
        
        # Non CV MOdel Buttons
        h5(strong("1. Create Non CV Model")),
               actionButton("runModel", "Run Model"),
               actionButton("run100Model", "Run 100 Model"),
        
        # Get Help Button
        h5(strong("Get Help")),
        actionButton("getHelp","Get Help")
      ),
      
      # No Cross Validation Display
      column(4,
             
             # Column Title
             h4("No Cross Validation", align = "center"),
             
             # Dataset
             h5(strong("Dataset")),
             tableOutput("modelData"),

             # Model Parameters
             h5(strong("Model Parameters")),
             textOutput("modelParams"),
             
             # Model Accuracy
             h5(strong("Model Accuracy")),
             textOutput("modelAccuracy")
             
             
             ),
      
      # Cross Validation Display
      column(4,
             
             
             # Column Title
             h4("Cross Fold Validation",align = "center")),
      
            # Datasets
            h5(strong("CV Dataset")),
            tableOutput("modelCVDataset"),
      
            # Model Parameters
            h5(strong("Model Parameters")),
            textOutput("modelCV1Params"),
            textOutput("modelCV2Params"),
            textOutput("modelCV3Params"),
      
            # Model Parameters
            h5(strong("Model Accuracies")),
            textOutput("modelCV1Accuracy"),
            textOutput("modelCV2Accuracy"),
            textOutput("modelCV3Accuracy"),
      
      ),
  
  
  tags$head(
    tags$style(
      HTML("
         .text-inside-cell {
            display: block;
            padding: 0px; /* Adjust the padding as needed */
            margin: -5px; /* Remove margin */

         }
      ")
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  #------ Initial Popup -----------------#
  showPopup <- reactiveVal(T)
  
  observeEvent(showPopup,{
    shinyalert("Welcome",
               paste0("This is a RShiny applet designed to teach students about <span style=\"font-weight:bold; \"> cross validation </span> <br> <br>",
                      "Logistic regression models are train on the titanic survival dataset <br> <br>",
                      clean_titanic), 
               size = "m", html = T)
    showPopup(F)
  })
  
  #=========== Reactive Values ================
  
  # Non CV Model
  model <- reactiveValues(
    params = NA,
    accuracy = NA
  )
  
  model1 <- reactiveValues(
    params = NA,
    accuracy = NA
  )
  
  model2 <- reactiveValues(
    params = NA,
    accuracy = NA
  )
  
  model3 <- reactiveValues(
    params = NA,
    accuracy = NA
  )
  
  modelDataset <- reactiveValues(
    train = NA,
    test = NA
  )
  
  modelCVDataset <- reactiveValues(
    data = NA
  )
  
  modelCVDataset1 <- reactiveValues(
    data = NA
  )
  
  modelCVDataset2 <- reactiveValues(
    data = NA
  )
  modelCVDataset3 <- reactiveValues(
    data = NA
  )
  

 
  #================ Outputs ======================
  
  # Non CV Data
  output$modelData <- renderTable(colnames = F,bordered = T,width = "100px",{
      temp <- matrix(data = titanic_df$Initials, nrow = 18)
      
      temp_fun <- function(t){
        if(t %in% modelDataset$train$Initials){
          paste0('<span class="text-inside-cell" style="color:red; font-size:10px">', t, '</span>')
        }
        else{
          paste0('<span class="text-inside-cell" style="color:blue;font-size:10px">', t, '</span>')
        }
      }
      

      ret <- matrix(mapply(temp_fun,temp),nrow=18)
      

      return(ret)
    }, sanitize.text.function = function(x) x)
  
  
  # Non CV Model params
  output$modelParams <- renderText({
    
    if(is.na(model$params)){
      ""
    }
    else{
    paste(
      "ln(p/(1-p)) = ", 
      model$params["(Intercept)"],
      " + PClass", model$params["Pclass"],
      " + Age",  model$params["Age"],
      " + Sex",  model$params["Sexmale"],
      sep = "")
    }
  })
  
  output$modelAccuracy <- renderText({
    paste("Model Accuracy = ",model$accuracy*100,"%",sep="")
  })
  
  output$modelCV1Accuracy <- renderText({
    paste("Model Accuracy = ",model1$accuracy*100,"%",sep="")
  })
  
  output$modelCV2Accuracy <- renderText({
    paste("Model Accuracy = ",model2$accuracy*100,"%",sep="")
  })
  
  output$modelCV3Accuracy <- renderText({
    paste("Model Accuracy = ",model3$accuracy*100,"%",sep="")
  })
  
  # Non CV Model params
  output$modelCV1Params <- renderText({
    
    if(is.na(model1$params)){
      "ln(p/(1-p)) ="
    }
    else{
      paste(
        "ln(p/(1-p)) = ", 
        model1$params["(Intercept)"],
        " + PClass", model1$params["Pclass"],
        " + Age",  model1$params["Age"],
        " + Sex",  model1$params["Sexmale"],
        sep = "")
    }
  })
  
  # Non CV Model params
  output$modelCV1Params <- renderText({
    
    if(is.na(model2$params)){
      "ln(p/(1-p)) ="
    }
    else{
      paste(
        "ln(p/(1-p)) = ", 
        model2$params["(Intercept)"],
        " + PClass", model2$params["Pclass"],
        " + Age",  model2$params["Age"],
        " + Sex",  model2$params["Sexmale"],
        sep = "")
    }
  })
  
  # Non CV Model params
  output$modelCV3Params <- renderText({
    
    if(is.na(model3$params)){
      "ln(p/(1-p)) ="
    }
    else{
      paste(
        "ln(p/(1-p)) = ", 
        model3$params["(Intercept)"],
        " + PClass", model3$params["Pclass"],
        " + Age",  model3$params["Age"],
        " + Sex",  model3$params["Sexmale"],
        sep = "")
    }
  })
  
  
  # CV Dataset
  output$modelCVDataset <- renderTable(colnames = F,bordered = T,width = "100px",{
    
    temp <- matrix(data = 
                      c(modelCVDataset1$data$Initials,
                        modelCVDataset2$data$Initials,
                        modelCVDataset3$data$Initials), nrow = 18,byrow = T)
    
    
    
    temp_fun <- function(t){
      if(t %in% modelCVDataset1$data$Initials){
        paste0('<span class="text-inside-cell" style="color:red; font-size:10px">', t, '</span>')
      }
      else if(t %in% modelCVDataset2$data$Initials){
        paste0('<span class="text-inside-cell" style="color:blue;font-size:10px">', t, '</span>')
      }
      else if(t %in% modelCVDataset3$data$Initials){
        paste0('<span class="text-inside-cell" style="color:yellow;font-size:10px">', t, '</span>')
      }
      else{
        paste0('<span class="text-inside-cell" style="color:black;font-size:10px">', t, '</span>')
      }
    }
    
    
    ret <- matrix(mapply(temp_fun,temp),nrow=18)
    
    
    return(ret)
  }, sanitize.text.function = function(x) x)
  
  
  
  
  #============== Observations ======================
  
  # Run Non CV Model Button
  observeEvent(input$runModel,
               {
                 
                 # Get new train/test split
                 train_rows <- sample(1:nrow(titanic_df),as.integer(0.7*nrow(titanic_df)))
                 test_rows <- (1:nrow(titanic_df))[!(1:nrow(titanic_df) %in% train_rows)]
                 modelDataset$train <- titanic_df[train_rows,]
                 modelDataset$test <- titanic_df[test_rows,]
                 
                 # Train Non CV Model
                 m <- glm(Survived ~ Pclass + Age + Sex, data = modelDataset$train, family = binomial(link = "logit"))
                 test_preds <- round(predict.glm(m, newdata = modelDataset$test, type = "response"))
                
                 # Store model parameters and accuracy
                 model$params <- round(summary(m)$coefficients[,1],2)
                 model$accuracy <-  round(mean(modelDataset$test$Survived == test_preds),2)
                 

               })
  
  
  # Run Non CV Model Button
  observeEvent(input$runCVModel,
               {
                 

                 # Get new train/test split
                 randRows <- sample(1:nrow(titanic_df),nrow(titanic_df))
                 rows1 <- randRows[1:(nrow(titanic_df)/3)]
                 rows2 <- randRows[((nrow(titanic_df)/3)+1):((nrow(titanic_df)*2)/3)]
                 rows3 <- randRows[(((nrow(titanic_df)*2)/3)+1):nrow(titanic_df)]
                 
                 modelCVDataset1$data <- titanic_df[rows1,]
                 modelCVDataset2$data <- titanic_df[rows2,]
                 modelCVDataset3$data <- titanic_df[rows3,]
                 
                 m1 <- glm(Survived ~ Pclass + Age + Sex, data = append(modelCVDataset1$data,modelCVDataset2$data), family = binomial(link = "logit"))
                 m2 <- glm(Survived ~ Pclass + Age + Sex, data = append(modelCVDataset2$data,modelCVDataset3$data), family = binomial(link = "logit"))
                 m3 <- glm(Survived ~ Pclass + Age + Sex, data = append(modelCVDataset3$data,modelCVDataset1$data), family = binomial(link = "logit"))
                 
                 test_preds1 <- round(predict.glm(m1, newdata = modelCVDataset3$data, type = "response"))
                 test_preds2 <- round(predict.glm(m2, newdata = modelCVDataset1$data, type = "response"))
                 test_preds3 <- round(predict.glm(m3, newdata = modelCVDataset2$data, type = "response"))
                 
                 model1$params <- round(summary(m1)$coefficients[,1],2)
                 model2$params <- round(summary(m2)$coefficients[,1],2)
                 model3$params <- round(summary(m3)$coefficients[,1],2)
                 
                 model1$accuracy <-  round(mean(modelCVDataset3$data$Survived == test_preds1),2)
                 model2$accuracy <-  round(mean(modelCVDataset1$data$Survived == test_preds2),2)
                 model3$accuracy <-  round(mean(modelCVDataset2$data$Survived == test_preds3),2)
                 
                 
               })
  
  
  
  observeEvent(input$getHelp, {
    shinyalert("Confused?", "
                            <div style='text-align: center;'>
                            Cross Validation is a way to obtain a more <span style=\"font-weight:bold; \"> reliable </span>test accuracy of a model. <br> <br>
                                                                      
               
               
           
                            
                            <span style=\"font-weight:bold; \"> Without Cross Validation </span> <br> 
               
                            The test accuracy of a model can depend heavily on the train-test split. <br> <br>
               
                            <span style=\"font-weight:bold; \"> With Cross Validation </span> <br> 
               
                            The dataset is randomly divided into 3 parts. <br> 
                            Three models are trained on ___. <br> 
                            The average . <br> 
                             The dataset is randomly divided into 3 parts. <br> 
                            ",size = "m",html = T
    )})
  

  
  
  
}



# Run RShiny Application
shinyApp(ui = ui, server = server)