# R Shiny code for the Cross Validation applet

# Author: Aaron Upchurch
# Date: Spring 2024
# Advisor: Dr. Johnson

#======== Imports ===========
library(shiny)
library(shinyalert)
library(shinyjs)

#======= Dataset Reprocessing ========

# Read dataset
titanic_df <- read.csv("datasets/titanic.csv")

# Extract the last names
titanic_df$Lastname <- sapply(strsplit(titanic_df$Name, ", "), `[`, 1)
titanic_df$Lastname <- sapply(titanic_df$Lastname,FUN=function(x){strsplit(x,split=" ")[[1]][1]})
titanic_df$Lastname <- sapply(titanic_df$Lastname,FUN=function(x){strsplit(x,split="-")[[1]][1]})

# Remove duplicate last names
titanic_df <- titanic_df[!duplicated(titanic_df$Lastname),]

# Remove all rows with missing data
titanic_df <- na.omit(titanic_df)

# Downsample dataset to 144 rows
titanic_df <- titanic_df[1:144,]

#=========== User Interface ======================
ui <- fluidPage(
  
  # Enable Shiny Javascript Library
  useShinyjs(),
  
  # Title
  titlePanel(h1("Cross Validation Applet", align = "center")),

  fluidRow(

    # Control Sidebar
    sidebarPanel(width = 3,
                 
                 # Create regular model button
                 h5(strong("1. Create Model Without Cross Validation")),
                 actionButton("createModel","Create Model"),
                 
                 # Create Cross Validated Model
                 h5(strong(textOutput("createCV"))),
                 actionButton("createCVModel","Create CV Model"),
                 
                 # K Slider Input
                 shinyWidgets::sliderTextInput(inputId = "k", 
                                               label = "3. Select K", 
                                               choices = c(2,3,4,6,8,12,16,24,48,144)),
                 
                 # Todo Text Box
                 h5(strong("Tutorial")),
                 h5(uiOutput("multiline_text")),
                 
                 # Get Help Button
                 h5(strong("Get Help")),
                 actionButton("getHelp","Get Help")
    ),
    
    # No Cross Validation Column
    column(4,
           
           # Column Title
           h3("No Cross Validation", align = "center"),
           
           # Name Dataset
           h5(strong(textOutput("datasetTitle"))),
           tableOutput("nameDataframe"),
           
           # Model Information Dataset
           h5(strong(textOutput("modelInformation"))),
           tableOutput("modelDataframe"),
           
    ),
    
    # K Fold Cross Validation Display
    column(4,
           
           # Column Title
           h3(textOutput("kFoldTitle"), align = "center"),
           
           # Name Dataset
           h5(strong(textOutput("cvDatasetTitle"))),
           tableOutput("cvNameDataframe"),
           
           # Model Information Dataset
           h5(strong(textOutput("cvModelInformation"))),
           tableOutput("cvModelDataframe"),
    ),),
  
  # Name Table Formatting
  tags$head(
    tags$style(
      HTML("
         .text-inside-cell {
            display: block;
            padding: 0px;
            margin: -5px;
         }
         .text-inside-cell2 {
            padding: 5px;
            margin: 5px;
         }"))))
         
         
        
# Server Logic
server <- function(input, output,session) {
  
  
  # Hide/Show Buttons 
  observe({
    
    # If haven't created enough regular models, hide CV model button and K slider
    if(input$createModel < 5){
      shinyjs::hide("createCVModel")
      shinyjs::hide("k")
    }

    else{
      
      # If have created enough regular models, show CV model button
      shinyjs::show("createCVModel")
      
      # if have already created enough CV models with K=2, show K slider
      if(input$createCVModel >= 5){
      shinyjs::show("k")
    }}})

  
  # Text Rendering for whether or not to show certain text
  output$datasetTitle <- renderText({ifelse(input$createModel == 0,"","Dataset")}) 
  output$modelInformation <- renderText({ifelse(input$createModel == 0,"","Model Information")}) 
  output$cvDatasetTitle <- renderText({ifelse(input$createCVModel == 0,"","Dataset")}) 
  output$cvModelInformation <- renderText({ifelse(input$createCVModel == 0,"","Model Information")}) 
  output$reminderText <- renderText({ifelse(input$createModel == 0,"","Notice how the test accuracy 
                                                                       varies for different 
                                                                      train-test splits")})
  output$createCV <- renderText({ifelse(input$createModel < 5,"","2. Create Model With Cross Validation")})
  text <-  HTML("Create a Non-CV model \U00A0 \U00A0 ")
  output$kFoldTitle <- renderText({ifelse(input$createCVModel == 0,"","K Fold Cross Validation")})
  
  # Text Box Rendering Logic
  output$multiline_text <- renderUI({
    
    # Starter Text
    if(input$createModel == 0){
      text <-  HTML("Create a model <b> without cross validation</b>.")
    }
    
    # After created first ordinary model
    else if(input$createModel == 1){
    text <-  HTML("The model's train and test sets are shown in the dataset table. <br> <br>
                  
                  The model's <span style=\'color:red\'> test accuracy </span> is shown in the model table. <br> <br>
                  
                  Create another model and notice how the <span style=\'color:red\'> test accuracy </span> changes.")
    }
    
    # after created second ordinary model
    else if(input$createModel < 2){
      text <-  HTML("The new model has a different train/test split and a different <span style=\'color:red\'> test accuracy </span>. <br> <br>
                    
                    Create a few more models and pay attention to the changing <span style=\'color:red\'> test accuracy </span>.
                    ")
    }
    
    # after created 7 ordinary models
    else if(input$createModel < 5 ){
      text <-  HTML("<b> IMPORTANT</b>: The changing <span style=\'color:red\'> test accuracy </span> makes it difficult to know how good our model really is.<br> <br>
                    
                     Keep creating new models and pay attention to the changing <span style=\'color:red\'> test accuracy </span>.
                    ")
    }
    
    # before creating CV model
    else if(input$createCVModel == 0){
      text <-  HTML("Create a model <b> with cross validation</b>.")
    }
    
    # after creating models with K=2
    else if(input$createCVModel < 5){
      text <-  HTML("The dataset is split into two equal sets. <br> <br>
                    
                    Two models are trained on one set and tested on the other. <br> <br>
                    
                    The <span style=\'color:red\'> average test accuracy </span> is then computed. <br> <br>
                    
                    Keep creating models with cross validation and notice how the <span style=\'color:red\'> average test accuracy </span> is more consistent than before.")
    }
    
    # after creating 8 models with K=2
    else if(input$k == 2){
      text <-  HTML("Increase K to 3 and create another model")
    }
    
    # while creating K=3 models
    else if(input$k == 3 && input$createCVModel >= 5 && input$createCVModel < 9){
      text <-  HTML("Now, the dataset is split into three equal sets. <br> <br>
                    
                    Three models are trained on two of the sets and tested on the other. <br> <br>
                    
                    Keep creating models with K=3 and notice how the average <span style=\'color:red\'> test accuracy </span> is now even more consistent than before.")
    }
    
    # until K is 8
    else if(input$k != 12){
      text <-  HTML("Keep increasing K and making models until the <span style=\'color:red\'> test accuracy </span> stays constant")
    }
    
    # once K is 8
    else{
      text <-  HTML("At K = 12, the <span style=\'color:red\'> test accuracy </span> is almost perfectly constant. <br> <br>
                    In summary, cross validation is useful because it creates a <b>more reliable</b> test accuracy for a model.")
    }
    
    
    # Define CSS styles
    circle_style <- "width: 260px; height: auto;  background-color: #FFFFFF; border: 1px solid #2c3e50; display: flex; justify-content: center; align-items: center; padding: 10px;"
    text_style <- "color: black; font-size: 14px;"
    
    # HTML rendering for text box
    box_text <- tags$div(
      style = circle_style,
      tags$span(style = text_style, text)
    )
    
    
    
    # Return the HTML text box
    box_text})
  
  #=========== Initial Popup Message ==================#
  showPopup <- reactiveVal(T)
  
    observeEvent(showPopup,{
      
    # Create smaller dataset for initial message
    small_titanic <- titanic_df[1:5,c("Lastname","Sex","Age","Pclass","Survived")]
    
    # Add HTML code to smaller table in popup
    small_titanic <- apply(small_titanic,
                           MARGIN=2,
                           FUN=function(x){
                             paste0('<span style="font-size:14px">', x, '</span>')
                           })
    
    small_titanic <- apply(small_titanic,
                           MARGIN=2,
                           FUN = function(x){
                             paste0("<td class = \"text-inside-cell2\">",x,"</td>")
                           })
    
    small_titanic <- apply(small_titanic,
                           MARGIN=1,
                           FUN=function(x){
                             paste("<tr>",x[1],x[2],x[3],x[4],x[5],"</tr>")
                           })
    
    small_titanic <- toString(small_titanic)
    
    small_titanic <- paste0("<center>
                        <table border='1'>
                           <tr>
                              <th> <span class=\"text-inside-cell2\";style=\"font-size:14px\"> Last Name </span> </th>
                              <th> <span class=\"text-inside-cell2\";style=\"font-size:14px\"> Sex </span> </th>
                              <th> <span class=\"text-inside-cell2\";style=\"font-size:14px\"> Age </span> </th>
                              <th> <span class=\"text-inside-cell2\";style=\"font-size:14px\"> Class </span> </th>
                              <th> <span class=\"text-inside-cell2\";style=\"font-size:14px\"> Survived </span> </th>
                           </tr>",
                        
                        small_titanic,
                        
                        "</table>
                            </center>")
    
    small_titanic <- gsub(',', '', small_titanic)
    shinyalert("Welcome",
               paste0("This is a RShiny applet designed to teach students about <span style=\"font-weight:bold; \"> cross validation </span> <br> <br>",
                      "We'll use <u>logistic regression models</u> to predict <br> if passengers on the Titanic would have <u>survived</u> <br> <br>",
                      small_titanic), 
               size = "m", html = T)
    showPopup(F)
  })
  
  #=========== Reactive Values ================#
  
  # Dataset for non-cv models
  dataset <- reactiveValues(
    whole = titanic_df[(sample(1:nrow(titanic_df),size = nrow(titanic_df))),],
    splits = c()
  )
  
  # Dataset for CV models
  CVdataset <- reactiveValues(
    whole = titanic_df[(sample(1:nrow(titanic_df),size = nrow(titanic_df))),],
    splits = c()
  )
  
  # Non-CV Model information dataframe
  modelDataframe <- reactiveVal(NA)
  
  # CV model information dataframe
  cvModelDataframe <- reactiveVal(NA)
  
  # Font colors for name table
  generate_html_color <- function() {
    sprintf("#%06X", sample(0:0xFFFFFF, 1))
  }
  random_colors <- replicate(144, generate_html_color())
  
  # Add some good initial colors
  set_colors <- c("#067f36","#a61008","#080E4B","#804600")

  html_colors <- reactiveVal(append(set_colors,random_colors))
  

  #================ Outputs ======================
  
  # Dataset
  output$cvNameDataframe <- renderTable(colnames = F,bordered = T,width = "100px",{
    
    if(input$createCVModel== 0 ){
      return(NULL)
    }
    
    temp <- matrix(data = dataset$whole$Lastname, nrow = 12)
    
    add_color <- function(t){
      for(i in 1:input$k){
        if(t %in% dataset$splits[[i]]$Lastname){
          return(paste0('<span class="text-inside-cell" style="color:',html_colors()[[i]],'; font-size:11px">', t, '</span>'))
        }
      }
    }
    
    ret <- matrix(mapply(add_color,temp),nrow=24)

    
    return(ret)
  }, sanitize.text.function = function(x) x)
  
  # Dataset
  output$nameDataframe <- renderTable(colnames = F,bordered = T,width = "100px",{
    
    if(input$createModel==0){
      return(NULL)
    }
    
    temp <- matrix(data = CVdataset$whole$Lastname, nrow = 12)
    
    
  
    
    
    
    add_color <- function(t){
      for(i in 1:2){
        if(t %in% CVdataset$splits[[i]]$Lastname){
          return(paste0('<span class="text-inside-cell" style="color:',html_colors()[[i]],'; font-size:11px">', t, '</span>'))
        }
      }
    }
    
    ret <- matrix(mapply(add_color,temp),nrow=24)
    
    
    return(ret)
  }, sanitize.text.function = function(x) x)
  
  
  # Model Datasets
  output$cvModelDataframe <- renderTable(colnames =T, bordered = T, {
    
    if(input$createCVModel == 0){
      return(NULL)
    }
    
    cvModelDataframe()
  }, sanitize.text.function = function(x) x)
  
  output$modelDataframe <- renderTable(colnames =T, bordered = T, {
    if(input$createModel == 0){
      return(NULL)
    }
    modelDataframe()
  }, sanitize.text.function = function(x) x)
  
  
  #============== Observations ======================
  
  observeEvent(input$createCVModel,
               {
                 
                 # randomly reorder dataset
                 dataset$whole <- dataset$whole[(sample(1:nrow(dataset$whole),size = nrow(dataset$whole))),]
                 
                 
                 if(input$k == 1){
                   train_rows <- as.array(sample( seq(1,nrow(dataset$whole)), as.integer(0.7*nrow(dataset$whole))))
                   test_rows <- as.array(setdiff(seq(1,nrow(dataset$whole)),train_rows))
                   
                   dataset$splits[[1]] <- dataset$whole[train_rows,]
                   dataset$splits[[2]] <- dataset$whole[test_rows,]
                   
                 }
                 
                 else{
                   
                   # divide dataset into k sections
                   for(i in 1:input$k){
                     start_row <- 1 + (i-1)*(144/input$k)
                     end_row <- i*(144/input$k)
                     subset_rows <- start_row:end_row
                     subset <- dataset$whole[subset_rows,]
                     dataset$splits[[i]] <- subset
                   }
                 }
                 
                 # Train Models
                 number <- c()
                 models <- c()
                 accuracies <- c()
                 params <- c()
                 trainSize <- c()
                 testSize <- c()
                 trainSet <- c()
                 testSet <- c()
                 accuracy_ints <- c()
                 
                 # Get Numbers
                 if(input$k == 1){
                   number = c(1)
                 }
                 else{
                   number = seq(1,input$k)
                 }

                   for(i in 1:input$k){
                     

  
                     # get training dataset
                     if(input$k == 1){
                       train <- dataset$splits[[1]]
                       test <- dataset$splits[[2]]
                     }
                     else{
                       
                       # get test dataset
                       test <- dataset$splits[[i]]
                       
                       train <- dataset$splits[as.array(setdiff(1:input$k,i))]
                       train <- do.call(rbind, train)
                     }
                     
                      
                     
                     # Create model
                     models[[i]] <- glm(Survived ~ Pclass + Age + Sex, data = train, family = binomial())

                     # Get model test predictions
                     test_preds <- round(predict.glm(models[[i]],type="response",newdata = test))
                     

                     # Get test set accuracy
                     accuracies[[i]] <- paste(toString(round(mean(test_preds == test$Survived)*100,2)),"%",sep="")
                     accuracy_ints[[i]] <- mean(test_preds == test$Survived)*100

                     # Get model coefficients
                     coeffs <- round(coef(models[[i]]),2)
                     
                     params[[i]] <- paste(
                       "ln(p/(1-p)) = ", 
                       coeffs["(Intercept)"],
                       " + PClass", coeffs["Pclass"],
                       " + Age",  coeffs["Age"],
                       " + Sex",  coeffs["Sexmale"],
                       sep = "")
                     
                     # Get model train size and test size
                     trainSize[[i]] <- as.integer(nrow(train))
                     testSize[[i]] <- as.integer(nrow(test))
                     
                     # Get model train and test sets
                     trainSet[[i]] <- as.integer(setdiff(1:input$k,i))
                     testSet[[i]] <- toString(round(i,0))
                   }
                 
                 
                 
                 
                 


             
                  
                 # add formating
                 for(i in 1:input$k){
                   for(j in 1:length(trainSet[[i]])){
                        trainSet[[i]][[j]] <- paste0('<span class= \"text-inside-cell\" style=\"color:',html_colors()[[as.integer(trainSet[[i]][[j]])]],'; font-size:20px; display: inline;\">', "&#9632 \U00A0  ", '</span>')
                   }
                   trainSet[[i]] <- paste(trainSet[[i]],collapse="")
                 testSet[[i]] <- paste0('<span class= \"text-inside-cell\" style=\"color:',html_colors()[[i]],';  font-size:20px; \">', "&#9632", '</span>')
                 }

                 number <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px; color:\">",as.integer(number),"</span>")
                 trainSize <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.integer(as.array(trainSize)),"</span>")
                 testSize <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.integer(as.array(testSize)),"</span>")
                 accuracies <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.array(accuracies),"</span>")
                 


                 # Add Accuracy row
                 number <- append(c("Average"),number)
                 trainSet <- as.array(append(c(" "),as.array(trainSet)))
                 testSet <- as.array(append(c(" "),as.array(testSet)))
                 trainSize <- append(c(" "),trainSize)
                 testSize <- append(c(" "),testSize)
                 accuracies <- append(c(paste0(round(mean(as.numeric(accuracy_ints))),"%")),accuracies)
                 
                 accuracies[[1]] <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px; color:red;\">",accuracies[[1]],"</span>")
                 
                 
                 cvModelDataframe(data.frame(
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Model # </span>" = number,
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Train Set </span>" = trainSet,
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Set </span>"= testSet,
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Train Set Size </span>" = trainSize,
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Set Size </span>" = testSize,
                   "<span class= \"text-inside-cell\" style=\"font-size:12px\"> Test Accuracy </span>" = accuracies
                   #Parameters = as.array(params)
                   ,check.names = F))})
  
  
  # If Create Model Button Pressed
  observeEvent(input$createModel,
               {
                 
                 # randomly reorder dataset 
                 CVdataset$whole <- CVdataset$whole[(sample(1:nrow(CVdataset$whole),size = nrow(CVdataset$whole))),]
                 
                 # Get train set rows
                 train_rows <- as.array(sample( seq(1,nrow(CVdataset$whole)), as.integer(0.7*nrow(CVdataset$whole))))
                 
                 # Get test set rows
                 test_rows <- as.array(setdiff(seq(1,nrow(CVdataset$whole)),train_rows))
                 
                 # Get train set
                 train <- CVdataset$whole[train_rows,]
                 
                 # Get test set
                 test <- CVdataset$whole[test_rows,]
                 
                 # Save train set
                 CVdataset$splits[[1]] <- train
                 
                 # Save test set
                 CVdataset$splits[[2]] <- test
                   
                 # Initialize columns of model information table
                 number <- c()
                 models <- c()
                 accuracies <- c()
                 params <- c()
                 trainSize <- c()
                 testSize <- c()
                 trainSet <- c()
                 testSet <- c()

                 # Get Model Numbers
                 number = c(1)
               
                 # Create logistic regresion model
                 models[[1]] <- glm(Survived ~ Pclass + Age + Sex, data = train, family = binomial())
               
                 # Get model test predictions
                 test_preds <- round(predict.glm(models[[1]],type="response",newdata = test))
               
                 # Get test set accuracy
                 accuracies[[1]] <- paste(toString(round(mean(test_preds == test$Survived)*100,1)),"%",sep="")

                 # Get model coefficients
                 coeffs <- round(coef(models[[1]]),2)
                 
                 params[[1]] <- paste(
                   "ln(p/(1-p)) = ", 
                   coeffs["(Intercept)"],
                   " + PClass", coeffs["Pclass"],
                   " + Age",  coeffs["Age"],
                   " + Sex",  coeffs["Sexmale"],
                   sep = "")
               
                 # Get model train size and test size
                 trainSize[[1]] <- as.integer(nrow(train))
                 testSize[[1]] <- as.integer(nrow(test))
               
                 # Get model train and test sets color squares
                 trainSet[[1]] <- paste0('<span class= \"text-inside-cell\" style=\"color:',html_colors()[[1]],'; font-size:20px;\">', "&#9632", '</span>')
                 testSet[[1]] <- paste0('<span class= \"text-inside-cell\" style=\"color:',html_colors()[[2]],';  font-size:20px\">', "&#9632", '</span>')
             
                 # add HTML formating to columns
                 number <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.integer(number),"</span>")
                 trainSize <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.integer(as.array(trainSize)),"</span>")
                 testSize <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.integer(as.array(testSize)),"</span>")
                 
                 accuracies <- ifelse(input$createModel < 5,
                                      paste0("<span class= \"text-inside-cell\" style=\"font-size:12px; color:red;\">",as.array(accuracies),"</span>"),
                                      accuracies <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px; color:black;\">",as.array(accuracies),"</span>")
                                      )
               
               # Put all columns into model information dataframe
               modelDataframe(data.frame(
                 "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Model # </span>" = number,
                 "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Train Set </span>" = as.array(trainSet),
                 "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Set </span>"= as.array(testSet),
                 "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Train Set Size </span>" = trainSize,
                 "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Set Size </span>" = testSize,
                 "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Accuracy </span>" = accuracies
                 #Parameters = as.array(params)
                 ,check.names = F)
               )})
  

  # Get Help Button Message
  observeEvent(input$getHelp, {
    shinyalert("Confused?", "
                            <div style='text-align: center;'>
                            Cross validation is a way to get a more <span style=\"font-weight:bold; \"> reliable </span>test accuracy of a model. <br> <br>
                                                                      

                            <span style=\"font-weight:bold; \"> Without Cross Validation </span> <br> 
               
                            The test accuracy of a model changes for different train test splits. <br> <br>
               
                            <span style=\"font-weight:bold; \"> With Cross Validation </span> <br> 
               
                            The average test accuracy varies considerably less. <br> <br> <br> 
               
                            Cross Validation Steps: <br>
                            <div style='text-align: left; display: inline-block;'>
                            <span style=\"font-size: 14px; line-height: 1;\">
                            1. Randomly divide the dataset into K sections <br>
                            2. Create K models that are trained on K-1 sections and tested on 1 section. <br>
                            3. Average the test accuracy of the K models to obtain a more reliable metric. <br> <br> <br>
                            </span> </div>
               
                            ",size = "m",html = T)})}

# Run RShiny Application
shinyApp(ui = ui, server = server)