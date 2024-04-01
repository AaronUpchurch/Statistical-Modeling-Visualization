#======== Imports ===========
library(shiny)
library(shinyalert)



#======= Dataset Reprocessing ========

# Read dataset
titanic_df <- read.csv("datasets/titanic.csv")

# Extract the last names
titanic_df$Lastname <- sapply(strsplit(titanic_df$Name, ", "), `[`, 1)
#titanic_df$Lastname <- sapply(titanic_df$Lastname,FUN=function(x){substr(x,1,3)})

# Remove duplicate last names
titanic_df <- titanic_df[!duplicated(titanic_df$Lastname),]

# Remove all rows with missing data
titanic_df <- na.omit(titanic_df)

# Downsample dataset to 144 rows
titanic_df <- titanic_df[sample(1:nrow(titanic_df),size=144),]




#=========== User Interface ======================
ui <- fluidPage(
  
  # Title
  titlePanel(h1("Cross Validation Applet", align = "center")),
  h5("By Aaron Upchurch", align = "center"),
  
  fluidRow(
    
    # Control Sidebar
    sidebarPanel(width = 2,
                 
                 h5(strong("1.Create Non-CV Models")),
                 actionButton("createModel","Create Model"),
                 
                 h5(strong("2.Show Cross Validation")),
                 checkboxInput("showCV", "Show CV", value = F, width = "800px"),
                 
                 
                 # K Slider Input
                 h5(strong("3. Select K")),
                 shinyWidgets::sliderTextInput(inputId = "k", 
                                               label = "", 
                                               choices = c(2,3,4,6,8,12,16,24,48,144)),
                 
                 # Run Model Buttons
                 h5(strong("4. Create CV Models")),
                 actionButton("createCVModel","Create CV Model"),
                 

                 # Get Help Button
                 h5(strong("Get Help")),
                 actionButton("getHelp","Get Help")
    ),
    
    # No Cross Validation Display
    column(5,
           
           # Column Title
           h4("No Cross Validation", align = "center"),
           
           # Name Dataset
           h5(strong("Dataset")),
           tableOutput("nameDataframe"),
           
           # Model Dataset
           h5(strong("Model Information")),
           tableOutput("modelDataframe"),
    ),
    
    # K Fold Cross Validation Display
    column(5,
           
           # Column Title
           h4("K Fold Cross Validation", align = "center"),
           
           # Dataset
           h5(strong("Dataset")),
           tableOutput("cvNameDataframe"),
           
           # Model Dataset
           h5(strong("Models")),
           tableOutput("cvModelDataframe"),
    ),
    
  ),
  tags$head(
    tags$style(
      HTML("
         .text-inside-cell {
            display: block;
            padding: 0px; /* Adjust the padding as needed */
            margin: -5px; /* Remove margin */
         }"))))




# Server Logic
server <- function(input, output,session) {
  
  #=========== Initial Popup ==================#
  showPopup <- reactiveVal(T)
  
  observeEvent(showPopup,{
    # Create smaller dataset for initial message
    small_titanic <- titanic_df[1:10,c("Lastname","Sex","Age","Pclass","Survived")]
    
    # Add HTML code to smaller dataset
    small_titanic <- apply(small_titanic,
                           MARGIN=2,
                           FUN=function(x){
                             paste0('<span style="font-size:14px">', x, '</span>')
                           })
    
    small_titanic <- apply(small_titanic,
                           MARGIN=2,
                           FUN = function(x){
                             paste0("<td>",x,"</td>")
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
                              <th> <span style=\"font-size:14px\"> Lastname </span> </th>
                              <th> <span style=\"font-size:14px\"> Sex </span> </th>
                              <th> <span style=\"font-size:14px\"> Age </span> </th>
                              <th> <span style=\"font-size:14px\"> Pclass </span> </th>
                              <th> <span style=\"font-size:14px\"> Survived </span> </th>
                           </tr>",
                        
                        small_titanic,
                        
                        "</table>
                            </center>")
    
    small_titanic <- gsub(',', '', small_titanic)
    shinyalert("Welcome",
               paste0("This is a RShiny applet designed to teach students about <span style=\"font-weight:bold; \"> cross validation </span> <br> <br>",
                      "Logistic regression models are train on the titanic survival dataset <br> <br>",
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
  modelDataframe <- reactiveVal(
    NA
  )
  
  # CV model information dataframe
  cvModelDataframe <- reactiveVal(
    NA
  )
  
  generate_html_color <- function() {
    sprintf("#%06X", sample(0:0xFFFFFF, 1))
  }
  
  # HTML colors
  html_colors <- reactiveVal(
    
    
    
    # Generate a list of 144 HTML colors
    replicate(144, generate_html_color())
    
  )
  
 
  

  #================ Outputs ======================
  
  # Dataset
  output$cvNameDataframe <- renderTable(colnames = F,bordered = T,width = "100px",{
    
    if(!input$showCV){
      return(NULL)
    }
    
    temp <- matrix(data = dataset$whole$Lastname, nrow = 12)
    
    

    

    
    add_color <- function(t){
      for(i in 1:input$k){
        if(t %in% dataset$splits[[i]]$Lastname){
          return(paste0('<span class="text-inside-cell" style="color:',html_colors()[[i]],'; font-size:10px">', t, '</span>'))
        }
      }
    }
    
    ret <- matrix(mapply(add_color,temp),nrow=24)

    
    return(ret)
  }, sanitize.text.function = function(x) x)
  
  # Dataset
  output$nameDataframe <- renderTable(colnames = F,bordered = T,width = "100px",{
    
    temp <- matrix(data = CVdataset$whole$Lastname, nrow = 12)
    
    
  
    
    
    
    add_color <- function(t){
      for(i in 1:2){
        if(t %in% CVdataset$splits[[i]]$Lastname){
          return(paste0('<span class="text-inside-cell" style="color:',html_colors()[[i]],'; font-size:10px">', t, '</span>'))
        }
      }
    }
    
    ret <- matrix(mapply(add_color,temp),nrow=24)
    
    
    return(ret)
  }, sanitize.text.function = function(x) x)
  
  
  # Model Dataset
  output$cvModelDataframe <- renderTable(colnames =T, bordered = T, {
    
    if(!input$showCV){
      return(NULL)
    }
    
    cvModelDataframe()
  }, sanitize.text.function = function(x) x)
  
  output$modelDataframe <- renderTable(colnames =T, bordered = T, {
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
                     accuracies[[i]] <- paste(toString(round(mean(test_preds == test$Survived)*100,0)),"%",sep="")
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
                 

                 print(trainSet[[1]])
                 
             
                  
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
                 
                 print(as.array(trainSet))

                 # Get model train and test sets
                
                 
                 
                 
                 cvModelDataframe(data.frame(
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Model # </span>" = number,
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Train Set </span>" = as.array(trainSet),
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Set </span>"= as.array(testSet),
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Train Set Size </span>" = trainSize,
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Set Size </span>" = testSize,
                   "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Accuracy </span>" = accuracies
                   #Parameters = as.array(params)
                   ,check.names = F)
                 )
         
                 
               })
  
  
  observeEvent(input$createModel,
               {
                 
                 # randomly reorder dataset
                 CVdataset$whole <- CVdataset$whole[(sample(1:nrow(CVdataset$whole),size = nrow(CVdataset$whole))),]
                 
                 
                 
                   train_rows <- as.array(sample( seq(1,nrow(CVdataset$whole)), as.integer(0.7*nrow(CVdataset$whole))))
                   test_rows <- as.array(setdiff(seq(1,nrow(CVdataset$whole)),train_rows))
                   
                   CVdataset$splits[[1]] <- CVdataset$whole[train_rows,]
                   CVdataset$splits[[2]] <- CVdataset$whole[test_rows,]
                   
                 
                 
                
                 
                 # Train Models
                 number <- c()
                 models <- c()
                 accuracies <- c()
                 params <- c()
                 trainSize <- c()
                 testSize <- c()
                 trainSet <- c()
                 testSet <- c()

                 # Get Numbers
                 number = c(1)
                 
                 
                 
                   
                   
                   
                 
                     train <- CVdataset$splits[[1]]
                     test <- CVdataset$splits[[2]]
                   

                   
                   
                   
                   # Create model
                   models[[1]] <- glm(Survived ~ Pclass + Age + Sex, data = train, family = binomial())
                   
                   # Get model test predictions
                   test_preds <- round(predict.glm(models[[1]],type="response",newdata = test))
                   
                   
                   # Get test set accuracy
                   accuracies[[1]] <- paste(toString(round(mean(test_preds == test$Survived)*100,0)),"%",sep="")

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
                   
                   # Get model train and test sets
                   trainSet[[1]] <- paste0('<span class= \"text-inside-cell\" style=\"color:',html_colors()[[1]],'; font-size:20px;\">', "&#9632", '</span>')
                   testSet[[1]] <- paste0('<span class= \"text-inside-cell\" style=\"color:',html_colors()[[2]],';  font-size:20px\">', "&#9632", '</span>')
                   
                 
                 
                  # add formating
                   number <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.integer(number),"</span>")
                   trainSize <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.integer(as.array(trainSize)),"</span>")
                   testSize <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.integer(as.array(testSize)),"</span>")
                   accuracies <- paste0("<span class= \"text-inside-cell\" style=\"font-size:12px;\">",as.array(accuracies),"</span>")
                   
            
                 
        
                 
                   modelDataframe(data.frame(
                     "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Model # </span>" = number,
                     "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Train Set </span>" = as.array(trainSet),
                     "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Set </span>"= as.array(testSet),
                     "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Train Set Size </span>" = trainSize,
                     "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Set Size </span>" = testSize,
                     "<span class= \"text-inside-cell\" style=\"font-size:12px;\"> Test Accuracy </span>" = accuracies
                     #Parameters = as.array(params)
                     ,check.names = F)
                   )
                 
                 
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