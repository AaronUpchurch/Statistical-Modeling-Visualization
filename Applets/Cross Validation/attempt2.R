#======== Imports ===========
library(shiny)
library(shinyalert)

#======= Dataset Reprocessing ========

# Read dataset
titanic_df <- read.csv("datasets/titanic.csv")

split_names <- strsplit(titanic_df$Name, ", ")

# Extract the last names
titanic_df$Lastname <- sapply(split_names, `[`, 1)

# Remove duplicate last names
titanic_df <- titanic_df[!duplicated(titanic_df$Lastname),]

# Remove all rows with missing data
titanic_df <- na.omit(titanic_df)

# Downsample dataset to 144 rows
titanic_df <- titanic_df[sample(1:nrow(titanic_df),size=144),]

# Create smaller dataset for initial message
small_titanic <- titanic_df[1:10,c("Lastname","Sex","Age","Pclass","Survived")]

# Add HTML code to table
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

# User Interface
ui <- fluidPage(
  
  useShinyalert(),
  
  # Title
  titlePanel(h1("Cross Validation Applet", align = "center")),
  h5("By Aaron Upchurch", align = "center"),
  
  fluidRow(
    
    # Control Sidebar
    sidebarPanel(width = 2,
                 
                 # K Slider Input
                 shinyWidgets::sliderTextInput(inputId = "k", 
                                               label = "1. Select K", 
                                               choices = c(1,2,3,4,6,8,12,16,24,48,144)),
                 
                 # Run Model Buttons
                 h5(strong("2. Create Models")),
                 actionButton("runModel", "Run Model"),
                 actionButton("run100Model", "Run 100 Model"),
                 
                 # Get Help Button
                 h5(strong("Get Help")),
                 actionButton("getHelp","Get Help")
    ),
    
    # No Cross Validation Display
    column(10,
           
           # Column Title
           h4("Simulations", align = "center"),
           
           # Dataset
           h5(strong("Dataset")),
           tableOutput("dataset"),
           
           # Model Dataset
           h5(strong("Models")),
           tableOutput("modelDataset"),
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
  
  #=========== nitial Popup ==================#
  showPopup <- reactiveVal(T)
  
  observeEvent(showPopup,{
    shinyalert("Welcome",
               paste0("This is a RShiny applet designed to teach students about <span style=\"font-weight:bold; \"> cross validation </span> <br> <br>",
                      "Logistic regression models are train on the titanic survival dataset <br> <br>",
                      small_titanic), 
               size = "m", html = T)
    showPopup(F)
  })
  
  #=========== Reactive Values ================#
  dataset <- reactiveValues(
    whole = titanic_df[(sample(1:nrow(titanic_df),size = nrow(titanic_df))),],
    splits = c()
  )
  
  modelDataset <- reactiveVal(
    NA
  )
  

  #================ Outputs ======================
  
  # Dataset
  output$dataset <- renderTable(colnames = F,bordered = T,width = "100px",{
    
    temp <- matrix(data = dataset$whole$Lastname, nrow = 12)
    
    
    generate_html_color <- function() {
      sprintf("#%06X", sample(0:0xFFFFFF, 1))
    }
    
    # Generate a list of 144 HTML colors
    html_colors <- replicate(144, generate_html_color())
    
    todo <- ifelse(input$k==1,2,input$k)
    
    
    add_color <- function(t){
      for(i in 1:todo){
        if(t %in% dataset$splits[[i]]$Lastname){
          return(paste0('<span class="text-inside-cell" style="color:',html_colors[[i]],'; font-size:10px">', t, '</span>'))
        }
      }
    }
    
    ret <- matrix(mapply(add_color,temp),nrow=12)

    
    return(ret)
  }, sanitize.text.function = function(x) x)
  
  
  # Model Dataset
  output$modelDataset <- renderTable(colnames =T, bordered = T, {
    modelDataset()
  })
  
  
  #============== Observations ======================
  

  
  observeEvent(input$k,
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
                     trainSet[[i]] <- paste(setdiff(1:input$k,i),collapse = ",")
                     testSet[[i]] <- toString(round(i,0))
                   }
                 

                #append accuracy average row
                 number <- append(c("Average"),number)
                 trainSet <- append(c(""),trainSet)
                 testSet <- append(c(""),testSet)
                 trainSize <- append(c(""),trainSize)
                 testSize <- append(c(""),testSize)
                 accuracies <- append(c(toString(round(mean(as.numeric(accuracy_ints))))),accuracies)
                 params <- append(c(""),params)
                 
                 
                 
                  
                 
                  modelDataset(data.frame(
                    Number = number,
                    TrainSets = as.array(trainSet),
                    TestSets = as.array(testSet),
                    TrainSize = as.array(trainSize),
                    TestSize = as.array(testSize),
                    Accuracy = as.array(accuracies),
                    Parameters = as.array(params))
  

                    
                  )
         
                 
               })
  
  
  observeEvent(input$runModel,{
    
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
      trainSet[[i]] <- paste(setdiff(1:input$k,i),collapse = ",")
      testSet[[i]] <- toString(round(i,0))
    }
    
    
    
    
    
    
    
    modelDataset(data.frame(
      Number = number,
      TrainSets = as.array(trainSet),
      TestSets = as.array(testSet),
      TrainSize = as.array(as.integer(trainSize)),
      TestSize = as.array(as.integer(testSize)),
      Accuracy = as.array(accuracies),
      Parameters = as.array(params))
      
      
      
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