# Functions used to run ordinary and cross validated logistic models

get_titanic_data <- function(){
  
  # import libraries
  library(dplyr)
  library(caret)
  
  # read in Titanic survival dataset
  print(getwd())
  titanic_df <- read.csv("data/titanic.csv")
  
  # remove all rows with missing values
  titanic_df <- na.omit(titanic_df)
  
  # shuffle dataset
  titanic_df <- titanic_df %>% dplyr::sample_frac(1)
  
  # make binary response variable a factor
  titanic_df$Survived <- as.factor(titanic_df$Survived)

return(titanic_df[0:300,])

}


run_ordinary_models <- function(num_models){
  
  titanic_df <- get_titanic_data()
  
  ordinary_models <- train(
                                  # Model Equation
                                  Survived ~ Pclass + Sex + Age,
                                  
                                  # Model Dataset
                                  data = titanic_df, 
                                  
                                  # Train Test Split
                                  trControl = trainControl(
                                                           # Leave Group Out Cross Validation
                                                           method = "LGOCV",
                                                           
                                                           # Proportion of train set
                                                           p = 0.8,
                                                           
                                                           # Number of models
                                                           number = num_models),
                                  # Generalized Linear Model               
                                  method="glm", 
                                  
                                  # Logistic Regression
                                  family=binomial)
  
  #* LGOCV is just regular train/test split
  
  # get model accuracys
  ordinary_model_accuracys <- ordinary_models$resample$Accuracy
  
  return(list(coef(ordinary_models$finalModel), ordinary_model_accuracys))

}


run_cv_models <- function(num_models){
  
  titanic_df <- get_titanic_data()

  cv_models <- train(
                  
                  # Model Equation
                  Survived ~ Pclass + Sex + Age,
                  
                  # Model Dataset
                  data = titanic_df, 
                  
                  # Train Test Split
                  trControl = trainControl(
                                          # Do Cross validation Multiple Times
                                          method = "repeatedcv", 
                                          
                                          # Number of Folds
                                          number = 3,
                                          
                                          # Number of Repetitions
                                          repeats = num_models), 
                  
                 # Generalized Linear Model
                 method="glm", 
                 
                 # Logistic Regression
                 family=binomial)
  
  
  # get model results
  cv_model_results <- cv_models$resample
  
  # organize model results by its repetition number
  cv_model_results[["Sample"]] <- substr(cv_model_results$Resample, 7, 15)
  
  # average model accuracys for each repetition
  cv_average_accuracys <- cv_model_results %>%
    group_by(Sample) %>%
    summarise_at(vars(Accuracy), list(Average = mean))
  
  cv_average_accuracys <- cv_average_accuracys$Average
  
  return(list(coef(cv_models$finalModel), cv_average_accuracys))

}




