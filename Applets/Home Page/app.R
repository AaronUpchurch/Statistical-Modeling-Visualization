# RShiny code for Home Page

# Imports
library(shiny)

# User Interface
ui <- fluidPage(
  
  # Title
  titlePanel(
    h1("STA 3180: Statistical Modeling Visualization Applets", align = "center",style = "font-family: mono; background-color:white; border:2px solid black")),
  
  # Authors
  h5("By",
    tags$a(href="https://www.linkedin.com/in/aaron-upchurch/", 
           "Aaron Upchurch"),
    "and",
    tags$a(href="https://www.linkedin.com/in/max-logalbo/", 
           "Max LoGalbo"),
    align="center",style = "font-family: mono;"),
  
  # App Image Links
  fluidRow(
  
    # backgorund color
    tags$style('.container-fluid {background-color: #f0f0f0;}'),
    
    # Left column
    column(6,align="center",
           
           # Logistic Regression
           h2("Logistic Regression",style = "font-family: mono;"),
           tags$a(href="https://aaronupchurch.shinyapps.io/logistic_regression/",img(src='cross_validation.png',width=500,style = "border: 2px solid black; border-radius: 10px;")),
           
           # Cross Validation
           h2("Cross Validation",style = "font-family: mono;"),
           tags$a(href=" https://aaronupchurch.shinyapps.io/cross_validation/",img(src='cross_validation.png',width=500, style = "border: 2px solid black; border-radius: 10px;")),
           ),
    
    # Right Column
    column(6,align="center",
           
           # Contingency Tables
           h2("Contingency Table",align="center",style = "font-family: mono;"),
           tags$a(href="https://aaronupchurch.shinyapps.io/contingency_table/",img(src='bootstrapping.png',width=500,style = "border: 2px solid black; border-radius: 10px;")),
           
           # Bootstrapping
           h2("Bootstrapping",align="center",style = "font-family: mono;"),
           tags$a(href=" https://aaronupchurch.shinyapps.io/bootstrapping/",img(src='bootstrapping.png',width=500,style = "border: 2px solid black; border-radius: 10px;")),
           
         ),
  ),
  

  h5("Source Code:",
     tags$a(href="https://github.com/AaronUpchurch/Statistical-Modeling-Visualization", 
            "GitHub"),),
  
  
  
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
}

library(shiny)


shinyApp(ui = ui, server = server)