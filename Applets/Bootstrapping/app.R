# RShiny script for Bootstrapping Applet

# ============ Imports ==================#
library(shiny)
library(ggplot2)
library(shinyjs)
library(stringr)
library(shinyalert)


#========== User Interface ==============#
ui <- fluidPage(
  
  
  useShinyalert(),
  shinyjs::useShinyjs(),
  
  #------- Title --------------------#
  titlePanel(h1("Bootstrapping Applet", align = "center")),
  
  h5("By Aaron Upchurch", align = "center"),
  
  #-------- Control Panel -------------
  fluidRow(sidebarPanel(
    
      # Distribution Selection
      selectInput(inputId = "distribution",
                  label = "1. Select Population Distribution",
                  choices = c("Normal" = "normal",
                              "Chi Squared" = "chi_squared",
                              "Binomial" = "binomial",
                              "Horseshoe - Weight" = "horseshoe_weight",
                              "Horseshoe - Width" = "horseshoe_width"),),
      
      # Metric Selection
      selectInput(inputId = "metric",
                  label = "2. Select Metric",
                  choices = c("Mean" = "mean",
                              "Variance" = "variance",
                              "Standard Deviation" = "standard deviation",
                              "Median" = "median"),),
      
      # Generate Sample Button
      h5(strong("3. Draw Sample from Population")),
      actionButton("getSample", "Generate Sample"),
      
      # Generate Bootstrap Button
      h5(strong("4. Draw Bootstrap from Sample")),
      actionButton("getBootstrap", "Generate Bootstrap Sample"),
      
      # Show animation check box
      checkboxInput("showAnimation", "Slow Animation", value = T, width = "800px"),
      
      # Draw 100 Bootstraps button
      h5(strong("5. Draw 300 Bootstraps from Sample")),
      actionButton("get100Bootstrap", "Generate 300 Bootstrap Samples"),
      
      # Show Bootstrap Confidence Interval Checkbox
      h5(strong("6. Get Bootstrap Confidence Interval")),
      checkboxInput("showCI", "Show Confidence Interval", value = FALSE, width = "800px"),
      
      # Show Get Help Button
      actionButton("showHelp", "Get Help")
  ),
  
  
  #----------------- Graph Panel ------------------
  column(8, align = "center",
         
       # Population Histogram
       h5(strong("Population Distribution")),
       plotOutput(outputId = "DistributionHistogram",
                  width = "75%",
                  height = "200px"),
       
       # Sample table
       fluidRow(column(5,align = "center",
                h5(strong("Sample")),
                tableOutput("sample_table"),
                textOutput("sample_mean"),),
                
       column(2,align = "center",
          
              div(
                style = "position: absolute; bottom: -130px; right: 60px;",
                icon("right-long","fa-3x")
              )
             ),
                
         
       # Bootstrap Table
       column(5,align = "center",
              h5(strong("Bootstrap Sample")),
              tableOutput("bootstrap_table"),
              span(textOutput("bootstrap_mean"),style="color:red; font-weight: bold; font-size: 13px"),)),
       
       # Bootstrap Dot Plot
       h5(strong(textOutput("metric")),align = "center"),
       plotOutput("bootstrap_dotplot", width = "75%",
                  height = "200px"),
       
       # Confidence Interval Text
       side_by_side_text <- div(
         style = "display: flex; flex-direction: row;justify-content: center;",
         span(style = "color: black; font-size: 17px;", textOutput("ci_begin")),
         span(style = "color: limegreen; font-size: 17px;", textOutput("ci_lower")),
         span(style = "color: black; font-size: 17px;", textOutput("ci_and")),
         span(style = "color: limegreen;font-size: 17px;", textOutput("ci_upper"))),
)))

#============== Server Functions ===================
server <- function(input, output,session) {
  
  #------ Initial Popup -----------------#
  showPopup <- reactiveVal(T)
  
  observeEvent(showPopup,{
    shinyalert("Welcome", "This is a RShiny application to help students learn about bootstrapping.
               
                          If you need help, press the 'Get Help' button below.", 
               size = "m")
    showPopup(F)
  })
  
  
  #---------- Population Distribution ---------------$
  
  # colors for population distribution
  colors = c("normal" = "red",
             "chi_squared" = "blue",
             "binomial" = "green",
             "horseshoe_weight" = "orange",
             "horseshoe_width" = "purple")
  
  # data for population distribution
  data_distributions= list(
    normal=rnorm(100000),
    chi_squared = rchisq(100000, df = 1),
    binomial = rbinom(100000, size = 10, prob = 0.5),
    horseshoe_width = read.csv("datasets/horseshoecrabs.csv")$width,
    horseshoe_weight = read.csv("datasets/horseshoecrabs.csv")$weight)
  
  # add small variablility to make each point unique for blue color coding later
  data_distributions[["horseshoe_width"]] = data_distributions[["horseshoe_width"]] + rnorm(n = length(data_distributions[["horseshoe_width"]]), sd = 0.0001)
  data_distributions[["horseshoe_weight"]] = data_distributions[["horseshoe_weight"]] + rnorm(n = length(data_distributions[["horseshoe_weight"]]), sd = 0.0001)
  
  # population distribution histogram
  output$DistributionHistogram <- renderPlot({
    
    a <- as.data.frame(data_distributions[[input$distribution]])
    
    colnames(a) <- input$distribution
    

    p <- ggplot(a, aes_string(x = input$distribution))+
      geom_density(alpha=.2, fill=colors[input$distribution], bw = 0.8) +
      xlab(paste("\u03bc =",
                 round(mean(data_distributions[[input$distribution]]),2),
                 "\u03c3 =",
                 round(sd(data_distributions[[input$distribution]]),2))) +
      theme_linedraw() +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
    
    
    return(p)
    
  })
  
  #--------- Sample Table ----------------#
  output$sample_table <- renderTable(colnames = F,bordered = T,{
    if(input$getSample[1]==0){
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)
    }
    else{
      matrix(data = round(my_sample(),2), nrow = 6)
      
    }
  })
  
  output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
    if(input$getBootstrap[1]==0){
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)
    }
    else{
      matrix(data = my_bootstrap(), nrow = 6)
    }
  })
  
  # DONT DELETE
  my_sample <- reactiveVal(-99)
  
 
  
  my_sample <- eventReactive(
    input$getSample,
    {
      

      lock(F)
      
      #  clear boostrap and dotplots plots
      output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
        matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)
        
      })
      bootstrap_means$data <- c()
      
      
      temp <- matrix(data = sample(data_distributions[[input$distribution]],36), nrow = 6)
      
      
      output$sample_table <- renderTable(colnames = F,bordered = T,{
        
        
        
        
        
      
        
          
          ret <- ifelse(round(temp,6) == round(last_added_value$value,6), 
                        paste0('<span style="color:blue; font-weight:bold; font-size=12px">', round(temp,2), '</span>'), 
                        round(temp,2))
        
          

        
        
        
        
        return(ret)
        
        
        
      }, sanitize.text.function = function(x) x)
      
      return(temp)
      
    }
  )
  
  observeEvent(input$getSample,{
    output$sample_mean <- renderText({paste("\u03bc =",
                                            round(mean(my_sample()),2),
                                            "s =",
                                            round(sd(my_sample()),2)
    )})})
  
  
  
  
  
  #--------- Get Bootstrap -------------#
  button_click_time <- reactiveValues(data = Sys.time())
  
  
  
  my_bootstrap <- eventReactive(
    input$getBootstrap,
    {
      
      

      s <- sample(my_sample(), replace = T)
      

      button_click_time$data <- Sys.time()
      
      function_map <- list(
        "mean" = mean,
        "variance" = var,
        "standard deviation" = sd,
        "maximum" = max,
        "minimum" = min,
        "median" = median
      )
      
      metric_bootstrap_calculation <- do.call(function_map[[input$metric]],list(x = s))
      output$bootstrap_mean <- renderText({paste(input$metric,"=",round(metric_bootstrap_calculation,2))})
      
      output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
        matrix(data = my_bootstrap(), nrow = 6)
      })
      

      return(s)
    }
  )
  
  
  
  
  #---------- Get 100 Bootstraps -------------- #

  
  observeEvent(input$get100Bootstrap, {
    
    # Click on getBootstrap button to trigger animation
    shinyjs::click("getBootstrap")

  
    
    # Generate 200 Bootstrap Sample
    for (i in 1:299){
      
      s <- sample(my_sample(), replace = T)
      
      function_map <- list(
        "mean" = mean,
        "variance" = var,
        "standard deviation" = sd,
        "maximum" = max,
        "minimum" = min,
        "median" = median
      )
      
      metric_bootstrap_calculation <- do.call(function_map[[input$metric]],list(x = s))
      bootstrap_means$data <- c(bootstrap_means$data,metric_bootstrap_calculation)
    }
    
    # show new bootstrap table for continuity
    output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = my_bootstrap(), nrow = 6)
    })})
  
  
 
  
  
  #--------- Bootstrap Dotplot --------------- #
  bootstrap_means <- reactiveValues(data = c())
  percentile_2.5 <- reactiveValues(data = c())
  percentile_97.5 <- reactiveValues(data = c())
  
  
  
  observeEvent(input$getBootstrap, {
    function_map <- list(
      "mean" = mean,
      "variance" = var,
      "standard deviation" = sd,
      "maximum" = max,
      "minimum" = min,
      "median" = median
    )
    metric_bootstrap_calculation <- do.call(function_map[[input$metric]],list(x = my_bootstrap()))
    bootstrap_means$data <- c(bootstrap_means$data, metric_bootstrap_calculation)
  })
  
  
  
  output$bootstrap_dotplot <- renderPlot({
    
    
    dot_colors <- rep('Bootstraps', length(bootstrap_means$data))
    
    if(length(dot_colors) == 0){
      return(NULL)
    }
    
    
    if(input$showCI){
      
      # add slight noise to bootstrap means to allow for correct percentile identification
      #bootstrap_means$data = bootstrap_means$data + rnorm(n = length(bootstrap_means$data),sd = 0.0001)
      
      index_2.5 <- max(as.integer(0.025*length(bootstrap_means$data)),1)
      index_97.5 <- as.integer(0.975*length(bootstrap_means$data))
      
      percentile_2.5$data <- sort(bootstrap_means$data)[index_2.5]
      percentile_97.5$data <- sort(bootstrap_means$data)[index_97.5]
      
      if(length(bootstrap_means$data) >= 300){
      dot_colors[which(bootstrap_means$data == percentile_2.5$data)[1]] = '2.5th Percentile'
      dot_colors[which(bootstrap_means$data == percentile_97.5$data)[1]] = '97.5th Percentile'
      }
      
      
    }
    
    function_map <- list(
      "mean" = mean,
      "variance" = var,
      "standard deviation" = sd,
      "maximum" = max,
      "minimum" = min,
      "median" = median
    )
    metric_bootstrap_calculation <- do.call(function_map[[input$metric]],list(x = my_bootstrap()))
    
    dot_colors[which(bootstrap_means$data == metric_bootstrap_calculation)[1]] = "Most Recent"
    
    
    d <- data.frame(x = bootstrap_means$data, Fill = dot_colors)
    category_colors <- c("Bootstraps" = "black", "Most Recent" = "red", "2.5th Percentile" = "Green","97.5th Percentile" = "Green")
    
    
    if(length(bootstrap_means$data) == 1){
      ggplot(d, aes(x = x, fill = Fill)) +
        scale_fill_manual(values = category_colors)+
        geom_dotplot(stackgroups = T,method = "histodot", dotsize = 0.5)+
        theme_linedraw() + xlim(0,2*bootstrap_means$data[1])  + 
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
    }
    else{
    
    ggplot(d, aes(x = x, fill = Fill)) +
      scale_fill_manual(values = category_colors)+
      geom_dotplot(stackgroups = T,method = "histodot",  dotsize = 0.5)+
      theme_linedraw() + 
        xlab(paste("bootstrap ",input$metric,"s",sep="")) +
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) 

    }
  })
  
  #--------- Animation ----------------------------------#
  
  last_added_value <- reactiveValues(value = -1000)
  
  
  animation_speed <- reactiveVal(1)
  
  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(100, session)
    
    
    if(input$showAnimation){
      animation_speed(1)
    }
    else{
      animation_speed(10)
    }
      n <- max(round( (Sys.time() - button_click_time$data)*animation_speed())  ,round(Sys.time()-Sys.time()))

      if(n > length(my_bootstrap())){
        if(n >= 3600){
          n <- 0
        }
        else{
        n <- length(my_bootstrap())
        }
      }

      
      if(n > 0){
        
        subset_bootstrap <- append(round(my_bootstrap()[1:n],2), rep("\U00A0 \U00A0 \U00A0 \U00A0",length(my_bootstrap())-n))
      
        last_added_value$value <- my_bootstrap()[n]

        
        output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
          
          
          
          temp <- matrix(data = subset_bootstrap, nrow = 6)
          

          ret <- ifelse(temp == round(last_added_value$value,2), 
                 paste0('<span style="color:blue; font-weight:bold;">', temp, '</span>'), 
                 temp)
          

          return(ret)
          
          
          
        }, sanitize.text.function = function(x) x)
        
        
    
        
        
        
      }
      else{
        output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
          matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",length(my_bootstrap())), nrow = 6)
        })
      }
    #}
    
  }
  )
  
  output$metric <- renderText({paste("Bootstrap ", str_to_title(input$metric),"s", " Distribution",sep = "")})
  
  
  #-------- Show Confidence Interval --------#
  observeEvent(input$showCI,{
         
     # If Show CI box is checked, display text
     if(input$showCI){
       
       if(length(bootstrap_means$data) >= 300){
       output$ci_begin <- renderText({paste("We are 95% confident that the population ",input$metric,"is between \U00A0")})
       output$ci_lower <- renderText({round(percentile_2.5$data,2)})
       output$ci_and <- renderText({"\U00A0 and \U00A0"})
       output$ci_upper <- renderText({round(percentile_97.5$data,2)})
       }
       else{
         
           message <- "At least 300 bootstrap samples are required before making a confidence interval."
           showNotification(ui = HTML(message),
                            duration = 120,
                            type = "warning")
       }
     }
    
    # If Show CI box is not checked, display no text
     else{
       output$ci_begin <- renderText({""})
       output$ci_lower <- renderText({""})
       output$ci_and <- renderText({""})
       output$ci_upper <- renderText({""})
     }})
  
  
  lock <- reactiveVal(T)

  #------- Reset App if New Distribution ----------
  observeEvent(input$distribution, {
    
    lock(T)
    
    # reset button click time
    button_click_time$data <- Sys.time() - as.numeric(3600)
    

    #shinyjs::click("getSample")
    

    
    # Deselect Show CI Checkbox
    updateCheckboxInput(session, "showCI", value = F)
  
    # Delete CI Interpretation Text
    output$ci_begin <- renderText({""})
    output$ci_lower <- renderText({""})
    output$ci_and <- renderText({""})
    output$ci_upper <- renderText({""})
    
    
    # STRANGE ERROR HERE
    # Delete Sample Mean and Standard Deviation Text
    output$sample_mean <- renderText({""})
    
    # Delete Bootstrap Table Metric Text
    output$bootstrap_mean <- renderText({""})
    
    # Clear Bootstrap Table
    output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)})
    
    output$sample_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)})
    
    
    # Clear Bootstrap Data
    bootstrap_means$data <- c()
    
    })
  
  
  #------- Reset App if New Metric ---------
  observeEvent(input$metric, {
    
    # Clear Bootstrap Table
    output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)})
    
    # Clear Bootstrap Data
    bootstrap_means$data <- c()
  })
  
  
  #-- Get Help Message ---------------------
  observeEvent(input$showHelp, {
    shinyalert("Confused?", "
                            <div style='text-align: center;'>
                            Bootstrapping is a way to calculate statistics using 
                                                                                <span style=\"font-weight:bold; \"> simulations </span> 
                                                                                rather than 
                                                                                              <span style=\"font-weight:bold; \"> equations</span>.<br> <br>
               
               
                            <div style='text-align: center;'>
                            Follow the steps below to practice bootstrapping:<br> <br>
               
                            
               
                            <div style='text-align: left; display: inline-block;'>
                            <span style=\"font-size: 14px; line-height: 1;\">
                            1. Refresh the page <br>
                            2. Click <span style=\"text-decoration: underline;\"> Generate Sample</span> to draw a random sample from the normal population <br>
                            3. Click <span style=\"text-decoration: underline;\"> Generate Bootstrap Sample</span> to draw a bootstrap sample from the random sample <br>
                            <span style=\"font-size: 12px;\"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *Note how bootstrapping uses sampling with replacement and allows for duplicate values </span> <br> 
                            <span style=\"font-size: 12px;\"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *The mean of the bootstrap sample is shown in red</span> <br> 
                            4. Click on <span style=\"text-decoration: underline;\"> Generate 300 Bootstrap Samples</span> to draw 300 bootstrap samples <br>
                            5. Check the <span style=\"text-decoration: underline;\"> Show Confidence Interval</span> box to show the bootstrap confidence interval <br>  <br>
                            </span> 
                            </div>
                            <div style='text-align: center;'>
                            Bootstrapping can be used to create confidence intervals for other population distributions and metrics.
                            ",size = "m",html = T
               )})
  
  
  

  
  }

# Call Shiny App
shinyApp(ui, server)