# RShiny script for Bootstrapping Applet

# Global Variables


# ============ Imports ==================#
library(shiny)
library(ggplot2)

#============= user Interface ==============#
ui <- fluidPage(
  
  
  #------- Title --------------------#
  titlePanel(h1("Bootstrapping Applet",
                align = "center")),
  
  #-------- Control Panel -------------
  fluidRow(sidebarPanel(
    
    
    # Distribution Selection
    selectInput(inputId = "distribution",
                label = "1. Select Population Distribution",
                choices = c("Normal" = "normal",
                            "Chi Squared" = "chi_squared",
                            "Binomial" = "binomial",
                            "Proportion" = "proportion"),
    ),
    
    # Metric Selection
    selectInput(inputId = "metric",
                label = "2. Select Metric",
                choices = c("Mean" = "mean",
                            "Variance" = "variance",
                            "Standard Deviation" = "standard deviation",
                            "Median" = "median"
                ),
    ),
    
    # Generate Sample Button
    h5(strong("3. Draw Sample from Population")),
    actionButton("getSample", "Generate Sample"),
    
    # Generate Bootsrap Button
    h5(strong("4. Draw Bootstrap from Sample")),
    actionButton("getBootstrap", "Generate Bootstrap Sample"),
    
    checkboxInput("showAnimation", "Show Animation", value = T, width = "800px"),
    
    h5(strong("5. Draw 100 Bootstraps from Sample")),
    actionButton("get100Bootstrap", "Generate 100x Bootstrap Samples"),
    
    
    # Show Bootstrap Confidence Interval Checkbox
    h5(strong("6. Get Bootstrap Confidence Interval")),
    checkboxInput("showCI", "Show Confidence Interval", value = FALSE, width = "800px"),
    
    
    # Show Get Help Explanatation
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
         fluidRow(
           column(6,align = "center",
                  h5(strong("Sample")),
                  tableOutput("sample_table"),
                  textOutput("sample_mean"),
           ),
           
           # Bootstrapp Table
           
           column(6,align = "center",
                  h5(strong("Bootstrap Sample")),
                  tableOutput("bootstrap_table"),
                  span(textOutput("bootstrap_mean"),style="color:red"),
           )),
         
         # Bootstrapp Dot Plot
         
         h5(strong(textOutput("metric"))),
         plotOutput("bootstrap_dotplot", width = "75%",
                    height = "200px"),
         
         
         #textOutput("confidenceInterval"),
         side_by_side_text <- div(
           style = "display: flex; flex-direction: row;justify-content: center;",
           span(style = "color: black;", textOutput("ci_begin")),
           span(style = "color: green;", textOutput("ci_lower")),
           span(style = "color: black;", textOutput("ci_and")),
           span(style = "color: green;", textOutput("ci_upper"))
           
         ),
         
         
  )
  )
  
)

#============== Server Functions ===================
server <- function(input, output,session) {
  
  
  #---------- Population Distribution ---------------$
  
  # colors for population distribution
  colors = c("normal" = "red",
             "chi_squared" = "blue",
             "binomial" = "green",
             "proportion" = "orange"
  )
  
  # data for population distribution
  data=data.frame(
    normal=rnorm(100000),
    chi_squared = rchisq(100000, df = 1),
    binomial = rbinom(100000, size = 10, prob = 0.5),
    proportion = rbinom(100000, size = 10, prob = 0.5) / 10
  )
  
  # population distribution histogram
  output$DistributionHistogram <- renderPlot({
    
    p <- ggplot(data, aes_string(x = input$distribution))+
      geom_density(alpha=.2, fill=colors[input$distribution], bw = 0.8) +
      xlab(paste("\u03bc =",
                 round(mean(data[[input$distribution]]),2),
                 "\u03c3 =",
                 round(sd(data[[input$distribution]]),2))) +
      theme_linedraw() +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,0.5))
    
    
    return(p)
    
  })
  
  #--------- Sample Table ----------------#
  output$sample_table <- renderTable(colnames = F,bordered = T,{
    if(input$getSample[1]==0){
      matrix(data = rep("\U00A0 \U00A0 \U00A0",49), nrow = 7)
    }
    else{
      matrix(data = my_sample(), nrow = 7)
      
    }
  })
  
  output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
    if(input$getBootstrap[1]==0){
      matrix(data = rep("\U00A0 \U00A0 \U00A0",49), nrow = 7)
    }
    else{
      matrix(data = my_bootstrap(), nrow = 7)
    }
  })
  
  
  
  my_sample <- eventReactive(
    input$getSample,
    {
      
      #  clear boostrap and dotplots plots
      output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
        matrix(data = rep("\U00A0 \U00A0 \U00A0",49), nrow = 7)
        
      })
      bootstrap_means$data <- c()
      
      start <- sample.int(100000,1)
      output$sample_mean <- renderText({paste("\u03bc =",
                                              round(mean(my_sample()),2),
                                              "\u03C3",
                                              round(sd(my_sample()),2)
      )})
      
      
      output$sample_table <- renderTable(colnames = F,bordered = T,{
        
        
        temp <- matrix(data = data[[input$distribution]][start:(start+48)], nrow = 7)
        
        
        print(temp)
        print(round(last_added_value$value,2))
        
      
        
          
          ret <- ifelse(round(temp,3) == round(last_added_value$value,3), 
                        paste0('<span style="color:blue; font-weight:bold;">', round(temp,2), '</span>'), 
                        round(temp,2))
          

        
        
        
        
        return(ret)
        
        
        
      }, sanitize.text.function = function(x) x)
      
      data[[input$distribution]][start:(start+48)]
      
      
    }
  )
  
  
  
  
  #--------- Bootstrap Table -------------#
  
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
        matrix(data = my_bootstrap(), nrow = 7)
      })
      
      
      
      return(s)
    }
  )
  
  
  #---------- Get 100 Bootstraps -------------- #
  observeEvent(input$get100Bootstrap, {
    for (i in 1:100){
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
    
    
    
    
  })
  
  
  
  
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
    
    
    if(input$showCI){
      index_2.5 <- max(as.integer(0.025*length(bootstrap_means$data)),1)
      index_97.5 <- as.integer(0.975*length(bootstrap_means$data))
      
      percentile_2.5$data <- sort(bootstrap_means$data)[index_2.5]
      percentile_97.5$data <- sort(bootstrap_means$data)[index_97.5]
      
      dot_colors[which(bootstrap_means$data == percentile_2.5$data)[1]] = 'Percentile'
      dot_colors[which(bootstrap_means$data == percentile_97.5$data)[1]] = 'Percentile'
      
      
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
    
    
    d <- data.frame(x = bootstrap_means$data, fill = dot_colors)
    category_colors <- c("Bootstraps" = "blue", "Most Recent" = "red", "Percentile" = "Green")
    
    
    if(length(bootstrap_means$data) == 1){
      ggplot(d, aes(x = x, fill = fill)) +
        scale_fill_manual(values = category_colors)+
        geom_dotplot(stackgroups = T,method = "histodot", show.legend = F, dotsize = 0.5)+
        theme_linedraw() + xlim(0,2*bootstrap_means$data[1])
    }
    else{
    
    ggplot(d, aes(x = x, fill = fill)) +
      scale_fill_manual(values = category_colors)+
      geom_dotplot(stackgroups = T,method = "histodot", show.legend = F, dotsize = 0.5)+
      theme_linedraw()
    }
  })
  
  #--------- Animation ----------------------------------#
  
  last_added_value <- reactiveValues(value = -1000)
  
  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(1000, session)
    
    
    if(input$showAnimation){
      n <- max(round(Sys.time() - button_click_time$data)  ,round(Sys.time()-Sys.time()))
      if(n > length(my_bootstrap())){n <- length(my_bootstrap())}
      
      
      if(n > 0){
        
        subset_bootstrap <- append(round(my_bootstrap()[1:n],2), rep("\U00A0 \U00A0 \U00A0",length(my_bootstrap())-n))
      
        last_added_value$value <- my_bootstrap()[n]
        print(last_added_value$value)
        
        
        output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
          
          
          
          temp <- matrix(data = subset_bootstrap, nrow = 7)
          
   

          ret <- ifelse(temp == round(last_added_value$value,2), 
                 paste0('<span style="color:blue; font-weight:bold;">', temp, '</span>'), 
                 temp)
          

          return(ret)
          
          
          
        }, sanitize.text.function = function(x) x)
        
        
    
        
        
        
      }
      else{
        output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
          matrix(data = rep("\U00A0 \U00A0 \U00A0",length(my_bootstrap())), nrow = 7)
        })
      }
    }
    
  }
  )
  
  output$metric <- renderText({paste("Bootstrap ", input$metric,"s", sep = "")})
  
  
  #-------- if click on show confidence interval --------#
  observeEvent(input$showCI,
               
               
               {
                 
                 if(input$showCI){
                   output$ci_begin <- renderText({paste("We are 95% confident that the population ",input$metric,"is between \U00A0")})
                   output$ci_lower <- renderText({round(percentile_2.5$data,2)})
                   output$ci_and <- renderText({"\U00A0 and \U00A0"})
                   output$ci_upper <- renderText({round(percentile_97.5$data,2)})
                 }
                 else{
                   output$ci_begin <- renderText({""})
                   output$ci_lower <- renderText({""})
                   output$ci_and <- renderText({""})
                   output$ci_upper <- renderText({""})
                 }
               }
  )
  
  
  #---------- Other -----------------------------------#
  
  # reset tables and plots if new distribution chosen
  observeEvent(input$distribution, {
    
    
    output$bootstrap_mean <- renderText({""})
    
    
    
    
    output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = rep("\U00A0 \U00A0 \U00A0",49), nrow = 7)
      
    })
    
    bootstrap_means$data <- c()
  })
  
  observeEvent(input$metric, {
    
    
    
    output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = rep("\U00A0 \U00A0 \U00A0",49), nrow = 7)
      
    })
    
    bootstrap_means$data <- c()
  })
  
  
  #-- Help ----------------------------
  observeEvent(input$showHelp, {
    message <- "
               
               
    Bootstrapping generates new samples by resampling from the original observed data with replacement. <br> <br>
    A distribution of the statistic of interest is then created from these simulated samples. <br> <br>
    The confidence interval bounds are then derived from the 2.5% percentile and 97.5% of the distribution."
    showNotification(ui = HTML(message),
                     duration = 120,
                     type = "message")
  })
  
  
  
  
  
  
}

shinyApp(ui, server)