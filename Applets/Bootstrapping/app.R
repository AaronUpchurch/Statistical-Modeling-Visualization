# RShiny script for Bootstrapping Applet

# ============ Imports ==================#
library(shiny)
library(ggplot2)
library(shinyjs)
library(stringr)
library(shinyalert)


#========== User Interface ==============#
ui <- fluidPage(
  
  # Enable use of shiny js package
  useShinyjs(),
  
  #------- Title ----------------------------------------#
  titlePanel(h1("Bootstrapping Applet", align = "center")),
  
  #------- Author -------------------------#
  h5("By Aaron Upchurch", align = "center"),
  
  #-------- Control Panel -------------
  fluidRow(sidebarPanel(
    
      h5(strong("Tutorial")),
      h5(uiOutput("tutorial_text")),
    
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
      checkboxInput("showAnimation", "Show Animation", value = T, width = "800px"),
      
      # Draw 100 Bootstraps button
      h5(strong("5. Draw 100 Bootstraps from Sample")),
      actionButton("get100Bootstrap", "Generate 100 Bootstrap Samples"),
      
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
                textOutput("sample_mean_stdev"),),
      
       # Black arrow
       column(2,align = "center",
              div(
                style = "position: absolute; bottom: -130px; right: 60px;",
                icon("right-long","fa-3x"))),
                
       # Bootstrap Table
       column(5,align = "center",
              h5(strong("Bootstrap Sample")),
              tableOutput("bootstrap_table"),
              span(textOutput("bootstrap_metric"),style="color:red; font-weight: bold; font-size: 13px"),)),
       
       # Bootstrap Dot Plot
       h5(strong(textOutput("bootstrap_dotplot_title")),align = "center"),
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
  
  #------- Tutorial Message -------------#
  output$tutorial_text <- renderUI({
    
    # Initial tutorial message
    if(input$getSample == 0){
    text <-  HTML("Start with a <u>normal</u> population distribution and a metric <u>mean</u>. <br> <br>
                  First, <b>draw a sample from the population<b>.")}
    
    # after create sample
    else if(input$getBootstrap == 0){
    text <-  HTML("Next, <b>draw a bootstrap from the sample<b>.")}
    
    # after creating first bootstrap
    else if(input$getBootstrap == 1){
    text <-  HTML("The animation shows the bootstrap being created by <u>sampling with replacement</u> from the sample.<br> <br>
                  The <span style=\'color:red\'> mean </span> of the bootstrap is shown in the bottom dotplot. <br> <br>
                  Generate another bootstrap.
                  ")}
    
    # after creating second bootstrap
    else if(input$getBootstrap < 5){
      text <-  HTML("The dotplot of bootstrap means will keep being filled as more bootstraps are created. <br> <br>
                    Keep generating bootstraps.
                  ")}
    
    else if(input$get100Bootstrap == 0){
      text <-  HTML("<b>Draw 100 bootstraps from the sample</b>.
                  ")}
    
    else if(input$showCI==F){
      text <-  HTML("Click on <b> show confidence interval</b> to create a confidence interval for the <u>population mean</u>.
                  ")}
    
    else{
      text <- HTML("The 95% confidence interval is created by taking the bootstrap means at the <u>2.5th</u> and <u>97.5th</u> percentile. <br> <br>
                   
                   Bootstrapping is useful because it calculates statistics with <u>simulations</u> instead of <u>equations</u>. <br> <br>
                   
                   Try bootstrapping with other population distributions and other metrics.")
    }
    
    
    
    
    
    
    # Create styles for text box
    box_style <- "width: 405px; height: auto;  background-color: #FFFFFF; border: 1px solid #2c3e50; display: flex; justify-content: center; align-items: center; padding: 10px;"
    text_style <- "color: black; font-size: 14px;"
    
    # add styles to text bos
    box_text <- tags$div(
      style = box_style,
      tags$span(style = text_style, text)
    )
  
    # Return the HTML text box
    box_text
  })
  
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
    
    # get selected population data
    population_data <- as.data.frame(data_distributions[[input$distribution]])
    
    # rename column name to selected distribution
    colnames(population_data) <- input$distribution
    
    # create population histogram
    population_histogram <- ggplot(population_data, aes_string(x = input$distribution))+
      geom_density(alpha=.2, fill=colors[input$distribution], bw = 0.8) +
      xlab(paste("\u03bc =",
                 round(mean(data_distributions[[input$distribution]]),1),
                 "\u03c3 =",
                 round(sd(data_distributions[[input$distribution]]),1))) +
      theme_linedraw() +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
    
    return(population_histogram)
  })
  
  #--------- Sample Table ----------------#
  output$sample_table <- renderTable(colnames = F,bordered = T,{
    
    # create sample table
    ifelse(input$getSample[1]==0,
      # if 'Get Sample' button has not been pressed, show blank table
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6),
      
      # if 'Get Sample' has been pressed, show sample table
      matrix(data = round(my_sample(),2), nrow = 6))
    }
  )
  
  #--------- Bootstrap Table ----------------#
  output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
    
    # create bootstrap table
    ifelse(input$getBootstrap[1]==0,
           
      # If 'Get Boostrap' button has not been pressed, show blank table
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6),
      
      # If 'Get Bootstrap' button has been pressed, show bootstrap table
      matrix(data = my_bootstrap(), nrow = 6))
  })
  
  #--------- Sample --------------------#
  
  # let my_sample be a reactive value for continuous updating
  my_sample <- reactiveVal(-99)
  
  # when 'Get Sample' is pressed, generate a sample from population
  my_sample <- eventReactive(
    input$getSample,
    {

      # clear bootstrap table and dotplot
      output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
        matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)
      })
      
      # clear bootstrap statistics list
      bootstrap_statistics$data <- c()
      
      # draw sample from population distribution
      sample_matrix <- matrix(data = sample(data_distributions[[input$distribution]],36), nrow = 6)
      
      # rendering sample table during bootstrap animation
      output$sample_table <- renderTable(colnames = F,bordered = T,{
          
          # if a number in the sample tables matches the current number in the boostrap animation, make it blue
          blue_sample_table <- ifelse(round(sample_matrix,6) == round(last_added_value$value,6), 
                        
                        # add blue font html code
                        paste0('<span style="color:blue; font-weight:bold; font-size=12px">', round(sample_matrix,2), '</span>'), 
                        
                        # keep all other numbers the same
                        round(sample_matrix,2))

        return(blue_sample_table)
      }, sanitize.text.function = function(x) x)
      
      return(sample_matrix)
      })
  
  #---------- Sample Mean and Standard Deviation ---------#
  
  # When 'Get Sample' is pressed, calculate the sample mean and standard deviation for plotting
  observeEvent(input$getSample,{
    output$sample_mean_stdev <- renderText({paste("\u03bc =",
                                            round(mean(my_sample()),2),
                                            "s =",
                                            round(sd(my_sample()),2))})})
  
  # Note: I'm not sure why I can't add this to the above eventReactive...
  
  
  #--------- Get Bootstrap -------------#
  
  # Reactive value for the last time to button was pressed
  # Used for animation 
  button_click_time <- reactiveValues(data = Sys.time())
  
  # Reactive value for the metric computed on the bootstrap
  metric_bootstrap_calculation <- reactiveVal()
  
  # If 'Get Bootstrap' button is pressed
  my_bootstrap <- eventReactive(
    input$getBootstrap,
    {
      

      # if sample has not been drawn yet, through error
      if(my_sample() == -99){
        showNotification(ui = "You must generate a sample before generating a bootstrap!",
                         duration = 120,
                         type = "error")
        return(NULL)
      }
      
      # Reset button click time to now
      button_click_time$data <- Sys.time()
      
      # Map of metric names to R functions
      function_map <- list(
        "mean" = mean,
        "variance" = var,
        "standard deviation" = sd,
        "median" = median
      )
      
      # Get new bootstrap
      new_bootstrap <- sample(my_sample(), replace = T)
      
      # Perform selected metric onto bootstrap
      metric_bootstrap_calculation(do.call(function_map[[input$metric]],list(x =  new_bootstrap)))
      
      return(new_bootstrap)
    }
  )
  
  #---------- Get 100 Bootstraps -------------- #

  observeEvent(input$get100Bootstrap, {
    
    # Click on getBootstrap button to trigger animation
    shinyjs::click("getBootstrap")
    
    # Generate 99 Bootstrap Sample
    for (i in 1:99){
      
      sample <- sample(my_sample(), replace = T)
      
      function_map <- list(
        "mean" = mean,
        "variance" = var,
        "standard deviation" = sd,
        "maximum" = max,
        "minimum" = min,
        "median" = median
      )
      
      metric_bootstrap_calculation2 <- do.call(function_map[[input$metric]],list(x = sample))
      bootstrap_statistics$data <- c(bootstrap_statistics$data,metric_bootstrap_calculation2)
    }
    
    # show new bootstrap table for continuity
    output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = sample, nrow = 6)
    })})
  

  #--------- Bootstrap Dotplot --------------- #
  
  # reactive values
  bootstrap_statistics <- reactiveValues(data = c())
  percentile_2.5 <- reactiveValues(data = c())
  percentile_97.5 <- reactiveValues(data = c())
  
  # render bootstrap dotplot title
  output$bootstrap_dotplot_title <- renderText({paste("Bootstrap ", str_to_title(input$metric),"s", " Distribution",sep = "")})
  
  # add last calculated bootstrap metric to dotplot list
  observeEvent(input$getBootstrap, {
    
    function_map <- list(
      "mean" = mean,
      "variance" = var,
      "standard deviation" = sd,
      "maximum" = max,
      "minimum" = min,
      "median" = median
    )
    metric_bootstrap_calculation2 <- do.call(function_map[[input$metric]],list(x = my_bootstrap()))
    bootstrap_statistics$data <- c(bootstrap_statistics$data, metric_bootstrap_calculation2)
  })
  
  
  
  output$bootstrap_dotplot <- renderPlot({
    
    
    dot_colors <- rep('Bootstraps', length(bootstrap_statistics$data))
    
    if(length(dot_colors) == 0){
      return(NULL)
    }
    
    
    if(input$showCI){
      
      # add slight noise to bootstrap means to allow for correct percentile identification
      #bootstrap_statistics$data = bootstrap_statistics$data + rnorm(n = length(bootstrap_statistics$data),sd = 0.0001)
      
      index_2.5 <- max(as.integer(0.025*length(bootstrap_statistics$data)),1)
      index_97.5 <- as.integer(0.975*length(bootstrap_statistics$data))
      
      percentile_2.5$data <- sort(bootstrap_statistics$data)[index_2.5]
      percentile_97.5$data <- sort(bootstrap_statistics$data)[index_97.5]
      
      if(length(bootstrap_statistics$data) >= 100){
      dot_colors[which(bootstrap_statistics$data == percentile_2.5$data)[1]] = '2.5th Percentile'
      dot_colors[which(bootstrap_statistics$data == percentile_97.5$data)[1]] = '97.5th Percentile'
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
    metric_bootstrap_calculation2 <- do.call(function_map[[input$metric]],list(x = my_bootstrap()))
    
    dot_colors[which(bootstrap_statistics$data == metric_bootstrap_calculation2)[1]] = "Most Recent"
    
    
    d <- data.frame(x = bootstrap_statistics$data, Fill = dot_colors)
    category_colors <- c("Bootstraps" = "black", "Most Recent" = "red", "2.5th Percentile" = "Green","97.5th Percentile" = "Green")
    
    
    if(length(bootstrap_statistics$data) == 1){
      ggplot(d, aes(x = x, fill = Fill)) +
        scale_fill_manual(values = category_colors)+
        geom_dotplot(stackgroups = T,method = "histodot", dotsize = 0.4)+
        theme_linedraw() + xlim(0,2*bootstrap_statistics$data[1])  + 
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
    }
    else{
    
    ggplot(d, aes(x = x, fill = Fill)) +
      scale_fill_manual(values = category_colors)+
      geom_dotplot(stackgroups = T,method = "histodot",  dotsize = 0.4)+
      theme_linedraw() + 
        xlab(paste("bootstrap ",input$metric,"s",sep="")) +
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) 

    }
  })
  
  #--------- Animation ----------------------------------#
  
  # Record last value added to the bootstrap table
  last_added_value <- reactiveValues(value = -1000)
  
  # set animation speed
  animation_speed <- reactiveVal(1)
  
  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(100, session)
    
    # Display bootstrap metric text
    output$bootstrap_metric <- renderText({
      ifelse(
      # if bootstrap exists and if the bootstrap animation has ended
      length(my_bootstrap()) > 0 && as.numeric((Sys.time() - button_click_time$data)*animation_speed()) >= 36,
      
      # display bootstrap metric
      paste(input$metric,"=",round(metric_bootstrap_calculation(),2)),
      
      # else display nothing
      "")})
    
    # change animation speed based on showAnimation check box
    ifelse(input$showAnimation,
      animation_speed(1),
      animation_speed(20)
    )
      
      # get index of currently added bootstrap value in table
      current_bootstrap_index <- round( (Sys.time() - button_click_time$data)*animation_speed())
      
      # ensure index is greater than 0
      current_bootstrap_index <- max(current_bootstrap_index,round(Sys.time()-Sys.time()))

      # ensure index is less than length of bootstrap
      if(current_bootstrap_index > length(my_bootstrap())){
        current_bootstrap_index <- length(my_bootstrap())
      }
      
      # if animation is current in progress
      if(current_bootstrap_index > 0){
        
        # if sample is blank, do nothing
        # For when population is changed during an animation
        if(length(bootstrap_statistics$data) == 0){
          return(NULL)
        }
        # create in progress animated bootstrap table
        #    will have some cells filled and some blank
        animated_bootstrap <- append(round(my_bootstrap()[1:current_bootstrap_index],2), rep("\U00A0 \U00A0 \U00A0 \U00A0",length(my_bootstrap())-current_bootstrap_index))
      
        # record the last added value
        last_added_value$value <- my_bootstrap()[current_bootstrap_index]

        # render bootstrap table for animation
        output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
          
          # create animated matrix
          animated_bootstrap_matrix <- matrix(data = animated_bootstrap, nrow = 6)
          
          # add blue font to last value added in matrix
          return_matrix <- ifelse(animated_bootstrap_matrix == round(last_added_value$value,2), 
                 paste0('<span style="color:blue; font-weight:bold;">', animated_bootstrap_matrix, '</span>'), 
                 animated_bootstrap_matrix)
      
          return(return_matrix)
          
        }, sanitize.text.function = function(x) x)
        
        
      }
      
      # if animation is not in progress, show blank table
      else{
        output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
          matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",length(my_bootstrap())), nrow = 6)
        })}})
  
  
  #-------- Show Confidence Interval --------#
  observeEvent(input$showCI,{
         
     # If Show CI box is checked, display text
     if(input$showCI){
       
       # If at least 100 bootstrap samples have been drawn, create 95% confidence interval
       if(length(bootstrap_statistics$data) >= 100){
       output$ci_begin <- renderText({paste("We are 95% confident that the population ",input$metric,"is between \U00A0")})
       output$ci_lower <- renderText({round(percentile_2.5$data,2)})
       output$ci_and <- renderText({"\U00A0 and \U00A0"})
       output$ci_upper <- renderText({round(percentile_97.5$data,2)})
       }
       
       # If less than 100 bootstrap samples have been draw, show warning
       else{
           showNotification(ui = "At least 100 bootstrap samples are required before making a confidence interval.",
                            duration = 120,
                            type = "warning")
       }}
    
    # If Show CI box is not checked, display no text
     else{
       output$ci_begin <- renderText({""})
       output$ci_lower <- renderText({""})
       output$ci_and <- renderText({""})
       output$ci_upper <- renderText({""})
     }})
  
  #------- Reset App if New Distribution Chosen ----------
  observeEvent(input$distribution, {
    
    # reset button click time
    button_click_time$data <- Sys.time() - as.numeric(3600)
    
    # Deselect Show CI Checkbox
    updateCheckboxInput(session, "showCI", value = F)
  
    # Delete CI Interpretation Text
    output$ci_begin <- renderText({""})
    output$ci_lower <- renderText({""})
    output$ci_and <- renderText({""})
    output$ci_upper <- renderText({""})
    
    # Delete Sample Mean and Standard Deviation Text
    output$sample_mean_stdev <- renderText({""})
    
    # Delete Bootstrap Table Metric Text
    output$bootstrap_metric <- renderText({""})
    
    # Clear Bootstrap Table
    output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)})
    
    output$sample_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)})
    

    
    # Clear Bootstrap Data
    bootstrap_statistics$data <- c()
    })
  
  
  #------- Reset App if New Metric Chosen ---------
  observeEvent(input$metric, {
    
    # Clear Bootstrap Table
    output$bootstrap_table <- renderTable(colnames = F,bordered = T,{
      matrix(data = rep("\U00A0 \U00A0 \U00A0 \U00A0",36), nrow = 6)})
    
    # Clear Bootstrap Data
    bootstrap_statistics$data <- c()
  })
  
  
  #-- Get Help Message ---------------------
  observeEvent(input$showHelp, {
    shinyalert("Confused?", "
                            <div style='text-align: center;'>
                            Bootstrapping is a way to calculate statistics using <span style=\"font-weight:bold; \"> simulations </span> rather than <span style=\"font-weight:bold; \"> equations</span>.<br> <br>
               
                            <div style='text-align: center;'>
                            Follow the steps below to practice bootstrapping:<br> <br>
               
                            <div style='text-align: left; display: inline-block;'>
                            <span style=\"font-size: 14px; line-height: 1;\">
                            1. Refresh the page <br>
                            2. Click <span style=\"text-decoration: underline;\"> Generate Sample</span> to draw a random sample from the normal population <br>
                            3. Click <span style=\"text-decoration: underline;\"> Generate Bootstrap Sample</span> to draw a bootstrap sample from the random sample <br>
                            <span style=\"font-size: 12px;\"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *Note how bootstrapping uses sampling with replacement and allows for duplicate values </span> <br> 
                            <span style=\"font-size: 12px;\"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *The mean of the bootstrap sample is shown in red</span> <br> 
                            4. Click on <span style=\"text-decoration: underline;\"> Generate 100 Bootstrap Samples</span> to draw 100 bootstrap samples <br>
                            5. Check the <span style=\"text-decoration: underline;\"> Show Confidence Interval</span> box to show the bootstrap confidence interval <br>  <br>
                            </span> 
                            </div>
                            <div style='text-align: center;'>
                            Bootstrapping can be used to create confidence intervals for other population distributions and metrics.
                            "
               ,size = "m",html = T)})}

# Call Shiny App
shinyApp(ui, server)