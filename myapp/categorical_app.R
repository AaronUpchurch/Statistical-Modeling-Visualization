library(shiny)
library(DT)
library(jsonlite)
library(ggplot2)
library(officer)
library(latex2exp)

#----------------------------------------------------
# TODO - CODE BELOW FOR CATEGORICAL APPLET:
#----------------------------------------------------


ui <- fluidPage(
  titlePanel("2x2 Categorical Applet"),
  fluidRow(
    column(12, align = "center",
           div(style = "width: 500px;",
    htmlOutput("info")
           ))
  ),
  fluidRow(
    column(12, align = "center",
           div(style = "width: 500px;", # Decreased width
               DTOutput("table"),               
               br(),
               fluidRow(
                 actionButton("calculate", "Calculate Values"),
                 actionButton("generatePlot", "Generate Mosaic Plot")
               ),
               p(
                 fluidRow(
                   column(3, uiOutput("odds_ratio_text")), # Adjusted margin-top
                   column(1, div(style = "margin-top: 10px;", uiOutput("odds_ui")))
                 )
               ),
               p(
                 fluidRow(
                   column(6, uiOutput("odds_ci_text")), # Adjusted margin-top
                   column(2, div(style = "margin-top: 10px;", uiOutput("odds_ci_ui")))
                 )
               ),
               p(
                 fluidRow(
                   column(3, uiOutput("chi_stat_text")), # Adjusted margin-top
                   column(1, div(style = "margin-top: 10px;", uiOutput("chi_stat_ui")))
                 )
               ),
               p(
                 fluidRow(
                   column(3, uiOutput("chi_p_text")), # Adjusted margin-top
                   column(1, div(style = "margin-top: 10px;", uiOutput("chi_p_ui")))
                 )
               ),
               p(
                 fluidRow(
                   column(5, uiOutput("fisher_text")), # Adjusted margin-top
                   column(1, div(style = "margin-top: 10px;", uiOutput("fisher_ui")))
                 )
               ),
           )
    )
  )
)


server <- function(input, output, session) {
  
  output$info <- renderText({"Feel free to double click and change the numbers within the table, along with the column and row headings!"})
  
  values2 <- reactiveValues(data = data.frame(A = c(279, 165), B = c(225, 191)))
  mycols <- reactiveValues(data = c('Democrat', 'Republican'))
  myrows <- reactiveValues(data = c('Male', 'Female'))
  values <- reactiveValues(data = NULL)
  valid <- reactiveValues(value = FALSE)
  

  observeEvent(input$col_changed, {
    colIndex <- input$col_changed$colIndex
    newColName <- input$col_changed$colName
    mycols$data[colIndex] <- newColName
  })
  
  observeEvent(input$calculate, {
    tryCatch({if (values2$data$A[1] > 0 & values2$data$A[2]> 0 & values2$data$B[1]> 0 & values2$data$B[2]> 0 & 
                  (typeof(values2$data$A[1]) == "integer" | typeof(values2$data$A[1]) == "double") &
                  (typeof(values2$data$A[2]) == "integer" | typeof(values2$data$A[2]) == "double") &
                  (typeof(values2$data$B[1]) == "integer" | typeof(values2$data$B[1]) == "double") &
                  (typeof(values2$data$B[2]) == "integer" | typeof(values2$data$B[2]) == "double"))
    {
      valid$value <- TRUE
      values$data <- values2$data
    }
      else
      {
        valid$value <- FALSE
        showModal(modalDialog(
          title = "Error",
          "Please enter positive numbers in all cells.",
          easyClose = TRUE
        ))
      }}, error = function(e) {
        valid$value <- FALSE
        showModal(modalDialog(
          title = "Error",
          "Please enter positive numbers in all cells.",
          easyClose = TRUE
        ))
        })
    
  })
  
  output$odds_ui <- renderUI({
    if (input$calculate > 0 & valid$value) {
      actionButton("info_odds", icon("info"))
    }
  })
  output$odds_ci_ui <- renderUI({
    if (input$calculate > 0 & valid$value) {
      actionButton("info_odds_ci", icon("info"))
    }
  })
  output$chi_stat_ui<- renderUI({
    if (input$calculate > 0 & valid$value) {
      actionButton("info_chi_stat", icon("info"))
    }
  })
  output$chi_p_ui <- renderUI({
    if (input$calculate > 0 & valid$value) {
      actionButton("info_chi_p", icon("info"))
    }
  })
  output$fisher_ui <- renderUI({
    if (input$calculate > 0 & valid$value) {
      actionButton("info_fisher", icon("info"))
    }
  })
  
  output$table <- renderDT({
    n <- 2
    columns <- mycols$data
    #data <- matrix(1:(2*2), nrow = 2)
    #rownames(data) <- LETTERS[1:n]
    datatable(
      values2$data,
      editable = TRUE,
      selection = 'none',
      options = list(
        dom = 't',
        ordering = FALSE,
        initComplete = JS(
          sprintf("
          function(settings, json) {
            var table = settings.oInstance.api();
            for (var i = 1; i < %d+1; i++) {
              var letter = String.fromCharCode(65 + i);
              var header = '<input type=\"text\" id=\"col' + letter + '\" value=\"' + %s[i] + '\" style=\"width: 100px;\">';
              $(table.column(i).header()).html(header);

              $('#col' + letter).on('blur', function() {
                var text = $(this).val();
                var colIndex = $(this).closest('th').index();
                $(table.column(colIndex).header()).html('<input type=\"text\" id=\"col' + text + '\" value=\"' + text + '\" style=\"width: 100px;\">');
                Shiny.setInputValue('col_changed', {colIndex: colIndex, colName: text});
              });
            }
          }
          ", n, toJSON(c(0, mycols$data)))
        ),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      ),
      colnames = mycols$data,
      rownames = myrows$data,
    )
  }, server = FALSE)
  ## TODO: IF BIG ISSUES, Change server to TRUE - BIG NOTE.
  
    observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    # print(info)
    # print(typeof(info))

    if (info$col > 0 & info$row > 0)
    {
      values2$data[info$row, info$col] <- as.numeric(info$value)
    }
    else if (info$col == 0)
    {
      # print("HERE1")
      myrows$data[as.numeric(info$row)] <- as.character(info$value)
    }
    else if (info$row == 0)
    {
      # print("HERE2")
      mycols$data[as.numeric(info$col)] <- as.character(info$value)
    }
    
    # print(myrows$data)
    # print(mycols$data)
    
  })
  
    output$odds_ratio_text <- renderUI({
      if (input$calculate > 0 & valid$value) {
        odds_ratio <- values$data$A[1] * values$data$B[2] / (values$data$A[2] * values$data$B[1])
        
        withMathJax(paste("$$ \\hat{\\theta}: ", round(odds_ratio, 2), "$$"))
      }
    })
    
    
    output$chi_stat_text <- renderUI({
      if (input$calculate > 0 & valid$value) {
        x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
        y <- chisq.test(x, correct = FALSE)
        
        withMathJax(paste("$$ X^2 \\text{ T.S.: } ", round(y$statistic, 2), "$$"))
      }
    })
    
    output$chi_p_text <- renderUI({
      if (input$calculate > 0 & valid$value) {
        x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
        y <- chisq.test(x, correct = FALSE)
        
        withMathJax(paste("$$ X^2 \\text{ p-value: } ", round(y$p.value, 2), "$$"))
      }
    })
    
    output$fisher_text <- renderUI({
      if (input$calculate > 0 & valid$value) {
        x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
        y <- fisher.test(x)
        
        withMathJax(paste("$$ \\text{Fisher's Exact p-value: } ", round(y$p.value, 2), "$$"))
      }
    })
    
    output$odds_ci_text <- renderUI({
      if (input$calculate > 0 & valid$value) {
        x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
        odds_ratio <- values$data$A[1] * values$data$B[2] / (values$data$A[2] * values$data$B[1])
        y <- exp(log(odds_ratio) + 1.96*(sqrt((1/x[1,1]) + (1/x[1,2]) + (1/x[2,1]) + (1/x[2,2]))))
        z <- exp(log(odds_ratio) - 1.96*(sqrt((1/x[1,1]) + (1/x[1,2]) + (1/x[2,1]) + (1/x[2,2]))))
        
        withMathJax(paste("$$ \\text{95% Wald CI for } \\theta: ( ", round(z, 4), ",", round(y, 4), ")$$"))
      }
    })
    observeEvent(input$info_odds_ci, {
      odds_ratio <- values$data$A[1] * values$data$B[2] / (values$data$A[2] * values$data$B[1])
      x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
      y <- exp(log(odds_ratio) + 1.96*(sqrt((1/x[1,1]) + (1/x[1,2]) + (1/x[2,1]) + (1/x[2,2]))))
      z <- exp(log(odds_ratio) - 1.96*(sqrt((1/x[1,1]) + (1/x[1,2]) + (1/x[2,1]) + (1/x[2,2]))))
      if (y > 1 & z < 1)
      {
        input_text <- paste(round(1-z, 4)*100, "% lower to", round(y - 1, 4)*100, "% higher in the category")
      }
      else if (y <1 & z < 1)
      {
        input_text <- paste(round(1-y, 4)*100, "% to", round(1-z, 4)*100, "% lower in the category")
      }
      else if (y > 1 & z > 1)
      {
        input_text <- paste(round(z-1, 4)*100, "% to", round(y-1, 4)*100, "% higher in the category")
      }
      else
      {
        input_text <- paste("This broke - send a message to maxlogan926@gmail.com")
      }
      showModal(modalDialog(
        title = "Odds Ratio CI",
        easyClose = TRUE,
        HTML(paste("The 95% Wald CI was calculated using the formula:")),
        withMathJax(paste("$$ e^{\\text{ln}(\\hat\\theta) \\pm 1.96 \\sqrt{\\frac{1}{a}*\\frac{1}{b}*\\frac{1}{c}*\\frac{1}{d}}}$$")),
        HTML(paste("Plugging in values, we can get: ")),
        withMathJax(paste("$$ e^{\\text{ln}(", round(odds_ratio, 3), ") \\pm 1.96 \\sqrt{\\frac{1}{", values$data$A[1], "}*\\frac{1}{", values$data$B[1], "}*\\frac{1}{", values$data$A[2], "}*\\frac{1}{", values$data$B[2], "}}} = (", round(z, 4), ",", round(y, 4), ")$$")),
        HTML(paste("Interpretation: We are 95% confident that the odds that an individual is in the category", mycols$data[1], "rather than the category", mycols$data[2], "is between ", input_text, myrows$data[1], "than in the category", myrows$data[2], "."))
      ))

    })
    
    observeEvent(input$info_odds, {
      a <- values$data$A[1]
      b <- values$data$B[1]
      c <- values$data$A[2]
      d <- values$data$B[2]
      odds_ratio <- values$data$A[1] * values$data$B[2] / (values$data$A[2] * values$data$B[1])
      if (odds_ratio > 1)
      {
        in_text <- paste("~", round((odds_ratio - 1)*100),
                         "% higher for individuals in the category")
      }
      else if (odds_ratio < 1)
      {
        in_text <- paste("~", round((1-odds_ratio)*100), "% lower for individuals in the category")
      }
      else
      {
        in_text <- paste("the same for individuals in the category")
      }
    showModal(modalDialog(
      title = "Odds Ratio Calculation",
      HTML(paste("The given values are:<br>")),
      fluidRow(
        column(12,
               tableOutput("chi_squared_table")  # output the table
        )
      ),
      HTML(paste("With each box labelled:<br>")),
      fluidRow(
        column(12,
               tableOutput("chi_squared_table_3")  # output the table
        )
      ),
      HTML(paste("Using these values, the Odds Ratio (OR) is calculated as follows:<br><br>")),
      withMathJax("$$ OR = \\frac{ad}{bc} = \\frac{", a, "\\times", d, "}{", b, "\\times", c, "} = ", round(odds_ratio, 4), " $$"),
      HTML(paste("Interpretation: The odds of an individual being within the category", 
                 mycols$data[1], 
                 "rather than the category", 
                 mycols$data[2], 
                 "is", 
                 in_text,
                 myrows$data[1], "than in the category", myrows$data[2], ".")),
      easyClose = TRUE
    ))
})
    observeEvent(input$info_chi_p, {
      x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
      y <- chisq.test(x, correct = FALSE)
      if (y$p.value < 0.05)
      {
        returntext <- paste("At the 95% confidence level, since our p-value,", signif(y$p.value, 2),", is smaller than 0.05, we have significance evidence to reject H<sub>0</sub>, providing significant evidence of an association
                            between the column categories and the row categories.")
      }
      else
      {
        returntext <- paste("At the 95% confidence level, since our p-value,", signif(y$p.value, 2),", is larger than 0.05, we have no significance evidence to reject H<sub>0</sub>, providing no evidence of an association
                            between the column categories and the row categories.")
      }
      
      showModal(modalDialog(
        title = "Chi-Squared Test P-Value",
        easyClose = TRUE,
        HTML(paste("Chi-squared tests examine the null hypothesis that there is no association between the column categories and the row categories. 
        The alternative hypothesis examines whether there is an association between these two sets of categories. <br><br> If the null hypothesis, H<sub>0</sub>,  is true, one would
expect the observed frequency in each cell to be close to the (estimated) expected frequency in that cell under
the null hypothesis (as can be shown in the test-statistic calculation), leading to a relatively small value for the test statistic. If H<sub>0</sub> is false, at least some
observed frequencies and (estimated) expected frequencies are far apart leading to a large value for the test
statistic. And the larger the difference between these two sets of frequencies (i.e., the larger the test statistic),
the stronger the evidence against the null hypothesis H<sub>0</sub>. <br><br> Under the null, we found a test statistic of ", round(y$statistic, 2), ". To find our degrees of freedom for our chi-squared 
                   distribution, we can calculate this by finding (rows - 1)*(columns-1) = (2-1)*(2-1) = 1. The probability that we would get a test statistic this large or greater in our chi-squared distribution: ")),
        withMathJax(paste("$$P(X^2_1 >", round(y$statistic, 2), ") = ", signif(y$p.value, 2), "$$")),
        HTML(returntext),
        plotOutput("modal_plot")
                    
      ))
      
    })
    output$modal_plot <- renderPlot({
      a <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
      b <- chisq.test(a, correct = FALSE)
      #print(b$statistic)
      x <- seq(.5*b$statistic, 3*b$statistic, length.out = 1000)
      y <- dchisq(x, df = 1)
      plot(x, y, type = "l", main = "Chi-Squared Distribution w/ Degrees of Freedom = 1", xlab = "Chi-Squared Value", ylab = "Density")
      abline(v = b$statistic, col = "red")
      polygon(c(b$statistic, x[x > b$statistic]), c(0, y[x > b$statistic]), col = "lightblue")
      text(b$statistic, .025, paste("Test Statistic:", signif(b$statistic, 3)), pos = 4, col="darkred")
      text(b$statistic*1.5, .003, paste("P-Value: ", signif(b$p.value, 3)), pos = 3, col="lightskyblue4")
    })
    
    observeEvent(input$info_fisher, {
      showModal(modalDialog(
        title = "Fisher's Exact P-Value",
        easyClose = TRUE,
        HTML("This is particularly useful when the sample size is limited, violating the assumptions of the chi-square test. The p-value 
        generated by Fisher's Exact Test indicates the probability of observing the data or more extreme results under the assumption that the 
        null hypothesis stating no association between the row and column variables is true. A low p-value suggests that the observed association between 
             the variables is unlikely to have occurred by chance alone, thus supporting the alternative hypothesis of an association.")
      ))
      
    })
    
    
    
    observeEvent(input$info_chi_stat, {
      x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
      y <- chisq.test(x, correct = FALSE)
      x <- addmargins(as.matrix(x))
      
      z <- data.frame(A = c(x[1,3]*x[3,1]/x[3,3], x[2,3]*x[3,1]/x[3,3]), B = c(x[1,3]*x[3,2]/x[3,3], x[2,3]*x[3,2]/x[3,3]))
      # mycols <- colnames(x)  # store column names in an array
      # myrows <- rownames(x)  # store row names in an array
      showModal(modalDialog(
        title = "Chi-Squared Test Statistic Calculation",
        easyClose = TRUE,
        HTML(paste("Given the below table, ")),
        fluidRow(
          column(12,
                 tableOutput("chi_squared_table")  # output the table
          )
        ),
        HTML(paste("We need to provide the Expected values for each cell, following the formula:")),
        withMathJax("$$\\text{Expected} = \\frac{\\text{Row Total} \\times \\text{Column Total}}{\\text{Total in Table}}$$"),
        HTML(paste("With this, we can provide the expected values in each cell: ")),
        fluidRow(
            column(12,
                   tableOutput("chi_squared_table_4")  # output the table
            )
          ),
        HTML(paste("And we now can calculate the test-statistic based on the formula: ")),
        withMathJax("$$X^2 = \\sum\\frac{(\\text{O} - \\text{E})^2}{\\text{E}}$$"), 
        withMathJax("$$ = 
                    \\frac{(", x[1,1], "-", signif(z[1,1], 3), ")^2}{", signif(z[1,1], 3), "} + 
                    \\frac{(", x[1,2], "-", signif(z[1,2], 3), ")^2}{", signif(z[1,2], 3), "} +
                    \\frac{(", x[2,1], "-", signif(z[2,1], 3), ")^2}{", signif(z[2,1], 3), "} +
                    \\frac{(", x[2,2], "-", signif(z[2,2], 3), ")^2}{", signif(z[2,2], 3), "} = ", round(y$statistic, 2), "$$"),
        HTML(paste("Alternatively, in the special 2x2 case, We can calculate the Chi-Squared test-statistic using the following formula: ")),
        withMathJax("$$X^2 = \\frac{n(n_{11}n_{22} - n_{12}n_{21})^2}{n_{1+}n_{2+}n_{+1}n_{+2}} \\text{where} $$"),
        fluidRow(
          column(12,
                 tableOutput("chi_squared_table_2")  # output the table
          )
        ),
        HTML(paste("Plugging in values, we get: ")),
        withMathJax("$$X^2 = \\frac{", x[3,3], "(", x[1, 1], "\\times", x[2,2], "-", x[1,2], "\\times", x[2,1], ")^2}{", x[3, 1], "\\times", x[3,2], "\\times", x[1,3], "\\times", x[2,3], "} = ", round(y$statistic, 2), "$$"),
        HTML(paste("Note that if the values in the table are considered \"small\", it is recommended to use a Fisher's Exact test."))
      ))
    })
    
    output$chi_squared_table <- renderTable({
      x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
      x <- addmargins(as.matrix(x))  # Convert to matrix and add row and column totals
      rownames(x) <- c(myrows$data, "Total")  # Set appropriate row names
      colnames(x) <- c(mycols$data, "Total")  # Set appropriate column names
      x  # Return the modified table
    }, rownames = TRUE, colnames = TRUE)
    output$chi_squared_table_2 <- renderTable({
      x <- data.frame(A = c(HTML(paste("n<sub>11</sub>")), HTML(paste("n<sub>12</sub>")), HTML(paste("n<sub>1+</sub>"))), B = c(HTML(paste("n<sub>21</sub>")), HTML(paste("n<sub>22</sub>")), HTML(paste("n<sub>2+</sub>"))), C = c(HTML(paste("n<sub>+1</sub>")), HTML(paste("n<sub>+2</sub>")), "n")) # Convert to matrix and add row and column totals
      rownames(x) <- c(myrows$data, "Total")  # Set appropriate row names
      colnames(x) <- c(mycols$data, "Total")  # Set appropriate column names
      x  # Return the modified table
    },sanitize.text.function = function(x) x,  rownames = TRUE, colnames = TRUE)
    output$chi_squared_table_3 <- renderTable({
      x <- data.frame(A = c("a", "c"), B = c("b", "d"))
      rownames(x) <- c(myrows$data)  # Set appropriate row names
      colnames(x) <- c(mycols$data)  # Set appropriate column names
      x  # Return the modified table
    }, rownames = TRUE, colnames = TRUE)
    output$chi_squared_table_4 <- renderTable({
      x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
      x <- addmargins(as.matrix(x))
      y <- data.frame(A = c(x[1,3]*x[3,1]/x[3,3], x[2,3]*x[3,1]/x[3,3]), B = c(x[1,3]*x[3,2]/x[3,3], x[2,3]*x[3,2]/x[3,3]))
      rownames(y) <- c(myrows$data)  # Set appropriate row names
      colnames(y) <- c(mycols$data)  # Set appropriate column names
      y  # Return the modified table
    }, rownames = TRUE, colnames = TRUE)
    
    
    generateMosaicPlot <- function() {
      x <- data.frame(A = c(values$data$A[1], values$data$A[2]), B = c(values$data$B[1], values$data$B[2]))
      rownames(x) <- myrows$data
      colnames(x) <- mycols$data
      mosaicplot(x, main = "Mosaic Plot", color = TRUE)
    }
    
    observeEvent(input$generatePlot, {
      
      tryCatch({if (values2$data$A[1] > 0 & values2$data$A[2]> 0 & values2$data$B[1]> 0 & values2$data$B[2]> 0 & 
                    (typeof(values2$data$A[1]) == "integer" | typeof(values2$data$A[1]) == "double") &
                    (typeof(values2$data$A[2]) == "integer" | typeof(values2$data$A[2]) == "double") &
                    (typeof(values2$data$B[1]) == "integer" | typeof(values2$data$B[1]) == "double") &
                    (typeof(values2$data$B[2]) == "integer" | typeof(values2$data$B[2]) == "double"))
      {
        valid$value <- TRUE
        values$data <- values2$data
        output$mosaicPlot <- renderPlot({
          generateMosaicPlot()
        })
        showModal(modalDialog(
          title = "Mosaic Plot",
          plotOutput("mosaicPlot")
        ))
        
      }
        else
        {
          valid$value <- FALSE
          showModal(modalDialog(
            title = "Error",
            "Please enter positive numbers in all cells.",
            easyClose = TRUE
          ))
        }}, error = function(e) {
          valid$value <- FALSE
          showModal(modalDialog(
            title = "Error",
            "Please enter positive numbers in all cells.",
            easyClose = TRUE
          ))
        })

    })
    
    
    
}

shinyApp(ui, server)
