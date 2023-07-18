# DATA423 - DATA SCIENCE IN INDUSTRY - ASSIGNMENT 1
# Student: Giang Bui
# ID: 37306207
# Date: 06/03/2023

# Define server for application that draws a histogram

# Define server logic ----
server <- function(input, output, session) {
  #thematic::thematic_shiny()
  
  output$Structure <- renderPrint({
    str(df)
  })
  output$Rawdata <- renderDataTable({
    datatable(data = as.data.frame(df))
  })
  output$Dfsummary <- renderUI({
    Data <- df
    summary_df <- summarytools::dfSummary(Data, graph.magnif = 2)
    summarytools::view(summary_df, 
         method = 'render',
         report.title = NA,
         headings = FALSE,
         bootstrap.css = FALSE,
         footnote = NA,
         max.tbl.height = 600,
         collapse = TRUE,
         silent = TRUE
    )
  })
  output$barplot <- renderPlot({
    cat_data %>% 
    ggplot(aes(x = !!input$categorical_var1, fill = !!input$categorical_var2)) +
      geom_bar(stat = "Count", color = "black") +
      ylab("Count") +
      ggtitle("Bar Chart Of A Categorical variable") +
      scale_y_continuous(breaks = seq(0,200,50)) +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 30),
            plot.title = element_text(size = 40, hjust = 0.5),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 20, face = "bold"))
      
  })
  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
    vcd::mosaic(formula, data = df,
                main = "MosaicPlot of Categorical data", cex.main = 2,
                legend = TRUE, shade = TRUE)

  })
  
  output$Histogram <- renderPlot({
    num_data %>% 
      ggplot(aes(x = !!input$VariableB)) +
      geom_histogram(bins = input$bins, color = "black", fill = "deepskyblue3") +
      ggtitle("Histogram of a numeric variable") +
      ylab("Frequency") +
      ggtitle("Histogram Of A Numeric Variable") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 30),
            plot.title = element_text(size = 40, hjust = 0.5),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 20, face = "bold"))
    
  })
  
  output$Boxplot <- renderPlot({
    data <- num_data
    data <- scale(data, center = input$standardise, scale = input$standardise)
    car::Boxplot(y = data, ylab = NA, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
                 horizontal = FALSE, outline = input$outliers,
                 col = brewer.pal(n = ncol(data), name = "Set3"),
                 outcol = "darkred", outpch = 20 , cex = 1, 
                 range = input$IQR, main = "Boxplots of Numeric data", 
                 id = ifelse(input$outliers, list(n = Inf, location = "avoid"), FALSE))
  })
  
  output$Corrgram <- renderPlot({
    data <- as.matrix(num_data)
    corrgram(data, 
             order = input$Group, 
             abs = input$abs, 
             cor.method = input$CorrMeth,
             text.panel = panel.txt,
             main = "Correlation of Numeric data")
  })
  
  output$MatPlot <- renderPlot({
    cols <- c(input$VariablesC) # choose the numeric columns
    numData <- scale(df[,cols], center = input$Center, scale = input$Scale) 
    matplot(numData, 
            type = "l", 
            col = rainbow(ncol(numData)), 
            xlab = "Observations in sequence", 
            ylab = "Value",
            main = "Mathplots of Numeric data")
  })
  
  output$Risingchart <- renderPlot({
    cols <- c(input$VariablesD)
    data <- as.matrix(df[,cols])
    for (col in 1:ncol(data)) {
      data[,col] <- data[order(data[,col]),col] #sort each column in ascending order
    }
    # scale so they can be graphed with a shared Y axis
    data <- scale(x = data, center = input$standardise_ris, scale = input$standardise_ris)  
    
    mypalette <- rainbow(ncol(data))
    matplot(y = data, type = "l", xlab = "Observations", ylab = "Values", 
            lty = 1, lwd = 1, col = mypalette, 
            main = "Rising Order charts of Numeric data")
    legend(legend = colnames(data), x = "topleft", y = "top", lty = 1, lwd = 1, 
           col = mypalette, ncol = round(ncol(data)^0.3))
    
  })
  
  observeEvent(input$select_all_groupA, {
    if (input$select_all_groupA) {
      updateCheckboxInput(session, "select_all_groupB", value = FALSE)
      updateCheckboxInput(session, "select_all_groupC", value = FALSE)
      updateSelectInput(session, "VariablesG", selected = c(option2_1, "Y"))
    }
  })
  observeEvent(input$select_all_groupB, {
    if (input$select_all_groupB) {
      updateCheckboxInput(session, "select_all_groupA", value = FALSE)
      updateCheckboxInput(session, "select_all_groupC", value = FALSE)
      updateSelectInput(session, "VariablesG", selected = c(option2_2,"Y"))
    }
  })
  observeEvent(input$select_all_groupC, {
    if (input$select_all_groupC) {
      updateCheckboxInput(session, "select_all_groupB", value = FALSE)
      updateCheckboxInput(session, "select_all_groupA", value = FALSE)
      updateSelectInput(session, "VariablesG", selected = c(option2_3,"Y"))
    }
  })

  output$MixedPairs <- renderPlot({
    cat_cols <- c(input$VariablesE)
    colour_1 <- c(input$VariablesF)
    num_cols <- c(input$VariablesG)
    clean_df <- na.omit(df)
    data_num <- data.frame(clean_df[,15:44], clean_df$Y)
    colnames(data_num) <- c(option2)
    data_mixed <- data.frame(clean_df[,cat_cols], clean_df[,num_cols])
    GGally::ggpairs(data = data_mixed,  mapping = ggplot2::aes(colour = clean_df[,colour_1]), 
                    columnLabels = c(cat_cols, num_cols), 
                    title = "Pairs of Assignment 1 data")
  })
    
  output$Missing_1 <- renderPlot({
    visdat::vis_miss(df, cluster = input$cluster) +
      labs(title = "Missingness of Ass1.csv data")
  })
  
  
}


