# DATA423 - DATA SCIENCE IN INDUSTRY - ASSIGNMENT 2
# Student: Giang Bui
# ID: 37306207
# Date: 27/03/2023

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  pMiss <- function(x){ sum(is.na(x)) / length(x) * 100 }
  
  getData <- shiny::reactive({
    read.csv("Ass2Data.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "NA")
  })
  correctNA <- shiny::reactive({
    df <- getData()
    df[df == -99] <- NA
    df$POLITICS <- as.character(df$POLITICS) #convert away from factor
    df[df == "--"] <- NA
    df$POLITICS <- as.factor(df$POLITICS) # convert back to factor
  
    #Replace all occurrences of HEALTHCARE_COST NA values with  the value 0, indicating free healthcare.
    if (input$HealthcareCostImpute == TRUE){
      df$SHADOW_HEALTHCARE_COST <- as.numeric(is.na(df$HEALTHCARE_COST)) # create a shadow variable
      df$HEALTHCARE_COST[is.na(df$HEALTHCARE_COST)] <- 0 #Assign missing to zero
    }
    
    #Replace all occurances of POLITICS NA value with the value "None" indicate No Politics
    if (input$PoliticsImpute == TRUE){
      df$SHADOW_POLITICS <- as.numeric(is.na(df$POLITICS)) # create a shadow variable
      df$POLITICS <- as.character(df$POLITICS) #convert away from factor
      df$POLITICS[is.na(df$POLITICS)] <- "None"
      df$POLITICS <- as.factor(df$POLITICS) # convert back to factor
    }
    
    df
  })
  
  ################## PREPROCESSING ################## PREPROCESSING ################## PREPROCESSING ################## 
  
  
  getClean <- shiny::reactive({
    
    df <- correctNA()
    
    #Delete Column(s) with excessive missingness
    vRatio <- apply(df,2,pMiss)
    df <- df[, vRatio < input$Var_thres]
    
    #Filter out all observations that contain any NA from the dataset
    if (input$naomit == TRUE) {
      df <- na.omit(df)
    }
    
    
    #Delete row(s) with excessive missingness
    oRatio <- apply(df,1,pMiss) 
    df <- df[oRatio < input$Obs_thres, ]
    
    # Delete outliers from the first run
    if (input$OutlierDel == TRUE){
      # out_codes <- OutCodes()
      # out_codes <- c("State24", "State42", "State44", "State45", "State46", "State51", "State66", "State81", "State128", 
      #                "State135", "State154", "State173", "State205", "State255", "State257", "State274", "State294",
      #                "State295", "State316", "State330", "State349", "State351")
      out_codes <- c( "State1"," State2","State24","State37", "State45"," State46", "State51", "State66", "State81", "State111",  "State120",
                      "State128", "State154", "State173", "State239", "State247", "State255", "State257", "State272", "State274", "State294", 
                      "State293", "State295", "State316","State330", "State349", "State351", "State373","State396")
      df <- df[!df$CODE %in% out_codes, ]
      
      df
    }
    
    
    # 1.4. delete observations having missing values from important variables:
    if (input$imppop == TRUE){
      df <- df[complete.cases(df$POP_DENSITY), ]
    }
    if (input$impdoc == TRUE){
      df <- df[complete.cases(df$DOCS), ]
    }
    if (input$impgdp == TRUE){
      df <- df[complete.cases(df$GDP), ]
    }
    if (input$imp50 == TRUE){
      df <- df[complete.cases(df$AGE50_PROPTN), ]
    }
    if (input$impinf == TRUE){
      df <- df[complete.cases(df$INFANT_MORT), ]
    }
    if (input$impvax == TRUE){
      df <- df[complete.cases(df$VAX_RATE), ]
    }
    if (input$imp25 == TRUE){
      df <- df[complete.cases(df$AGE25_PROPTN), ]
    }

    df
  })
  #Display Variable Importance
  output$VariableImportance <- renderPrint({
    imp <- varImp(model())
    imp_vars <- rownames(imp$importance)
    imp_vars_df <-cbind(variable = imp_vars, imp$importance)
    imp_vars_df <- imp_vars_df[order(-imp_vars_df$Overall), ]
    imp_vars_ordered <- rownames(imp_vars_df)
    cat(paste(imp_vars_ordered, collapse = ","))
  })
  

    #Display removed variables by threshold after cleaning
    output$RemoveVar <- renderPrint({
      dat <- correctNA()
      vRatio <- apply(dat,2,pMiss)
      cat("Variables to remove:",paste(colnames(dat)[vRatio > input$Var_thres], collapse = ","))
    })
    
    #Display removed observations by threshold after cleaning
    output$RemoveObs <- renderPrint({
      dat <- correctNA()
      oRatio <- apply(dat,1,pMiss)
      cat("Observations:", paste(head(rownames(dat)[oRatio > input$Obs_thres], n = 50), collapse = ", "))
    })
    
    #Display Outliers after training model:
    output$Outliersnum <- renderPrint({
      out_codes <- OutCodes()
      cat("Observations:", paste(out_codes, collapse = ", "))
    })
    #Display data dimension after cleaning:
    output$dimdf <- renderPrint({
      df <- getClean()
      dim <- dim(df)
      cat(paste("data.frame: ",dim[[1]] ," obs. of  ", dim[[2]]," variables"))
    })
    
    output$Vismiss_cleanned <- renderPlot({
      visdat::vis_miss(getClean(), cluster = input$cluster_cleanned) 
    })
    
    getTrain <- shiny::reactive({
      data <- getClean()
      data[data$OBS_TYPE == "Train",]
    })
    
    getTest <- shiny::reactive({
      data <- getClean()
      data[data$OBS_TYPE == "Test",]
    })
    ################## GLMNET ################## GLMNET ################## GLMNET ################## GLMNET ##################
    
    observeEvent(input$Mean_mode, {
      if (input$Mean_mode) {updateCheckboxInput(session, "Knn_med", value = FALSE)
        updateCheckboxInput(session, "Bag_med", value = FALSE)}})
    observeEvent(input$Knn_med, {
      if (input$Knn_med) {updateCheckboxInput(session, "Mean_mode", value = FALSE)
        updateCheckboxInput(session, "Bag_med", value = FALSE) }})
    observeEvent(input$Bag_med, {
      if (input$Bag_med) {updateCheckboxInput(session, "Mean_mode", value = FALSE)
        updateCheckboxInput(session, "Knn_med", value = FALSE)}})
    observeEvent(input$dummy, {
      if (input$dummy) {updateCheckboxInput(session, "onehot", value = FALSE)}})
    observeEvent(input$onehot, {
      if (input$onehot) {updateCheckboxInput(session, "dummy", value = FALSE)}})
    
    
    getRecipe <- shiny::reactive({
      train <- getTrain()
      rec <- recipes::recipe(DEATH_RATE ~., data = train) %>% 
        update_role("CODE", new_role = "id") %>% 
        update_role("OBS_TYPE", new_role = "split") %>% 
        step_impute_mean(all_numeric_predictors(), skip = !input$Mean_mode) %>%
        step_impute_mode(all_nominal_predictors(), skip = !input$Mean_mode) %>%
        step_impute_knn(all_predictors(), neighbors = input$numNeighbors, skip = !input$Knn_med) %>%
        step_impute_bag(all_predictors(), skip = !input$Bag_med) %>% 
        step_center(all_numeric(), -has_role("outcome"), skip = !input$centering) %>%
        step_scale(all_numeric(), -has_role("outcome"), skip = !input$scaling) %>%  
        step_dummy(all_predictors(), -all_numeric(), one_hot = input$onehot)
      rec
    })
    
    model <- shiny::reactive({
      req(input$Go)
      isolate({
        train <- getTrain()
        rec <- getRecipe()
        glm_model <- caret::train(rec, data = train, method ="glmnet")
        return(glm_model)
      })
    })
    output$glmnetprocess <- renderPrint({
      model()
    })
    
    output$Importance <- renderPlot({
      imp <- varImp(model())
      plot(imp)
    })
    
    
    output$ActualvsPred_Train <- renderPlot({
      train <- getTrain()
      # Plot Actual and prediction points for train data:
      train$Prediction <- predict(model(), newdata = train)
      rang_train <- range(c(train$DEATH_RATE, train$Prediction))
      ggplot(data = train) +
        geom_point(mapping = aes(x = Prediction, y = DEATH_RATE)) +
        geom_abline(slope = 1, col = "blue") +
        labs(title = "Death rate predictions of COVID training data", y = "predicted", x = "actual") + coord_fixed(ratio = 1, xlim = rang_train, ylim = rang_train, expand = TRUE)
      
      })
    
    output$ActualvsPred_Test <- renderPlot({
      test <- getTest()
      # Plot Actual and prediction points for test data:
      test$Prediction <- predict(model(), newdata = test)
      rang_test <- range(c(test$DEATH_RATE, test$Prediction))
      ggplot(data = test) +
        geom_point(mapping = aes(x = Prediction, y = DEATH_RATE)) +
        geom_abline(slope = 1, col = "blue") +
        labs(title = "Death rate predictions of COVID test data", y = "predicted", x = "actual") + coord_fixed(ratio = 1, xlim = rang_test, ylim = rang_test, expand = TRUE)
       
    })
    
    output$ActualvsPred <- renderPlot({
      data <- getClean()
      # Plot Actual and prediction points for whole data:
      data$Prediction <- predict(model(), newdata = data)
      rang_all <- range(c(data$DEATH_RATE, data$Prediction))
      ggplot(data = data) +
        geom_point(mapping = aes(x = Prediction, y = DEATH_RATE)) +
        geom_abline(slope = 1, col = "blue") +
        labs(title = "Death rate predictions of COVID whole data", y = "predicted", x = "actual") + coord_fixed(ratio = 1, xlim = rang_all, ylim = rang_all, expand = TRUE)
      
    })
    
    output$RMSE <- renderPrint({
      # Train and test root meann square error
      data <- getClean()
      train <- getTrain()
      test <- getTest()
      train$Prediction <- predict(model(), newdata = train)
      test$Prediction <- predict(model(), newdata = test)
      data$Prediction <- predict(model(), newdata = data)
      rmse_train <- rmse(train$DEATH_RATE, train$Prediction)
      rmse_test <- rmse(test$DEATH_RATE, test$Prediction)
      rmse_all <- rmse(data$DEATH_RATE, data$Prediction)
      cat(paste("|| RMSE TRAIN: ",rmse_train, "||   ||RMSE TEST: ",rmse_test, "||   ||RMSE TOTAL:",rmse_all, "||"))
      
    })
    
    ResidualData <- shiny::reactive({
      req(input$Go)
      isolate({
        data <- getClean()
        train <- getTrain()
        test <- getTest()
        train$Prediction <- predict(model(), newdata = train)
        test$Prediction <- predict(model(), newdata = test)
        data$Prediction <- predict(model(), newdata = data)
        #Make a new columns residuals to detect outliers:
        train$Residual <- train$DEATH_RATE - train$Prediction
        test$Residual <- test$DEATH_RATE - test$Prediction
        data$Residual <- data$DEATH_RATE - data$Prediction
        #Create new data frame for residual with 2 colum: CODE and RESIDUAL
        all_residual <- data.frame(data[,c("CODE", "Residual")])
        train_residual  <- data.frame(train[,c("CODE", "Residual")])
        test_residual  <- data.frame(test[,c("CODE", "Residual")])
        
        #Merge all residual data
        data_residual1 <- full_join(all_residual,train_residual, by="CODE" )
        data_residual <- full_join(data_residual1,test_residual, by="CODE" )
        colnames(data_residual) <- c("CODE", "All_Residual","Train_Residual" , "Test_Residual")
        
        return(data_residual)
      })
    })
    
    output$Residatatable <- renderDataTable({
      data <- getClean()
      data_residual <- ResidualData()
      data_full <- full_join(data_residual, data, by="CODE" )
      data_full <- DT::datatable(data = as.data.frame(data_full))
    })
    
    output$ResiBox <- renderPlot({
      data_residual <-  ResidualData()
      data_box <- as.matrix(data_residual[,-c(1)])
      resi_boxplot <- car::Boxplot(y = data_box, ylab = "residuals", use.cols = TRUE, notch = FALSE, varwidth = FALSE,
                                   horizontal = FALSE, outline = TRUE,
                                   # col = brewer.pal(n = dim(data_box)[2], name = "RdBu"),
                                   col = brewer.pal(n = dim(data_box)[2], name = "Dark2"),
                                   range = input$resirange, main = "Boxplots of Residual",
                                   id = list(n=50, labels=data_residual$CODE, location="avoid"))
      
    })
    
    OutCodes <- shiny::reactive({
      if (!input$Go) {
        out_codes <- c()
      }
      else {
        req(input$Go)
        isolate({
          dat <- ResidualData()
          out <- boxplot.stats(dat$All_Residual)$out
          out_ind <- which(dat$All_Residual %in% c(out))
          out_codes <- dat$CODE[out_ind]
        })
      }
      return(out_codes)
    })
    
    
    output$Histogram <- renderPlot({
      data_residual <-  ResidualData()
      if(input$Resi_dat == "All Data"){
        data_residual %>% 
          ggplot(aes(x = All_Residual)) +
          geom_histogram(bins = input$bins, color = "black", fill = "deepskyblue3") +
          ggtitle("Histogram of Residuals from All Dataset") +
          ylab("Frequency") +
          theme(axis.text = element_text(size = 20), axis.title = element_text(size = 30),
                plot.title = element_text(size = 40, hjust = 0.5),legend.text = element_text(size = 16),
                legend.title = element_text(size = 20, face = "bold"))
      } else {
        if(input$Resi_dat == "Training Data"){
          data_residual %>% 
            ggplot(aes(x = Train_Residual)) +
            geom_histogram(bins = input$bins, color = "black", fill = "deepskyblue3") +
            ggtitle("Histogram of Residual from Training data") +
            ylab("Frequency") +
            theme(axis.text = element_text(size = 20), axis.title = element_text(size = 30),
                  plot.title = element_text(size = 40, hjust = 0.5),legend.text = element_text(size = 16),
                  legend.title = element_text(size = 20, face = "bold"))
        } else {
          data_residual %>% 
            ggplot(aes(x = Test_Residual)) +
            geom_histogram(bins = input$bins, color = "black", fill = "deepskyblue3") +
            ggtitle("Histogram of Residuals from Test Data") +
            ylab("Frequency") +
            theme(axis.text = element_text(size = 20), axis.title = element_text(size = 30),
                  plot.title = element_text(size = 40, hjust = 0.5),legend.text = element_text(size = 16),
                  legend.title = element_text(size = 20, face = "bold"))
        }
      }
      
      
    })
    
    ################## MISSINGNESS ################## MISSINGNESS ################## MISSINGNESS ################## 
    output$Vismiss <- renderPlot({
      visdat::vis_miss(correctNA(), cluster = input$cluster) +
        labs(title = "Missingness of Ass2.csv data")
    })
    
    output$Missupset <- renderPlot({
      naniar::gg_miss_upset(data = correctNA(), nsets = input$numVars)
    })
    
    output$Tree <- renderPlot({
      data <- correctNA()
      data$MISSINGNESS <- apply(X = is.na(data), MARGIN = 1, FUN = sum)
      tree <- caret::train(MISSINGNESS ~ .-CODE -OBS_TYPE, data = data, method = "rpart", na.action = na.rpart)
      rpart.plot(tree$finalModel, main = "Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)
    })
    
    output$CorrMiss <- renderPlot({
      m <- is.na(correctNA()) + 0 # this is a trick that transforms Logical to Binary
      cm <- colMeans(m)
      m <- m[, cm > 0 & cm < 1, drop = FALSE] #remove none-missing or all-missing variables
      data <- as.matrix(m)
      corrgram::corrgram(data, 
                         order = input$Group1, 
                         abs = input$abs1, 
                         cor.method = input$CorrMeth1, 
                         text.panel = panel.txt)
    })
    
      ################## EDA ################## EDA ################## EDA ################## EDA ##################
      
    output$Rawdata <- DT::renderDataTable({
      DT::datatable(data = as.data.frame(correctNA()))
    })
    
    output$Structure <- renderPrint({
      str(correctNA())
    })
    
    output$Dfsummary <- renderUI({
      Data <- correctNA()
      summary_df <- summarytools::dfSummary(Data, graph.magnif = 2)
      summarytools::view(summary_df, method = 'render', report.title = NA, headings = FALSE,
                         bootstrap.css = FALSE, footnote = NA, max.tbl.height = 600,
                         collapse = TRUE, silent = TRUE
                         )
    })
    
    output$Mosaic <- renderPlot({
      formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
      vcd::mosaic(formula, data = correctNA(),
                  main = "MosaicPlot of Categorical data", cex.main = 2,
                  legend = TRUE, shade = TRUE)
      
    }, height = 700, width = 700)
    
    output$Corrgram <- renderPlot({
      num_data <- correctNA() %>% 
        dplyr::select("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                      "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE",
                      "HEALTHCARE_COST", "DEATH_RATE")
      data <- as.matrix(num_data)
      corrgram(data, 
               order = input$Group, 
               abs = input$abs, 
               cor.method = input$CorrMeth,
               text.panel = panel.txt,
               main = "Correlation of Numeric data")
    })
    
    observeEvent(input$select_all_groupA, {
      if (input$select_all_groupA) {
        updateSelectInput(session, "VariablesG", selected = option2)
      }
    })
    
    output$MixedPairs <- renderPlot({
      req(input$ggpairgo)
      isolate({
        cat_cols <- c(input$VariablesE)
        colour_1 <- c(input$VariablesF)
        num_cols <- c(input$VariablesG)
        clean_df <- na.omit(correctNA())
        data_num <- data.frame(clean_df[,1:9], clean_df$HEALTHCARE_COST, clean_df$DEATH_RATE)
        colnames(data_num) <- c(option2)
        data_mixed <- data.frame(clean_df[,cat_cols], clean_df[,num_cols])
        GGally::ggpairs(data = data_mixed,  mapping = ggplot2::aes(colour = clean_df[,colour_1]), 
                        columnLabels = c(cat_cols, num_cols), 
                        title = "Pairs of Assignment 1 data")
      })
      
    })

}
