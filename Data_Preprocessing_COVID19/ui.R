# DATA423 - DATA SCIENCE IN INDUSTRY - ASSIGNMENT 2
# Student: Giang Bui
# ID: 37306207
# Date: 27/03/2023

library(shiny)

# Define UI for application that draws a histogram
UI <- navbarPage(
  "ASSIGNMENT 2 - Giang Bui",
  shiny::tabPanel("EDA",
                  fluidRow(
                    column(2,
                           h5(HTML("<b>Manual imputation for </b>")), 
                           h5(HTML("<b>'Not Applicable' values</b>")),
                    ),
                    column(3,
                           checkboxInput("HealthcareCostImpute", "Impute HEALTHCARE_COST missing value with 0 and create a shadow variable:"),
                    ),
                    column(7,
                           checkboxInput("PoliticsImpute", "Impute POLITICS missing value with 'None' and create a shadow variable:"),
                    ),
                    style = "background-color: #a8eddc;"
                  ),
                  tabsetPanel(
                    tabPanel("Raw Data", "Raw data:",
                             dataTableOutput(outputId = "Rawdata")),
                    tabPanel("Data Structure", "Data structure:",
                             verbatimTextOutput(outputId = "Structure")),
                    tabPanel("Dataframe Summary", "Dataframe",
                             htmlOutput(outputId = "Dfsummary")),
                    tabPanel("MosaicCharts",
                             selectizeInput(inputId = "VariablesA", 
                                            label = "Choose Categorical Variables to chart:", 
                                            choices = option1, multiple = TRUE, 
                                            selected = c("POLITICS","HEALTHCARE_BASIS")),
                             plotOutput(outputId = "Mosaic")),
                    tabPanel("CorrgramPlots",
                             checkboxInput(inputId = "abs", 
                                           label = "Apply absolute correlation", value = TRUE),
                             selectInput(inputId = "CorrMeth", label = "Correlation method", 
                                         choices = c("pearson","spearman","kendall"), 
                                         selected = "spearman"),
                             selectInput(inputId = "Group", label = "Grouping method", 
                                         choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), 
                                         selected = "OLO"),
                             plotOutput(outputId = "Corrgram")),
                    tabPanel("MixedPairs Numeric - Categorical",
                             checkboxGroupInput(inputId = "VariablesE", 
                                                label = "Choose Categorical Variables to chart:", 
                                                choices = option1, 
                                                inline = TRUE,
                                                selected = "HEALTHCARE_BASIS"),
                             selectInput(inputId = "VariablesG", 
                                         label = "Choose Numeric Variables to chart:", multiple = TRUE,
                                         choices = option2,
                                         selected = c("POP_DENSITY", "DOCS", "GDP", "AGE50_PROPTN", "INFANT_MORT", "VAX_RATE")
                             ),
                             checkboxInput("select_all_groupA", "Select All"),
                             radioButtons(inputId = "VariablesF", 
                                          label = "Choose Categorical Variable to colour:", 
                                          choices = option1, 
                                          inline = TRUE, 
                                          selected = "HEALTHCARE_BASIS"),
                             actionButton(inputId = "ggpairgo", label = "Plot Now!", icon = icon("play")),
                             p("Click on the 'play' button to show the plot", style = "color: red;"),
                             br(), 
                             plotOutput(outputId = "MixedPairs", height = "1150", width = "1150")
                             
                    ),
                    tabPanel("Missingness",
                             fluidPage(
                               h5(HTML("<b>The section on missingness utilizes raw data (after NA correction) to investigate patterns and correlations in missing data.</b>"), style = "color: #0f02a3;"),
                               tabsetPanel(
                                 tabPanel("visdat::vis_miss", br(),
                                          checkboxInput(inputId = "cluster", 
                                                        label = "Cluster missingness", value = FALSE),
                                          plotOutput(outputId = "Vismiss")
                                 ),
                                 tabPanel("gg_miss_upset chart", br(),
                                          sliderInput(inputId = "numVars", "Number of Variables:", min = 2, max = 14, value = 5),
                                          h3("A gg_miss_upset visualisation of single or grouped variables of missingess", align ="center"),
                                          plotOutput(outputId = "Missupset")
                                 ),
                                 tabPanel("Missing_Tree",
                                          plotOutput(outputId = "Tree")
                                 ),
                                 tabPanel("CorrgramMissing",
                                          fluidRow(
                                            column(3,br(),checkboxInput(inputId = "abs1", 
                                                                        label = "Apply absolute correlation", value = TRUE)),
                                            column(4,br(),selectInput(inputId = "CorrMeth1", label = "Correlation method", 
                                                                      choices = c("pearson","spearman","kendall"), 
                                                                      selected = "spearman",
                                                                      width = "200px")),
                                            column(5,br(),selectInput(inputId = "Group1", label = "Grouping method", 
                                                                      choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), 
                                                                      selected = "OLO",
                                                                      width = "200px"))
                                          ),
                                          fluidRow(br(),
                                                   h3("Variable missing value correlation", align ="center"),
                                                   plotOutput(outputId = "CorrMiss"))
                                 )   
                               )
                             )
                    )
                    
                    
                  )
                    
  ),
  
  ##### GLM ######
  shiny::tabPanel("GLM MODEL",
                  tabsetPanel(
                    tabPanel("Preprocessing",
                             fluidPage(
                               fluidRow(
                                 column(4,  style = "background-color: #a8eddc;",
                                        # br(),h5(HTML("<b>Variables sorted in descending order for prioritizing significance (after GLMNET).:</b>")),
                                        # verbatimTextOutput(outputId = "VariableImportance"),
                                        h5(HTML("<b>1. Variable Deleting:</b>"), style = "color: #0f02a3;"),
                                        sliderInput("Var_thres", "Variable missing threshold (%):",
                                                    min = 1, max = 100, value = 100, step = 1),
                                        # h4("Variable to remove (number of missing values > Threshold):"),
                                        verbatimTextOutput(outputId = "RemoveVar"),
                                        h5(HTML("<b>2. Observation Deleting:</b>"), style = "color: #0f02a3;"),
                                        checkboxInput("naomit", "Filter out all observations that contain any NA from the dataset", value = FALSE),
                                        sliderInput("Obs_thres", "Observation missing threshold (%):",
                                                    min = 1, max = 100, value = 100, step = 1),
                                        h5(HTML("<b>Observations to remove (number of missing values > Threshold):</b>")),
                                        verbatimTextOutput(outputId = "RemoveObs"),
                                        h5(HTML("<b>3. Outliers Deleting:</b>"),style = "color: #0f02a3;"),
                                        h5(HTML("<b>Residual Outlier observation CODE (appear here after performming GLMNET):</b>")),
                                        verbatimTextOutput(outputId = "Outliersnum"),
                                        checkboxInput("OutlierDel", "Delete Outliers after GLMNET performance", value = FALSE),
                    
                                 ),
                    
                                 column(8,
                                        h5(HTML("<b>Missingness Visualization after Cleaning</b>")),
                                        verbatimTextOutput(outputId = "dimdf"),
                                        checkboxInput(inputId = "cluster_cleanned",
                                                      label = "Cluster missingness", value = FALSE),
                                        plotOutput(outputId = "Vismiss_cleanned"),
                    
                                 )
                               ),
                    
                               fluidRow(style = "background-color: #a8eddc;",
                                        h5(HTML("<b>4. delete observations having missing values from important variables:</b>"), style = "color: #0f02a3;"),
                                        column(2,
                                               checkboxInput("imppop", "POP_DENSITY"),
                                               checkboxInput("imp25", "AGE25_PROPTN"),),
                                        column(2, checkboxInput("impdoc", "DOCS"),),
                                        column(2, checkboxInput("impgdp", "GDP"),),
                                        column(2, checkboxInput("imp50", "AGE50_PROPTN"),),
                                        column(2, checkboxInput("impinf", "INFANT_MORT"),),
                                        column(2, checkboxInput("impvax", "VAX_RATE"),),
                               )
                             )
                             ),
                    tabPanel("Model Training",
                             fluidRow(
                               column(3, 
                                      style = "background-color: #a8eddc;",
                                      h3(HTML("<b>Recipe Preparation:</b>")), br(),
                                      h4("> Choose method of imputation:"),
                                      checkboxInput("Mean_mode", "Impute all numeric with mean method & nomial with mode method"),
                                      checkboxInput("Knn_med", "Impute with KNN"),
                                      conditionalPanel(
                                        condition = "input.Knn_med == true",
                                        div(
                                          id = "Knn", p("Adjust the number of Neighbors:", style = "color: blue;"),
                                          sliderInput(inputId = "numNeighbors", "Number of neighbors:", min = 1, max = 100, value = 6)
                                        )
                                      ),
                                      checkboxInput("Bag_med", "Impute with bag method", value = TRUE),
                                      h4("> Standardization and Normalization:"),
                                      checkboxInput(inputId = "centering",
                                                    label = "> Centering numeric values", value = TRUE),
                                      checkboxInput(inputId = "scaling",
                                                    label = "> Scaling numeric values", value = TRUE),
                                      h4("> One-Hot or Dummy Encoding:"),
                                      checkboxInput(inputId = "dummy",
                                                    label = "> Dummy encoding", value = TRUE),
                                      checkboxInput(inputId = "onehot",
                                                    label = "> One-hot encoding", value = FALSE),
                                      br(), actionButton(inputId = "Go", label = "Train Model Now", icon = icon("play")),
                                      p("Click on the 'play' button to train GLMNET model", style = "color: red;"),
                               ),
                               column(9,
                                      h3(HTML("<b>MODEL SUMMARY</b>")), br(),
                                      verbatimTextOutput(outputId = "glmnetprocess"),br(),
                                      br(),
                               )
                             ),
                             fluidRow(
                               h3(HTML("<b>VARIABLE IMPORTANCE</b>")),
                               plotOutput(outputId = "Importance"),
                               br(), h3(HTML("<b>ACTUAL VS PREDICTION SCATTER PLOT</b>"))

                             ),
                             fluidRow(
                               column(4, plotOutput(outputId = "ActualvsPred_Train"), br(),),
                               column(4, plotOutput(outputId = "ActualvsPred_Test"), br(), ),
                               column(4, plotOutput(outputId = "ActualvsPred"), br(),)
                             ),
                             fluidRow(
                               br(), h3(HTML("<b>ROOT MEAN SQUARED ERROR</b>")),
                               verbatimTextOutput(outputId = "RMSE"), br(), br(), br(),
                             )
                    
                    ),

                    tabPanel("Residual Data table",
                             h4("The table is shown after performming GLMNET"),
                             h3(HTML("<b>RESIDUAL DATA TABLE RESULT</b>")),
                             DT::dataTableOutput(outputId = "Residatatable")
                    ),
                    tabPanel("Residual Boxplot Plotting",
                             h4("The Residual Boxplot is shown after performming GLMNET"),
                             h3(HTML("<b>RESIDUAL BOXPLOT PLOTING</b>")),
                             sliderInput(inputId = "resirange", "IQR Multiplier:",
                                         min = 1, max = 5, step = 0.1, value = 1.5),
                             plotOutput(outputId = "ResiBox"), br()
                    ),
                    tabPanel("Residual Histogram",
                             h4("The residual histogram is shown after performming GLMNET"),
                             h3(HTML("<b>RESIDUAL HISTOGRAM PLOTTING</b>")),
                             radioButtons(inputId = "Resi_dat", 
                                          label = "Choose ALL/TRAIN/TEST DATA to plot:", 
                                          choices = c("All Data", "Training Data", "Test Data"),
                                          inline = TRUE, 
                                          selected = "All Data"),
                             sliderInput(inputId = "bins",
                                         "Number of bins:",
                                         min = 1,
                                         max = 50,
                                         value = 30),
                             plotOutput(outputId = "Histogram")
                    )
                    )
)
)
  

