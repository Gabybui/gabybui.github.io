# DATA423 - DATA SCIENCE IN INDUSTRY - ASSIGNMENT 1
# Student: Giang Bui
# ID: 37306207
# Date: 06/03/2023

# Define UI for application that draws a histogram

ui <- navbarPage(
  "Assignment 1 - Giang Bui",
  #theme = bslib::bs_theme(bootswatch = "litera"),
  navbarMenu("Data Summary", 
             tabPanel("Raw Data", "Raw data:",
                      dataTableOutput(outputId = "Rawdata")),
             tabPanel("Data Structure", "Data structure:",
                      verbatimTextOutput(outputId = "Structure")),
             tabPanel("Dataframe Summary", "Dataframe",
                      htmlOutput(outputId = "Dfsummary"))
  ),
  navbarMenu("Categorical Analysis", 
             tabPanel("Bar Plot",
                      varSelectInput("categorical_var1", "Choose a categorical variable to chart::",cat_data),
                      varSelectInput("categorical_var2", "Choose a categorical variable to fill the chart::",
                                     cat_data),
                      plotOutput(outputId = "barplot")),
             tabPanel("MosaicCharts",
                      selectizeInput(inputId = "VariablesA", 
                                     label = "Choose Categorical Variables to chart:", 
                                     choices = option1, multiple = TRUE, 
                                     selected = c("Price","Speed", "Temp")),
                      plotOutput(outputId = "Mosaic")
                      )
  ),
  navbarMenu("Nummeric Analysis", 
             tabPanel("Histograms",
                      varSelectInput("VariableB", "Choose Numeric Variable to chart::", num_data),
                      sliderInput(inputId = "bins",
                                  "Number of bins:",
                                  min = 1,
                                  max = 50,
                                  value = 30),
                      plotOutput(outputId = "Histogram")
             ),
             
             tabPanel("BoxPlots",
                      checkboxInput(inputId = "standardise", 
                                    label = "Show standardized", value = FALSE),
                      checkboxInput(inputId = "outliers", 
                                    label = "Show outliers", value = TRUE),
                      sliderInput(inputId = "IQR", label = "IQR Multiplier", 
                                  min = 0, max = 5, step = 0.1, value = 1.5),
                      plotOutput(outputId = "Boxplot")
             ),
             tabPanel("CorrgramPlots",
                      checkboxInput(inputId = "abs", 
                                    label = "Apply absolute correlation", value = TRUE),
                      selectInput(inputId = "CorrMeth", label = "Correlation method", 
                                  choices = c("pearson","spearman","kendall"), 
                                  selected = "spearman"),
                      selectInput(inputId = "Group", label = "Grouping method", 
                                  choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), 
                                  selected = "OLO"),
                      plotOutput(outputId = "Corrgram"),
             ),
             tabPanel("MatPlots",
                      selectizeInput(inputId = "VariablesC", 
                                     label = "Choose Variables to chart:", 
                                     choices = option2, multiple = TRUE, 
                                     selected = special_sensor),
                      checkboxInput(inputId = "Center", label = "Center", value = TRUE),
                      checkboxInput(inputId = "Scale", label = "Scale", value = TRUE),
                      plotOutput(outputId = "MatPlot")
             ),
             tabPanel("Risingcharts",
                      selectizeInput(inputId = "VariablesD", 
                                     label = "Choose Variables to chart: ", 
                                     choices = option2, multiple = TRUE, 
                                     selected = special_sensor),
                      checkboxInput(inputId = "standardise_ris", 
                                    label = "Show standardized", value = TRUE),
                      plotOutput(outputId = "Risingchart")
             ),
  ),
  navbarMenu("Mixed-Pairs Nummerical and Categorical Analysis", 
             tabPanel("MixedPairs Numeric - Categorical",
                      checkboxGroupInput(inputId = "VariablesE", 
                                         label = "Choose Categorical Variables to chart:", 
                                         choices = option1, 
                                         inline = TRUE,
                                         selected = "Id_Group"),
                      selectInput(inputId = "VariablesG", 
                                  label = "Choose Numeric Variables to chart:", multiple = TRUE,
                                  choices = option2,
                                  selected = c("sensor2", "sensor3", "sensor5", "Y")
                                  ),
                     checkboxInput("select_all_groupA", "Select Y & sensor1 to sensor10"),
                     checkboxInput("select_all_groupB", "Select Y & sensor11 to sensor20"),
                     checkboxInput("select_all_groupC", "Select Y & sensor21 to sensor30"),
                     radioButtons(inputId = "VariablesF", 
                                  label = "Choose Categorical Variable to colour:", 
                                  choices = option1, 
                                  inline = TRUE, 
                                  selected = "Id_Group"),
                     plotOutput(outputId = "MixedPairs", height = "1150", width = "1150")
                     
             ),
             
  ),
  navbarMenu("Missingness", 
             tabPanel("visdat::vis_miss",
                      checkboxInput(inputId = "cluster", 
                                    label = "Cluster missingness", value = FALSE),
                      plotOutput(outputId = "Missing_1")
             ),
             
  ),
  
)


