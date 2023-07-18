shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Giang Bui"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             htmlOutput(outputId = "Dfsummary"),
             checkboxGroupInput(inputId = "VariablesE", 
                                label = "Choose Categorical Variables to chart:", 
                                choices = c("BloodType"), 
                                inline = TRUE,
                                selected = "BloodType"),
             selectInput(inputId = "VariablesG",
                         label = "Choose Numeric Variables to chart:", multiple = TRUE,
                         choices = c("Alcohol", "Coffee", "Exercise","ChemoTreatments",
                                     "ReagentA","ReagentB","ReagentC","ReagentD","ReagentE","ReagentF","ReagentG",
                                     "ReagentH","ReagentI","ReagentJ","ReagentK","ReagentL","ReagentM","ReagentN", "Response"),
                         selected = c("ReagentF", "ReagentH", "ReagentJ","ReagentD", "ReagentN", "ReagentL", 
                                      "Alcohol", "Coffee", "Exercise", "Response")
             ),
             checkboxInput("select_all_groupA", "Select All"),
             radioButtons(inputId = "VariablesF",
                          label = "Choose Categorical Variable to colour:",
                          choices = c("BloodType"),
                          inline = TRUE,
                          selected = "BloodType"),
             actionButton(inputId = "ggpairgo", label = "Plot Now!", icon = icon("play")),
             p("Click on the 'play' button to show the plot", style = "color: red;"),
             br(),
             plotOutput(outputId = "MixedPairs", height = "1750", width = "1750"),
             
             
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  ),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "glmnet_importance")  #  <- typically this is specific to OLS
                                  ),
                                  
                         ),
                         
                         
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                        
                         tabPanel("nnls Model",
                                  verbatimTextOutput(outputId = "nnls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "nnls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(nnls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = nnls_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "nnls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "nnls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnls_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "nnls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnls_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "nnls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "nnls_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "nnls_ModelPlots"),
                                  verbatimTextOutput(outputId = "nnls_Recipe"),
                                  verbatimTextOutput(outputId = "nnls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "nnls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         
                         
                         tabPanel("BstLm Model",
                                  verbatimTextOutput(outputId = "BstLm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "BstLm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(BstLm_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = BstLm_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "BstLm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "BstLm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "BstLm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "BstLm_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "BstLm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "BstLm_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "BstLm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "BstLm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "BstLm_ModelPlots"),
                                  verbatimTextOutput(outputId = "BstLm_Recipe"),
                                  verbatimTextOutput(outputId = "BstLm_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "BstLm_importance")  #  <- typically this is specific to OLS
                                  ),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "BstLm_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         tabPanel("bayesglm Model",
                                  verbatimTextOutput(outputId = "bayesglm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "bayesglm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bayesglm_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bayesglm_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "bayesglm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bayesglm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "bayesglm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bayesglm_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "bayesglm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bayesglm_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "bayesglm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bayesglm_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "bayesglm_ModelPlots"),
                                  verbatimTextOutput(outputId = "bayesglm_Recipe"),
                                  verbatimTextOutput(outputId = "bayesglm_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "bayesglm_importance")  #  <- typically this is specific to OLS
                                  ),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "bayesglm_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         tabPanel("glmStepAIC Model",
                                  verbatimTextOutput(outputId = "glmStepAIC_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmStepAIC_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmStepAIC_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmStepAIC_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmStepAIC_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmStepAIC_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmStepAIC_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmStepAIC_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "glmStepAIC_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmStepAIC_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "glmStepAIC_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmStepAIC_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "glmStepAIC_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmStepAIC_Recipe"),
                                  verbatimTextOutput(outputId = "glmStepAIC_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmStepAIC_Coef")  #  <- typically this is specific to OLS
                                  ),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "glmStepAIC_importance")  #  <- typically this is specific to OLS
                                  ),
                                  
                         ),
                         
                         
                         tabPanel("gam Model",
                                  verbatimTextOutput(outputId = "gam_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "gam_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(gam_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = gam_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "gam_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gam_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "gam_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gam_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "gam_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gam_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "gam_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gam_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "gam_ModelPlots"),
                                  verbatimTextOutput(outputId = "gam_Recipe"),
                                  verbatimTextOutput(outputId = "gam_ModelSummary2"),
                                  # wellPanel(
                                  #   h3("Variable Importance"),
                                  #   plotOutput(outputId = "gam_importance")  #  <- typically this is specific to OLS
                                  # ),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "gam_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         
                         
                         tabPanel("gaussprPoly Model",
                                  verbatimTextOutput(outputId = "gaussprPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "gaussprPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(gaussprPoly_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = gaussprPoly_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "gaussprPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "gaussprPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "gaussprPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "gaussprPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gaussprPoly_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "gaussprPoly_ModelPlots"),
                                  verbatimTextOutput(outputId = "gaussprPoly_Recipe"),
                                  verbatimTextOutput(outputId = "gaussprPoly_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "gaussprPoly_importance")  #  <- typically this is specific to OLS
                                  )
                                  
                         ),
                         
                         
                         tabPanel("svmPoly Model",
                                  verbatimTextOutput(outputId = "svmPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmPoly_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmPoly_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "svmPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "svmPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmPoly_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "svmPoly_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmPoly_Recipe"),
                                  verbatimTextOutput(outputId = "svmPoly_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "svmPoly_importance")
                                  )
                                  
                                  
                         ),
                         
                         
                         
                         
                         tabPanel("gaussprRadial Model",
                                  verbatimTextOutput(outputId = "gaussprRadial_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "gaussprRadial_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(gaussprRadial_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = gaussprRadial_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "gaussprRadial_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprRadial_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "gaussprRadial_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprRadial_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "gaussprRadial_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprRadial_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "gaussprRadial_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gaussprRadial_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "gaussprRadial_ModelPlots"),
                                  verbatimTextOutput(outputId = "gaussprRadial_Recipe"),
                                  verbatimTextOutput(outputId = "gaussprRadial_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "gaussprRadial_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "gaussprRadial_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         
                         tabPanel("krlsPoly Model",
                                  verbatimTextOutput(outputId = "krlsPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "krlsPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(krlsPoly_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = krlsPoly_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "krlsPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "krlsPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsPoly_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "krlsPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsPoly_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "krlsPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "krlsPoly_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "krlsPoly_ModelPlots"),
                                  verbatimTextOutput(outputId = "krlsPoly_Recipe"),
                                  verbatimTextOutput(outputId = "krlsPoly_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "krlsPoly_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "krlsPoly_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         
                         
                         # tabPanel("rvmLinear Model",
                         #          verbatimTextOutput(outputId = "rvmLinear_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                         #                   selectizeInput(inputId = "rvmLinear_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(rvmLinear_initial,ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = rvmLinear_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                         #                   bsTooltip(id = "rvmLinear_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         # 
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rvmLinear_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "rvmLinear_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rvmLinear_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "rvmLinear_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rvmLinear_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "rvmLinear_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "rvmLinear_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "rvmLinear_ModelPlots"),
                         #          verbatimTextOutput(outputId = "rvmLinear_Recipe"),
                         #          verbatimTextOutput(outputId = "rvmLinear_ModelSummary2"),
                         #          wellPanel(
                         #            h3("Variable Importance"),
                         #            plotOutput(outputId = "rvmLinear_importance")  #  <- typically this is specific to OLS
                         #          ),
                         #          wellPanel(
                         #            h3("Coefficients"),
                         #            tableOutput(outputId = "rvmLinear_Coef")  #  <- typically this is specific to OLS
                         #          )
                         # ),
                         
                         
                         
                         
                         tabPanel("rf Model",
                                  verbatimTextOutput(outputId = "rf_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rf_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rf_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rf_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rf_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "rf_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "rf_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rf_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rf_ModelPlots"),
                                  verbatimTextOutput(outputId = "rf_Recipe"),
                                  verbatimTextOutput(outputId = "rf_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "rf_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "rf_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         
                         # tabPanel("bag Model",
                         #          verbatimTextOutput(outputId = "bag_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                         #                   selectizeInput(inputId = "bag_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(bag_initial,ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = bag_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                         #                   bsTooltip(id = "bag_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         # 
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "bag_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "bag_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "bag_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "bag_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "bag_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "bag_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "bag_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "bag_ModelPlots"),
                         #          verbatimTextOutput(outputId = "bag_Recipe"),
                         #          verbatimTextOutput(outputId = "bag_ModelSummary2"),
                         #          wellPanel(
                         #            h3("Variable Importance"),
                         #            plotOutput(outputId = "bag_importance")  #  <- typically this is specific to OLS
                         #          ),
                         #          wellPanel(
                         #            h3("Coefficients"),
                         #            tableOutput(outputId = "bag_Coef")  #  <- typically this is specific to OLS
                         #          )
                         # ),
                         
                         
                         
                         tabPanel("bagEarthGCV Model",
                                  verbatimTextOutput(outputId = "bagEarthGCV_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "bagEarthGCV_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bagEarthGCV_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bagEarthGCV_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "bagEarthGCV_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarthGCV_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "bagEarthGCV_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarthGCV_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "bagEarthGCV_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarthGCV_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "bagEarthGCV_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bagEarthGCV_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "bagEarthGCV_ModelPlots"),
                                  verbatimTextOutput(outputId = "bagEarthGCV_Recipe"),
                                  verbatimTextOutput(outputId = "bagEarthGCV_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "bagEarthGCV_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "bagEarthGCV_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         
                         tabPanel("glmboost Model",
                                  verbatimTextOutput(outputId = "glmboost_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmboost_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmboost_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmboost_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmboost_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmboost_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmboost_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmboost_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "glmboost_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmboost_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "glmboost_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmboost_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmboost_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmboost_Recipe"),
                                  verbatimTextOutput(outputId = "glmboost_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "glmboost_importance")  #  <- typically this is specific to OLS
                                  ),
                                  hr()
                                  
                         ),
                         
                         
                         
                         tabPanel("M5 Model",
                                  verbatimTextOutput(outputId = "M5_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "M5_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(M5_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = M5_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "M5_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "M5_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "M5_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "M5_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "M5_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "M5_ModelPlots"),
                                  verbatimTextOutput(outputId = "M5_Recipe"),
                                  verbatimTextOutput(outputId = "M5_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "M5_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "M5_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         
                         
                         tabPanel("cubist Model",
                                  verbatimTextOutput(outputId = "cubist_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "cubist_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(cubist_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = cubist_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "cubist_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "cubist_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "cubist_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "cubist_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "cubist_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "cubist_ModelPlots"),
                                  verbatimTextOutput(outputId = "cubist_Recipe"),
                                  verbatimTextOutput(outputId = "cubist_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "cubist_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "cubist_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         # tabPanel("dnn Model",
                         #          verbatimTextOutput(outputId = "dnn_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                         #                   selectizeInput(inputId = "dnn_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(dnn_initial,ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = dnn_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                         #                   bsTooltip(id = "dnn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #                   
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "dnn_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "dnn_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "dnn_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "dnn_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "dnn_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "dnn_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "dnn_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "dnn_ModelPlots"),
                         #          verbatimTextOutput(outputId = "dnn_Recipe"),
                         #          verbatimTextOutput(outputId = "dnn_ModelSummary2"),
                         #          wellPanel(
                         #            h3("Variable Importance"),
                         #            plotOutput(outputId = "dnn_importance")  #  <- typically this is specific to OLS
                         #          ),
                         #          wellPanel(
                         #            h3("Coefficients"),
                         #            tableOutput(outputId = "dnn_Coef")  #  <- typically this is specific to OLS
                         #          )
                         # ),
                         
                         
                         tabPanel("mlp Model",
                                  verbatimTextOutput(outputId = "mlp_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "mlp_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(mlp_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = mlp_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "mlp_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "mlp_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "mlp_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "mlp_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "mlp_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "mlp_ModelPlots"),
                                  verbatimTextOutput(outputId = "mlp_Recipe"),
                                  verbatimTextOutput(outputId = "mlp_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "mlp_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "mlp_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         tabPanel("avNNet Model",
                                  verbatimTextOutput(outputId = "avNNet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "avNNet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(avNNet_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = avNNet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "avNNet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "avNNet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "avNNet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "avNNet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "avNNet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "avNNet_ModelPlots"),
                                  verbatimTextOutput(outputId = "avNNet_Recipe"),
                                  verbatimTextOutput(outputId = "avNNet_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "avNNet_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "avNNet_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         tabPanel("pcaNNet Model",
                                  verbatimTextOutput(outputId = "pcaNNet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pcaNNet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pcaNNet_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pcaNNet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pcaNNet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcaNNet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pcaNNet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcaNNet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "pcaNNet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcaNNet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "pcaNNet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pcaNNet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pcaNNet_ModelPlots"),
                                  verbatimTextOutput(outputId = "pcaNNet_Recipe"),
                                  verbatimTextOutput(outputId = "pcaNNet_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "pcaNNet_importance")  #  <- typically this is specific to OLS
                                  ),
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "pcaNNet_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         tabPanel("brnn Model",
                                  verbatimTextOutput(outputId = "brnn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "brnn_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(brnn_initial,ppchoices)),
                                                          multiple = TRUE,
                                                          selected = brnn_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "brnn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "brnn_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "brnn_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "brnn_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "brnn_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "brnn_str"),
                                  plotOutput(outputId = "brnn_ModelPlots"),
                                  verbatimTextOutput(outputId = "brnn_Recipe"),
                                  verbatimTextOutput(outputId = "brnn_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "brnn_importance")  #  <- typically this is specific to OLS
                                  )
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "brnn_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
