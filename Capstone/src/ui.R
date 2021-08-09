# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  theme = "style.css",
  # Title
  img(src = "logo.png", width = "270px", height = "25px"),
  h3("Comparison of Classification Performance of Random Forest and Logistic Regression"),
  hr(),
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar Inputs
    sidebarPanel(
      # Input: Number of Rows
      numericInput(inputId = "nrows",
                   label = "Number of Rows",
                   value = 100,min=100,max = 10000),
      
      # Input: Noise Variables
      numericInput(inputId = "noise",
                   label = "Noise Variables",
                   value = 5,min=0,max = 100),
      
      # Distribution for Noise Variables
      selectInput("ndist", label ="Noise Variable Distribution", 
                  choices = list("Normal" = "normal", 
                                 "Gamma" = "gamma"), 
                  selected = "normal"),
      
      # Input: Noise Variables Variance
      numericInput(inputId = "nvar",
                   label = "Noise Variance",
                   value = 1,min=0,max = 10,step=0.50),
      
      # Input: Explanatory Variables
      numericInput(inputId = "ev",
                   label = "Explanatory Variable",
                   value = 10, min=0, max = 100, step=1),
      
      # Input: Categorical Variables
      numericInput(inputId = "cat",
                   label = "Categorical Variable",
                   value = 0, min=0, max = 100, step=1),
      
      # Input: Explanatory Variables Weights
      selectInput(inputId = "weights",
                  label = "Explanatory Variable Coefficients",
                  choices = list("Uniform: 0.50" = "1", 
                                 "Small/Large: 0.30, 0.70" = "2",
                                 "Small/Medium/Large: 0.20, 0.50, 0.80" = "3"), 
                  selected = "1"),
      # Input: y-intercept
      numericInput(inputId = "yint",
                   label = "Y-Intercept",
                   value = 0.10, min=0,max = 1.0, step = 0.10),
      hr(),
      h4("Model Input Parameters"),            
      numericInput(inputId = "split",
                   label = "Train/Test Split",
                   value = 0.70, min=0,max = 1.0, step = 0.05),        
      
      hr(), 
      h4("Variable Selection Model"),            
      selectInput(inputId = "varselect",
                  label = "Variable Selection Method",
                  choices = list("backward" = "backward", 
                                 "forward" = "forward",
                                 "forward & backward" = "both"), 
                  selected = "forward"),      
      hr(), 
      h4("Random Forest Parameters"),      
      sliderInput(inputId = "ntree",                   
                  label = "Number of Trees",                   
                  value = 100, min=50,max = 550, step = 1),
      
      hr(),   
      h4("Probability Threshold"),      
      sliderInput(inputId = "prob_thresh",                   
                  label = "Probability",                   
                  value = 0.50, min=0,max = 1.0, step = 0.10),
      
      hr(), 
      h4("Simulation: Number of Iterations"),      
      sliderInput(inputId = "n_sim",                   
                  label = "Iterations",
                  value = 1, min=1,max = 10000, step = 1),
      
      br(),
      submitButton("Simulate Data", icon("refresh")),
      helpText("Press 'Simulate Data' to generate simulated dataset")
    ),
    
    # Main Panel
    mainPanel(
      
      # Outputs
      tabsetPanel(
        tabPanel("Simulated Data", 
                 fluidRow(
                   column(12,
                          h3("Equation of model:"),
                          h4(tableOutput("equation")),
                          p("Note: In the equation above, y is the response variable is a function of explanatory variables. In the table below, only the first 10 data row are displayed;  'y' is the log probability of the response variable and not the true y from the equation above.")
                   )
                 ),
                 br(),
                 fluidRow(column(12,tableOutput("table"))),
                 fluidRow(column(12,
                                 h4("Correlation of variables: "))),
                 fluidRow(column(12,plotOutput("cplot")))
        ), 
        tabPanel("Case 1", 
                 fluidRow(
                   column(12,
                          h4("Case Description"),
                          p("In this case, we simulate the dataset and set the variance of each variable (noise and explantory)
                            to 0.5 to 5.0 in an interval of 0.5 and display the results of each simulation.")
                          )
                   ),
                 tags$hr(),
                 fluidRow(
                   column(12,
                          h4("Logistic Regression Results"),
                          p(textOutput("lr_title_nvar"))
                   )
                 ),
                 fluidRow(column(12,align="center",tableOutput("lr_sim_nvar"))),
                 tags$hr(),
                 fluidRow(
                   column(12,
                          h4("Random Forest Results"),
                          p(textOutput("rf_title_nvar"))
                   )
                 ),
                 fluidRow(column(12,align="center",tableOutput("rf_sim_nvar"))),
                 tags$hr(),
                 fluidRow(column(12,h4("Paired Two Sample T-Test Results"))),
                 fluidRow(column(12,align="center",tableOutput("ttest1"))),
                 tags$hr(),
                 fluidRow(column(12,h4("Simulation Plots"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case1_chart1"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case1_chart2"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case1_chart3"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case1_chart4"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case1_chart5"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case1_chart6")))
                 ),
        tabPanel("Case 2",
                 fluidRow(
                   column(12,
                          h4("Case Description"),
                          p("In this case, we simulate the dataset and set the number of
                            noise variables to 1, 5, 10, 20, and 50 and display the results of each simulation.")
                          )
                   ),
                 tags$hr(),
                 fluidRow(
                   column(12,
                          h4("Logistic Regression Results"),
                          p(textOutput("lr_title_num_nvar"))
                   )
                 ),
                 fluidRow(column(12,align="center",tableOutput("lr_sim_num_nvar"))),
                 tags$hr(),
                 fluidRow(
                   column(12,
                          h4("Random Forest Results"),
                          p(textOutput("rf_title_num_nvar"))
                   )
                 ),
                 fluidRow(column(12,align="center",tableOutput("rf_sim_num_nvar"))),
                 tags$hr(),
                 fluidRow(column(12,h4("Paired Two Sample T-Test Results"))),
                 fluidRow(column(12,align="center",tableOutput("ttest2"))),
                 tags$hr(),
                 fluidRow(column(12,h4("Simulation Plots"))),
                 fluidRow(column(12,plotOutput("case2_chart1"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case2_chart2"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case2_chart3"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case2_chart4"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case2_chart5")))
                 ),
        tabPanel("Case 3",
                 fluidRow(
                   column(12,
                          h4("Case Description"),
                          p("In this case, we simulate the dataset and set the number of
                            explanatory variables to 1, 5, 10, 20, and 50 and display the results of each simulation.")
                          )
                   ),
                 tags$hr(),
                 fluidRow(
                   column(12,
                          h4("Logistic Regression Results"),
                          p(textOutput("lr_title_num_nevar"))
                   )
                 ),
                 fluidRow(column(12,align="center",tableOutput("lr_sim_num_evar"))),
                 tags$hr(),
                 fluidRow(
                   column(12,
                          h4("Random Forest Results"),
                          p(textOutput("rf_title_num_nevar"))
                   )
                 ),
                 fluidRow(column(12,align="center",tableOutput("rf_sim_num_evar"))),
                 tags$hr(),
                 fluidRow(column(12,h4("Paired Two Sample T-Test Results"))),
                 fluidRow(column(12,align="center",tableOutput("ttest3"))),
                 tags$hr(),
                 fluidRow(column(12,h4("Simulation Plots"))),
                 fluidRow(column(12,plotOutput("case3_chart1"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case3_chart2"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case3_chart3"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case3_chart4"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case3_chart5")))
                 ),
        tabPanel("Case 4",
                 fluidRow(
                   column(12,
                          h4("Case Description"),
                          p("In this case, we simulate the dataset and set the number of observations from 10 to 1000
                            with explanatory variables to 1 10, 20, and 50 and display the results of each simulation.")
                          )
                   ),
                 fluidRow(
                   column(12,
                          h4("Simulation Results")
                   )
                 ),
                 fluidRow(column(12,align="center",tableOutput("lr_rf_sim_num_nobs"))),
                 tags$hr(),
                 fluidRow(column(12,h4("Paired Two Sample T-Test Results"))),
                 fluidRow(column(12,align="center",tableOutput("ttest4"))),
                 tags$hr(),
                 fluidRow(column(12,h4("Simulation Plots"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart1"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart2"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart3"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart4"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart5"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart6"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart7"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart8"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart9"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart10"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart11"))),
                 tags$hr(),
                 fluidRow(column(12,plotOutput("case4_chart12")))
                 )
      ) # End tab panel
      ) # End main panel
    )
)
#End
