# ui.R
library(shiny)
library(ggplot2)

# Loading data file 
SubPerfData <- readRDS(file="SubPerfData.Rda")
##Exploring dataset variables
dataset <- with(SubPerfData, data.frame(NumFunc, ReceitaB, SP,SL,SC,ST,SM))

shinyUI(fluidPage(
  
  titlePanel("Modeling on a KM Data Set using Regression"),
  br(),
    
   sidebarLayout(
                sidebarPanel(),
                mainPanel(
                  h4("In the discussion presented here, we'll use regression analysis for estimating the relationships among variables.",align = "center" ),
                  h4("For this study, we have a R dataset (SubPerfData.Rda) with 81 observations and 26 variables, with data gathered using a questionaire applyed and tabulated from a number of Brazilian firms.", align = "center"),
                  h4("We select a subset with 7 variables from the original 26 using 'subset' function", align="center"),
                  h4("We want to fit a model using Regression Analysis to understand which among the independent variables are related to the dependent variable, and to explore the forms of these relationships.", align = "center")
                                   
                  )
                 ),
  sidebarLayout(position = "right",
                sidebarPanel( 
                  h4('The variables stands for:'),
                  h5('NumFunc - Number of employees'),
                  h5('ReceitaB - Gross Income'),
                  h5('SP - Performance in Process'),
                  h5('SL - Performance in Leadership'),
                  h5('SC - Performance in Culture'),
                  h5('ST - Performance in Technology'),
                  h5('SM - Performance in Metrics')
                            ),
                mainPanel(
                  h4("First, let's look at the correlation between all the variables in the dataset."),
                  h4('Correlation Matrix'),
                  tableOutput('tabela'),
                  hr()
                  )
                ),
  fluidRow(
    column(3,
           h4("Exploratory Data"),
           br(),
           h5("Pick one option"),
           h5("Sorry! In this version, only linear model is operacional..."),
           checkboxInput(inputId = "varlm",
                         label = strong("Linear Model (lm)"),
                         value = TRUE),
           
           checkboxInput(inputId = "varglm",
                         label = strong("Generalized Linear Model (glm)"),
                         value = FALSE)
           
           ),
    column(2, 
           h5("Now, select the response variable (y) and the predictor (x)"),
           selectInput('x', 'X', names(dataset)),
           selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
           h5("When 'ok', to see the plot, click on 'Go'"),
           actionButton("goButton", label="Go!", value=0)
          ),
    column(2, offset=1, 
             textOutput("text1"),
             br(),
             tableOutput('resum')
           ),
    column(4, offset=2, 
             plotOutput('plot'),
             hr()
    )
),  

fluidRow(
    column(3,
       h3("Picking the best two variables"),
       br(),
       h4("Variable 'SL' may be selected and compared with all others")
           ),
    
    column(2, offset=1, 
           tableOutput('corresp')
           ),

     column(4, offset=2, 
           h4("As we can see, variable 'SL' has a good coorelation with variable 'SM'"),
           h4("And both parameters are statistically signficant at 95%"),
           tableOutput('model1'),
           hr()
            )
     ),
fluidRow(
  column(8,
         h4("This gives us an equation of SL =  2.1582 +  0.7381(SM)")           
  )
)

 )
)

