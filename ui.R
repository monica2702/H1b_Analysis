#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(    
  
  # Give the page a title
  titlePanel("H1B Filings from 2011-2016"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
      #First Drop-down Input of Employers
      # selectInput("employer", "Employer: ", 
      #             choices=employerNames),
      selectizeInput("employer", "Employer: ", 
                  choices=employerNames,selected=NULL,multiple=FALSE),
      hr(),
      
      #Second - Check box group output of Case Status
      checkboxGroupInput("status", label = h3("Status of Application: "), 
                         choices = caseStatus, selected = caseStatus
                         
      ),
      hr(),
      
      #Slide input for range of Salaries
      uiOutput("interaction_slider"),
      
      hr(),
      
      helpText("Data of H1b filing from 2011-16 from Kaggle")
      
      
    ),
    
    # Create a spot for the barplot
    mainPanel(
      tabsetPanel(
        
        tabPanel("Statistical",
          plotOutput("yearPlot",hover = hoverOpts("plotHover")),
          column(width = 5,verbatimTextOutput("debug")),
          column(width = 5,verbatimTextOutput("hoverText")),
          column(12,plotOutput("jobPiePLot")),
          column(12,
                 dataTableOutput('year_table')
          )
        ),
        tabPanel("GIS",verbatimTextOutput("testText"),
                 fluidPage(
                   plotOutput("heatMap")
                 ),
                 leafletOutput("bubbleMap")
                 ),
        tabPanel("Prediction",
                 fluidRow(
                   selectizeInput("pEmployer", "Employer: ", 
                                  choices=c("NA",employerNames),selected=NULL,multiple=FALSE),
                   selectizeInput("pSoc", "Job: ", 
                                  choices=c(jobSoc),selected=NULL,multiple=FALSE),
                   selectizeInput("pLoc", "State: ", 
                                  choices=c(stateList),selected=NULL,multiple=FALSE),
                   numericInput("pSal", 
                                h3("Salary"), 
                                value = 80000)
                   ),
                  actionButton("predict", "Predict Result"),
                 verbatimTextOutput("predictionResult"),
                 gaugeOutput("sucessGauge")
                 )
                 )
      )
      
    )
    
  )
)



