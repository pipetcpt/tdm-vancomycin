# setup ----

library(shinydashboard)
library(shinyTime)
library(lubridate)
library(shinythemes)

tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
)

# main ----

shiny::navbarPage(
  
  # Title page ----
  
  title = "Vancomycin TDM",
  theme = shinytheme("paper"),
  #selected = 'dosa', #icon = "prescription",
  
  # Chapter 1. Patient Info `pinfo` ----
  
  tabPanel(
    #icon = icon("user-plus"), 
    icon = icon("address-card"), 
    title = "Patient Info",
    tabName="pinfo",
    h2("Patient Information",
       align='center',
       style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    column(width=2),
    column(width=4,
           box(title = "Patient ID",width = NULL,solidHeadner = TRUE,status = "primary",
               textInput("pid","", value = "003866", width = NULL, placeholder = NULL)
           ),
           
           #tags$hr(),
           
           box(title = "Information", status = "primary",width = NULL,solidHeader = TRUE,
               numericInput("scr", "Serum Creatinine (mg/dL)", 0.9, min = NA, max = NA, step = 0.1,
                            width = NULL),
               
               radioButtons("sex", ("Gender"),inline=TRUE,
                            choices = list("Male" = "Male", "Female" = "Female"),selected = NULL, choiceName = "Male"),
               #selectInput("sex", "Select Gender", c("Male","Female"), selected = NULL, multiple = FALSE,
               #           selectize = TRUE, width = NULL, size = NULL)
               #,
               numericInput("age", "Age (year)", 40,min = NA, max = NA, step = 1),
               numericInput("weight", "Weight (kg)", 70, min = NA, max = NA, step = 0.1)
           )
    )
    ,
    column(width=4,
           box(width=NULL, 
               title="Creatinine Clearance (mL/min)",
               background = "maroon",
               verbatimTextOutput("creatinine_clearance")),
           p("**Cockcroff-Gault Equation")
    ),
    column(width=2)
  ),
  
  # Chapter 2. Dosage Regimen `dosa` ----
  
  tabPanel(
    icon = icon("eyedropper"),
    title = "Dosage regimen", 
    tabName="dosa",
    h2(" Dosage Regimen",align='center',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    column(width=2),
    column(width=4,
           box(width = NULL, status = "primary", 
               solidHeader = TRUE, 
               title="Upload Dosing History", 
               fileInput('file1', 'Choose File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
               tags$hr(),
               selectInput("rt", "Administration Route", c("IV Infusion"), selected = NULL, multiple = FALSE,
                           selectize = TRUE, width = NULL, size = NULL)
           ) ,
           tags$hr(),
           box(width=NULL,
               background = "teal",
               "Dosing History Template (CSV)",
               tags$p(), 
               downloadButton('downloadData', 'Download')
           )
    ),
    column(width=4,
           box( width=NULL, status = "primary", solidHeader = TRUE, title="Dosing History", 
                tableOutput('dosing_history_contents')
                
           )
           
    ),
    column(width=2)
  ),
  
  # Chapter 3. Observations `obs` ----
  
  tabPanel(
    icon = icon("flask"),
    title = "Obesrvations", 
    tabName="obs",
    h2("Observations",align='center',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    column(width=2),
    column(width=4,
           box(width = NULL, status = "primary", solidHeader = TRUE, title="Number of Observations",
               selectInput("Observations","",c("1" = "1","2" = "2"))
               #textInput("1")
           )
    ),
    column(width=4,
           conditionalPanel(
             condition = "input.Observations == 1", 
             box(width = NULL , solidHeader = TRUE, title="First Observation",
                 numericInput("obsc", "Observed Concentration (mg/L)", 5, min = NA, max = NA, step = NA,
                              width = NULL),
                 dateInput("obsDate", "Date", value = "2017-05-06", min = NULL, max = NULL,
                           format = "yyyy-mm-dd", startview = "month", weekstart = 1,
                           language = "en", width = NULL),
                 timeInput("obsTime", "Time", value = strptime("23:00", "%R"), seconds = FALSE)
             )
           ),
           conditionalPanel(
             condition = "input.Observations == 2",
             box(width = NULL ,solidHeader = TRUE, title="First Observation",
                 numericInput("obsc1", "Observed Concentration (mg/L)", 4.5, 
                              min = NA, max = NA, step = NA,
                              width = NULL),
                 textInput("obsd1", "Time", value = "2017-05-06", width = NULL, placeholder = NULL),
                 timeInput("obst1", "Date", value = strptime("23:00", "%R"), seconds = FALSE)
             ),
             tags$hr(),
             box(width = NULL ,solidHeader = TRUE, title="Second Observation",
                 numericInput("obsc2", "Observed Concentration (mg/L)", 10, 
                              min = NA, max = NA, step = NA,
                              width = NULL),
                 textInput("obsd2", "Time", value = "2017-01-01", width = NULL, placeholder = NULL),
                 timeInput("obst2", "Date", value = strptime("23:30", "%R"), seconds = FALSE)
             )
           )
    ),
    column(width=2)
  ),
  
  # Chapter 4. PK profile 1 `main` ----
  
  tabPanel(
    icon = icon("line-chart"),
    title = "PK profile 1", 
    tabName="main", 
    column(width=3,
           box( width=NULL, status = "primary", solidHeader = TRUE, title="", 
                tableOutput('output_table1_time_predicted_concentration')
           ),
           box( width=NULL, status = "primary", solidHeader = TRUE, title="", 
                tableOutput('outputtable2'),
                tags$hr()
           ),
           box( width=NULL, status = "primary", solidHeader = TRUE, title="", 
                tableOutput('outputtable3')
           )
    ),
    column(width=9,
           box(width = NULL, status = "primary", title="",
               plotOutput("plotCONC",  height="500px")))
  ),
  
  # Chapter 5. PK profile 2 `main2` ----
  
  tabPanel(
    icon = icon("line-chart"),
    title = "PK profile 2", 
    tabName="main2",
    column(width=3,
           box( width = NULL, status="warning", solidHeader = TRUE, title = "",
                sliderInput("newdose", "Next dose (mg)", 1000, min = 0, max = 2000, step = 250,ticks=TRUE,
                            width = NULL)),
           box( width = NULL, status="warning", solidHeader = TRUE, title = "",
                sliderInput("newtau", "New dosing interval (hours)", 12, min = 4, max = 48, step = 4,
                            width = NULL)),
           box( width = NULL, status="warning", solidHeader = TRUE, title = "",
                sliderInput("newinf", "Infusion duration", 1, min = 0.5, max = 4, step = 0.5,
                            width = NULL)),
           tags$hr(),
           box( width=NULL, solidHeader = TRUE, title='',status="primary",
                sliderInput("ll", "trough levels (mg/L)", 15, min = 10, max = 20, step = 5,
                            width = NULL),
                
                sliderInput("ul", "peak levels (mg/L)", 40 , min = 25, max = 50, step = 5,
                            width = NULL)
           )
    ),
    column(width=9,
           box(width = NULL, status = "primary", title="",
               plotOutput("plotCONC2",  height="500px")
           )
    )
  )
)
