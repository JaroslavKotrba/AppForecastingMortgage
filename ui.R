# Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

# ui
ui <- dashboardPage(
  skin='blue',
  
  dashboardHeader(
    title = "Calculation of Payments for a Customized Mortgage",
    titleWidth = 600
  ),
  
  dashboardSidebar(
    width = 300,
    br(),
    h4(strong("Your Inputs"), style="padding-left: 15px"),
    
    numericInput(
      inputId = "mortgage",
      label = "Select a mortgage amount:",
      value = 3000000,
      min = 0, max = 10000000, step = 100000
    ),
    
    numericInput(
      inputId = "i.m",
      label = "Select mortgage rate (we start at 3%): ",
      value = 0.03,
      min = 0.01, max = 0.99, step = 0.001
    ),
    
    selectInput(
      inputId = "year",
      label = "Select how many years: ",
      choices = c(1:100),
      selected = 15
    ),
    
    numericInput(
      inputId = "inflation",
      label = "Select inflation rate (we start at 3%): ",
      value = 0.03,
      min = 0.01, max = 0.99, step = 0.001
    )
    
  ),
  
  dashboardBody(
    tabsetPanel(
      type = "pills", # tabs look
      id = "tab_selected",
      # tab panel number 1
      tabPanel(
        title = "Mortgage",
        style="padding-top: 15px",
        h4(strong("Mortgage calculation:"), style="padding-left: 15px"),
        verbatimTextOutput("payment_per_month"),
        verbatimTextOutput("payment_total"),
        verbatimTextOutput("payment_difference"),
        verbatimTextOutput("payment_more"),
        verbatimTextOutput("payment_salary"),
        verbatimTextOutput("payment_part"),
        h4(strong("Inflation calculation:"), style="padding-left: 15px"),
        verbatimTextOutput("mortgage_inflation"),
        h4(strong("Your payment:"), style="padding-left: 15px"),
        plotlyOutput("plotly"),
        p("\n"),
        uiOutput("link", style="padding-left: 20px")
      ),
      # tab panel number 2
      tabPanel(
        title = "CNB Rates",
        style="padding-top: 15px",
        h5(strong("Here is the development of the three main rates given by the CNB, the output is directly taken from the main web page of the CNB."), style="padding-left: 15px"),
        h4(strong("Repo rates:"), style="padding-left: 15px"),
        plotOutput("repo"),
        h4(strong("Discount rates:"), style="padding-left: 15px"),
        plotOutput("disco"),
        h4(strong("Lombard rates:"), style="padding-left: 15px"),
        plotOutput("lombard"),
        p("\n"),
        uiOutput("link2", style="padding-left: 20px")
      )
    )
  )
)
