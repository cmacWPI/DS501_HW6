library(shiny)
library(tm)
library(dplyr)
library(tidyr)
library(readr)
library(plsdepot)
library(ggplot2)
library(visreg)
library(caret)
library(RCurl)

githubCSV <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid <- read_csv(githubCSV)
state <- covid %>%
  na.omit() %>% 
  select(state, cases, deaths) %>%
  group_by(state) %>%
  summarize(TotalCase = max(cases),TotalDeath = max(deaths))

dateCOVID <- covid %>%
  na.omit() %>% 
  select(state, date, cases, deaths)

firstState <- state$state[1]
firstDate <- dateCOVID$date[1]
colnames(state)[1] <- "State"
colnames(state)[2] <- "Date"

ui <- fluidPage(headerPanel('COVID-19 Analysis - Linear Regression'),
  title = "COIVD-19 Linear Regression Analysis",
  tabsetPanel(              
    tabPanel(title = "App",
             plotOutput("covidData"),
             mainPanel(
               h3(textOutput("selected_state")),
               verbatimTextOutput("lrData")
             ),
             sidebarPanel(
               selectInput("pickState", 
                           label = "Choose a state to display",
                           choices = c(state[1]),
                           selected = firstState),
               checkboxInput("checkRegLine", "Show regression line", FALSE),
               selectInput("pickDate", 
                           label = "Choose a date to display for table",
                           choices = c(dateCOVID[2]),
                           selected = firstDate)
             ),
             mainPanel(
               h3(textOutput("selected_date")),
               column(12, dataTableOutput('covidTable')
               ),
             )
    ),
    tabPanel(title = "About",
             h1("About this App", align = "center"),
             p("This app uses linear regression analysis to predict future number of deaths from COVID-19 in United States. The dataset is updated every day in real time."),
             h3("What is regression? And what is linear regression?", align = "center"),
             p("A regression is used to describe relationship between a single response variable and predictor variables. A linear regression is used to estimate 'a continuous value as a linear (additive) function of other variables' where input variables can be can be continuous or discrete. And their output can be either set of coefficients that have indicate the impact of each driver or as linear expression for predicting outcome as function of drivers."),
             p("In an sense, we are describing the relationship between two variables of X and Y, and after data collection, an pair of observation goes like (x1, y1)…..(xn, yn) and so forth"),
             h3("What is this dataset about and why are we using linear regression on COVID-19 dataset?", align = "center"),
             p("This dataset is about showing cumulative number of COVID cases and death based on count data from laboratory and probable cases from federal and government. It is continuously updated every day to show total cases and death of each state from COVID."),
             p("By using this dataset for linear regression, it allows us to predict an continuous outcome of cases and death for each state."),
             uiOutput("datasetCredit"),
             h3("My Code", align = "center"),
             uiOutput("code")
    )
  ),
)


server <- function(input, output) {
  url <- a("Coronavirus (Covid-19) Data in the United States", href="https://github.com/nytimes/covid-19-data")
  output$datasetCredit <- renderUI({
    tagList("Dataset is taken from", url, 'via GitHub, credits to them for providing this data.')
  })
  
  code <- a("My code", href="https://github.com/cmacWPI/DS501_HW6")
  output$code <- renderUI({
    tagList(code, 'can be found in GitHub')
  })
  stateSelect <- reactive({
    covid %>%
      na.omit() %>% 
      select(state, cases, deaths) %>%
      filter(state == input$pickState)
  })
  
  dateSelect <- reactive({
    table <- covid %>%
      na.omit() %>% 
      select(state, date, cases, deaths) %>%
      filter(date == input$pickDate)
    colnames(table)[1] <- "State"
    colnames(table)[2] <- "Date"
    colnames(table)[3] <- "Total Cases"
    colnames(table)[4] <- "Total Deaths"
    table
  })
  
  output$covidData <- renderPlot({
    lr = lm(deaths ~ cases, data=stateSelect())
    if(input$checkRegLine==TRUE)
    {
      qplot(cases, deaths, data=stateSelect()) + geom_point(colour = "#3366FF", size = 3) + geom_abline(intercept = lr[1]$coefficients[1], slope = lr[1]$coefficients[2], color="red")
    }
    else
    {
      qplot(cases, deaths, data=stateSelect()) + geom_point(colour = "#3366FF", size = 3)
    }
  })
  
  output$covidTable <- renderDataTable(dateSelect())
  output$selected_state <- renderText({ 
    paste("Linear Regression Data for ", input$pickState)
  })
  output$selected_date <- renderText({ 
    paste("Data for Total Cases and Deaths in ", input$pickDate)
  })
  output$lrData <- renderPrint({ 
    summary(lm(deaths ~ cases, data=stateSelect()))
  })

}

shinyApp(server = server, ui = ui)