library(shiny)
library(tidyverse)
library(readxl)
library(rio)
library(dplyr)
library(tidyr)
library(ChainLadder)
library(lubridate)
library(readr)
library(zoo)
library(ggplot2)

options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

# Define the User Interface for the Application
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Mulish", rel = "stylesheet"),
    tags$style(HTML("
      body {font-family: 'Mulish', sans-serif;}
      .shiny-output-error { color: #ff0000;}
      .shiny-output-error:before {content: 'Error: ';}
    "))
  ),
  titlePanel("Unearned Premium Reserve Tool", windowTitle = "Unearned Premium Reserving Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = c(".csv")),
      textInput("valDate", "Valuation Date", value = "03/31/2024"),
      numericInput("cutoffYear", "Cutoff Year", value = 2013, min = 1990, max = 3000),
      numericInput("startYear", "Start Year", value = 2021, min = 2000, max = 3000),
      numericInput("endYear", "End Year", value = 2024, min = 2000, max = 3000),
      selectInput("endYearQuarter", "Select Quarter for End Year", choices = c("All" = "All", "Q1" = "Q1", "Q2" = "Q2", "Q3" = "Q3", "Q4" = "Q4"), selected = "All"),
      actionButton("goButton", "Calculate"),
      br(),
      actionButton("calcUPR", "Calculate Gross UPR Sum"),
      actionButton("calcDAC", "Calculate DAC Sum")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data View", tableOutput("viewData")),
        tabPanel("Summary by IRA Class", tableOutput("summaryData"), downloadButton("downloadData", "Download Summary CSV")),
        tabPanel("Calculations", verbatimTextOutput("UPRSum"), verbatimTextOutput("DACSum"))
      )
    )
  )
)

# Define the server logic required to read the input and calculate outputs
server <- function(input, output, session) {
  
  # Reactive value for storing the processed data
  data <- eventReactive(input$goButton, {
    req(input$file1)
    inFile <- input$file1
    read_csv(inFile$datapath, 
             col_types = cols(
               Premium = col_number(), 
               BegDate = col_date(format = "%m/%d/%Y"), 
               EndDate = col_date(format = "%m/%d/%Y"), 
               Commission = col_number()))
  })
  
  # Process data when go button is clicked
  processedData <- eventReactive(input$goButton, {
    req(data())
    Val_Date <- mdy(input$valDate)
    data() %>%
      mutate(
        Duration = round(as.numeric(difftime(EndDate, BegDate, units = "days")) + 1, digits = 7),
        Unearned_Duration = ifelse(BegDate > Val_Date, 1, ifelse(EndDate <= Val_Date, 0, (EndDate - Val_Date) / Duration)),
        Earned_Duration = Duration - Unearned_Duration,
        Gross_UPR = Unearned_Duration * Premium,
        DAC = Unearned_Duration * Commission,
        Auth_Year = year(BegDate)
      )
  })
  
  # Display initial or processed data
  output$viewData <- renderTable({
    req(processedData())
    head(processedData())
  })
  
  # Calculate and display Gross UPR Sum
  output$UPRSum <- renderPrint({
    req(input$calcUPR, processedData())
    sum(processedData()$Gross_UPR, na.rm = TRUE)
  })
  
  # Calculate and display DAC Sum
  output$DACSum <- renderPrint({
    req(input$calcDAC, processedData())
    sum(processedData()$DAC, na.rm = TRUE)
  })
  
  # Reactive function for summarizing data by IRA Class
  summaryData <- eventReactive(input$goButton, {
    req(processedData())
    data <- processedData()
    for (yr in input$startYear:input$endYear) {
      year_quarters <- define_quarters(yr)
      quarters_to_iterate <- if (yr == input$endYear && input$endYearQuarter != "All") {
        input$endYearQuarter
      } else if (yr == input$endYear && input$endYearQuarter == "All") {
        c("Q1", "Q2", "Q3", "Q4")
      } else {
        c("Q1", "Q2", "Q3", "Q4")
      }
      
      for (quarter in quarters_to_iterate) {
        quarter_EP_col <- paste0(quarter, "_", yr, "_EP")
        data[[quarter_EP_col]] <- calculate_EP(data, year_quarters, quarter, yr, input$cutoffYear)
      }
    }
    data %>%
      group_by(`IRA Class`) %>%
      summarize(across(contains("_EP"), sum, na.rm = TRUE))
  })
  
  # Display summarized data
  output$summaryData <- renderTable({
    req(summaryData())
    summaryData()
  })
  
  # Add a download handler for the summary data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("summary-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(summaryData())  # Ensure the summary data is ready before attempting to download
      write.csv(summaryData(), file, row.names = FALSE)
    }
  )
  
}

# Define helper functions used in calculations
define_quarters <- function(year) {
  Q1_start <- mdy(paste('1/1/', year))
  Q1_end <- mdy(paste('3/31/', year))
  Q2_start <- mdy(paste('4/1/', year))
  Q2_end <- mdy(paste('6/30/', year))
  Q3_start <- mdy(paste('7/1/', year))
  Q3_end <- mdy(paste('9/30/', year))
  Q4_start <- mdy(paste('10/1/', year))
  Q4_end <- mdy(paste('12/31/', year))
  return(list(Q1_start = Q1_start, Q1_end = Q1_end,
              Q2_start = Q2_start, Q2_end = Q2_end,
              Q3_start = Q3_start, Q3_end = Q3_end,
              Q4_start = Q4_start, Q4_end = Q4_end))
}

calculate_EP <- function(data, year_quarters, quarter, year, cutoff_year) {
  quarter_EP <- ifelse(data$Auth_Year < cutoff_year, 0,
                       (pmax(0, pmin(year_quarters[[paste0(quarter, "_end")]], data$EndDate) -
                               pmax(year_quarters[[paste0(quarter, "_start")]], data$BegDate) + 1) /
                          data$Duration) * data$Premium)
  return(ifelse(is.na(quarter_EP), 0, quarter_EP))
}

# Run the application
shinyApp(ui = ui, server = server)