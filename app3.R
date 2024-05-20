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
library(scales)

options(shiny.maxRequestSize = 1000 * 1024^2)  # 100 MB

# Define the User Interface for the Application
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Mulish", rel = "stylesheet"),
    tags$style(HTML("
      body {font-family: 'Mulish',
      sans-serif;
      background-color: #f4f4f4;
      }
      .instruction-text {
        color: #2c3e50; /* Dark blue color for text */
        font-size: 9px; /* Slightly larger font size for readability */
        margin-top: 20px; /* Add some space above the text */
      }
      .instruction-header {
        font-size: 12px;
        color: #e74c3c; /* Red color for headings to grab attention */
        font-weight: bold; /* Make the header bold */
        margin-bottom: 5px; /* Space below the header */
      }
      .shiny-output-error { color: #ff0000;}
      .shiny-output-error:before {content: 'Error: ';}
      .well {
        background-color: #ffffff;  /* White background for sidebar and panels */
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);  /* Soft shadow for depth */
      }
      h3 {
        color: #333333;  /* Dark grey color for headings */
        text-align: center;  /* Center-align the headings */
        font-weight: bold; 
      }
      .btn {  /* Styling buttons */
        background-color: #007bff;  /* Bootstrap primary color */
        color: white;
      }
      .btn:hover {
        background-color: #0056b3;
      }
    "))
  ),
  titlePanel("Unearned Premium Reserve Tool", windowTitle = "Unearned Premium Reserving Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = c(".csv")),
      tags$div(
        class = "instruction-text",
        tags$p(class = "instruction-header", "Data Upload Guidelines:"),
        tags$ul(
          tags$li("Ensure the data format is CSV."),
          tags$li("Label columns as follows:"),
          tags$ul(
            tags$li("BegDate: Representing the policy start date."),
            tags$li("EndDate: Representing the policy end date."),
            tags$li("IRA CLASS: Should be the statutory class.")
          )
        )
      ),
      textInput("valDate", "Valuation Date", value = "03/31/2024"),
      numericInput("cutoffYear", "Set Policy Start Year Threshold", value = 2013, min = 1990, max = 3000),
      numericInput("startYear", "Start Year of Analysis", value = 2021, min = 2000, max = 3000),
      numericInput("endYear", "End Year of Analysis", value = 2024, min = 2000, max = 3000),
      selectInput("endYearQuarter", "Select Quarter for End Year of Analysis", choices = c("All" = "All", "Q1" = "Q1", "Q2" = "Q2", "Q3" = "Q3", "Q4" = "Q4"), selected = "All"),
      actionButton("goButton", "Run Analysis"),
      br(),
      br(),
      actionButton("calcUPR", "Calculate Sum of Gross UPR"),
      br(),
      br(),
      actionButton("calcDAC", "Calculate Sum of DAC"),
      br(),
      br(),
      actionButton("calcClassWiseUPR", "Calculate Class-wise Gross UPR Sum"),
      br(),
      br(),
      actionButton("showGraph", "Class-wise Gross UPR Graph", class = "btn-primary")
      
      

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Overview",
                 
                 tableOutput("viewData")),
        tabPanel("Earned Premiums Summary by IRA Class", tableOutput("summaryData"), downloadButton("downloadData", "Download Earned Premiums Summary by IRA Class as CSV")),
        tabPanel("Summaries",
                 verbatimTextOutput("UPRSum"),
                 br(),
                 verbatimTextOutput("DACSum"),
                 br(),
                 h3("Class-wise Gross UPR Summarization"),
                 br(),
                 downloadButton("downloadUPR", "Download the Gross UPR Summary Table"),
                 br(),
                 tableOutput("classWiseUPR"),
                 br(),
                 plotOutput("classWiseUPRPlot")),
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
    head(processedData(), n=30)
  })
  
  # Calculate and display Gross UPR Sum
  output$UPRSum <- renderText({
    req(input$calcUPR, processedData())
    upr_sum <- sum(processedData()$Gross_UPR, na.rm = TRUE)
    formatted_upr_sum <- comma(upr_sum)
    paste("The Sum of Gross UPR is:", formatted_upr_sum)
  })
  
  # Calculate and display DAC Sum
  output$DACSum <- renderText({
    req(input$calcDAC, processedData())
    dac_sum <- sum(processedData()$DAC, na.rm = TRUE)
    formatted_dac_sum <- comma(dac_sum)
    paste("The Sum of DAC is:", formatted_dac_sum)
  })
  
  # Reactive function for class-wise UPR summarization
  classWiseUPR <- eventReactive(input$calcClassWiseUPR, {
    req(processedData())
    data <- processedData()
    data %>%
      group_by(`IRA Class`) %>%
      summarise(Class_wise_UPR_Sum = sum(Gross_UPR, na.rm = TRUE)) %>%
      mutate(Class_wise_UPR_Sum = scales::comma(Class_wise_UPR_Sum))  # Format numbers with commas
  })
  
  # Display class-wise UPR summarization with enhanced styling
  output$classWiseUPR <- renderTable({
    req(classWiseUPR())
    classWiseUPR()
  }, sanitize.text.function = function(x) x)
  
  # Define download handler for the UPR table
  output$downloadUPR <- downloadHandler(
    filename = function() {
      paste("Class-wise-Gross-UPR-Summary", Sys.Date(), ".csv", sep = "")  # Construct filename
    },
    content = function(file) {
      req(classWiseUPR())  # Ensure data is ready before download
      write.csv(classWiseUPR(), file, row.names = FALSE)  # Write the data to a CSV file
    }
  )

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
    summarized <- data %>%
      group_by(`IRA Class`) %>%
      summarize(across(contains("_EP"), sum, na.rm = TRUE))
    
    # Apply formatting with commas to all summarized columns
    summarized <- summarized %>%
      mutate(across(contains("_EP"), scales::comma))
    return(summarized)
  })
  
  # Display summarized data
  output$summaryData <- renderTable({
    req(summaryData())
    summaryData()
  }, sanitize.text.function = function(x) x)
  

  
  # Add a download handler for the summary data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Earned Premiums Summary-Data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(summaryData())  # Ensure the summary data is ready before attempting to download
      write.csv(summaryData(), file, row.names = FALSE)
    }
  )
  # Render the bar graph for class-wise UPR
  output$classWiseUPRPlot <- renderPlot({
    req(classWiseUPR())  # Ensure the data is available
    
    # Prepare the data by removing commas for numeric conversion
    data <- classWiseUPR() %>%
      mutate(Class_wise_UPR_Sum = as.numeric(gsub(",", "", Class_wise_UPR_Sum)))
    
    # Create the bar graph
    ggplot(data, aes(x = `IRA Class`, y = Class_wise_UPR_Sum, fill = `IRA Class`)) +
      geom_bar(stat = "identity", color = "black", fill = "#0137A6") +
      labs(title = "Class-wise Gross UPR Summary",
           x = "IRA Class",
           y = "Gross UPR Sum") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
            axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for better readability
            legend.position = "none")  # Hide legend if not necessary
  })
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
