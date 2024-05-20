library(tidyverse)
library(ChainLadder)
library(lubridate)
library(openxlsx)

data <- read_csv("Data.csv",
                 col_types = cols(Date_Loss = col_date(format = "%m/%d/%Y"), 
                                  Gross_Paid = col_number(), 
                                  Date_Paid = col_date(format = "%m/%d/%Y")))

View(data)#Output this data Triangle on a Tab

data <- data %>% 
  mutate(Acc_Quarters = as.yearqtr(Date_Loss),
         Loss_Year = year(Date_Loss),
         Dev_period = as.numeric(difftime(Date_Paid, Date_Loss, units = "days")) %/% 90 + 1)

# Define the Valiation_start and Valuation_end dates
Valuation_start_date <- mdy("1/1/2018")# integrate a calendar for date selection on my R Shiny application
Valuation_end_date <- mdy("3/31/2023")# integrate a calendar for date selection on my R Shiny application


# Calculate the number of quarters between start_date and end_date
Periods <- (year(end_date) - year(start_date)) * 4 + (quarter(end_date) - quarter(start_date) + 1)

Acc_period <- data %>% 
  filter(Loss_Year >= year(Valuation_start_date) & Loss_Year <= year(Valuation_end_date)) %>% 
  select(Acc_Quarters) %>%
  distinct() %>%
  arrange(Acc_Quarters) %>%
  rowwise() %>%
  mutate(Dev_period = list(seq(1, Periods, 1))) %>%
  unnest(Dev_period)

Val_class <- unique(data$Statutory_Class)#I need to have flexibility in picking all the distinct classes  options from this column in a widget

wb <- createWorkbook()

for (val_class in Val_class) {
  tryCatch({
    Val_Data <- data %>%
      filter(Statutory_Class == val_class) %>%
      select(Acc_Quarters, Gross_Paid, Dev_period) %>%
      group_by(Acc_Quarters, Dev_period) %>%
      summarise(Gross_Amount = sum(Gross_Paid)) %>%
      right_join(Acc_period)#Output this table on a Tab for each class
    
    Inc_Tri <- as.triangle(Val_Data,
                           origin = "Acc_Quarters",
                           dev = "Dev_period",
                           value = "Gross_Amount")#Output this Incremental Triangle on a Tab for each class
    
    Cum_Tri <- incr2cum(Inc_Tri)#Output this Cummulative Triangle on a Tab for each class
    
    Boot_Method <- BootChainLadder(Cum_Tri, R = 9999, process.distr = "gamma")
    
    summary_Boot_Method <- summary(Boot_Method)$Totals#Output this summary_Boot_Method_Totals Table on a tab named 'SUMMARIES' for each class
    confidence_level <- quantile(Boot_Method, c(0.6, 0.65, 0.7, 0.75, 1))$ByOrigin)#Output this confidence_level_byorigin Table on a tab named 'SUMMARIES' for each class
    
    # Create a new sheet for each class result
    addWorksheet(wb, sheetName = val_class)
    
    # Write summary_Boot_Method to the sheet
    writeData(wb, sheet = val_class, x = summary_Boot_Method, colNames = TRUE, rowNames = TRUE)
    
    # Write confidence_level to the sheet
    writeData(wb, sheet = val_class, x = confidence_level, startCol = ncol(summary_Boot_Method) + 3, colNames = TRUE, rowNames = TRUE)
  }, error = function(e) {
    cat(paste("Error occurred for class", val_class, "Skipping...\n"))
    return(NULL)
  })
}    

# Save the workbook
saveWorkbook(wb, "results10000GrossPaid.xlsx")
