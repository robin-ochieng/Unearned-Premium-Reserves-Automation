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

data <- read_csv("C:/Users/Robin Ochieng/Kenbright/Automation and Risk Margin - General/Kenya/Reserving/UPR/Occidental/Data/Data_v1.csv", 
                 col_types = cols(BegDate = col_date(format = "%m/%d/%Y"), 
                                  EndDate = col_date(format = "%m/%d/%Y"),
                                  Premium = col_number(),
                                  Commission = col_number()))

Val_Date<- mdy("03/31/2024")

data <- data %>%
  mutate(
    Duration = as.numeric(difftime(EndDate, BegDate, units = "days")) + 1,
    Unearned_Duration = ifelse(EndDate >= Val_Date, (EndDate - Val_Date), 0),
    Earned_Duration = Duration - Unearned_Duration,
    Gross_UPR= (Unearned_Duration/Duration)*Premium,
    DAC = round(Unearned_Duration/Duration)*Commission,digits = 7,
    GEP = (Earned_Duration/Duration)*Premium)

sum(data$Gross_UPR,na.rm = TRUE)
sum(data$DAC,na.rm = TRUE)

define_year <- function(year) {
  Year_start <- mdy(paste('1/1/', year))
  Year_end <- mdy(paste('12/31/', year))
  
  return(list(Year_start = Year_start, Year_end = Year_end))}



define_year_span <- function(year) {
  Year_start <- as.Date(paste(year, "01-01", sep = "-"))  # Start of the year
  Year_end <- as.Date(paste(year, "12-31", sep = "-"))    # End of the year
  return(list(Year_start = Year_start, Year_end = Year_end))
}

calculate_yearly_metric <- function(data, year_span) {
  yearly_metric <- (pmax(0, pmin(year_span$Year_end, data$EndDate) - 
                           pmax(year_span$Year_start, data$BegDate) + 1) / 
                      data$Duration) * data$Premium
  return(yearly_metric)
}

# Iterate over the years from 2013 to 2024
years <- 2013:2024
for (year in years) {
  year_span <- define_year_span(year)
  
  # Calculate the yearly metric
  yearly_metric_col <- paste0("Year_", year)
  data[[yearly_metric_col]] <- calculate_yearly_metric(data, year_span)
}

define_year_span <- function(year) {
  if (year == 2024) {
    Year_start <- as.Date("2024-01-01")
    Year_end <- as.Date("2024-03-31")  # Adjusting for Q1 only
  } else {
    Year_start <- as.Date(paste(year, "01-01", sep = "-"))
    Year_end <- as.Date(paste(year, "12-31", sep = "-"))
  }
  return(list(Year_start = Year_start, Year_end = Year_end))
}

# Summarize data by IRA CLASS and EP for each quarter and year
summary_data <- data %>%
  group_by(`IRA Class`) %>%
  summarize(across(contains("Year_"), sum, na.rm = TRUE))

