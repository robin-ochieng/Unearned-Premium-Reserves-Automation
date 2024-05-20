library(tidyverse)
library(readxl)
library(rio)
library(dplyr)
library(tidyr)
library(ChainLadder)
library(lubridate)
library(readr)
library(zoo)
library(MASS)

data <- read_csv("C:/Users/Robin Ochieng/Kenbright/Automation and Risk Margin - General/Kenya/Occidental/Data/Data.csv",
                 col_types = cols(Date_Loss = col_date(format = "%m/%d/%Y"), 
                                  Gross_Paid = col_number(), 
                                  Date_Paid = col_date(format = "%m/%d/%Y")))

View(data)#Output this data Triangle on a Tab


data<- data %>% mutate(
  Paid_Year = year(Date_Paid),
  Loss_Year = year(Date_Loss),
  Paid_Quarter = quarter(Date_Paid),
  Loss_Quarter = quarter(Date_Loss),
  Acc_Quarters = paste(Loss_Year, Loss_Quarter, sep = "Q"),
  Dev_period = as.numeric(difftime(Date_Paid, Date_Loss, units = "days"))%/% 90+1)



# Define the Valiation_start and Valuation_end dates
Valuation_start_date <- mdy("1/1/2019")# integrate a calendar for date selection on my R Shiny application
Valuation_end_date <- mdy("12/31/2023")# integrate a calendar for date selection on my R Shiny application


# Calculate the number of quarters between start_date and end_date
Periods <- 20


Acc_period <- data %>% 
  filter(Loss_Year >= year(Valuation_start_date) & Loss_Year <= year(Valuation_end_date)) %>% 
  dplyr::select(Acc_Quarters)%>%
  distinct()%>%
  arrange(Acc_Quarters)%>%
  rowwise()%>%
  mutate(Dev_period = list(seq(1,Periods,1)))%>%
  unnest(Dev_period)#Include this to the app logic 


Val_class <- "Motor Commercial" #I need to have flexibility in picking all the distinct classes  options from this column in a widget

# Now perform the rest of the operations on the data
Val_Data <- data %>%
  filter(Statutory_Class %in%  Val_class) %>%
  dplyr::select(Acc_Quarters, Gross_Paid, Dev_period) %>%
  group_by(Acc_Quarters, Dev_period) %>%
  summarise(Gross_Amount = sum(Gross_Paid))%>%
  right_join(Acc_period)#Output this table on a Tab


Inc_Tri <- as.triangle(Val_Data,
                       origin = "Acc_Quarters",
                       dev = "Dev_period",
                       value = "Gross_Amount")#Output this Incremental Triangle on a Tab


Cum_Tri <- incr2cum(Inc_Tri)#Output this Cummulative Triangle on a Tab

Boot_Method <- BootChainLadder(Cum_Tri, R=9999, process.distr = "gamma")

summary_Boot_Method_Totals <- summary(Boot_Method)$Totals
summary_Boot_Method_Byorigin <- summary(Boot_Method)$ByOrigin
View(summary_Boot_Method_Totals)#Output this summary_Boot_Method_Totals Table on a tab named 'SUMMARIES'
View(summary_Boot_Method_Byorigin)#Output this summary_Boot_Method_Byorigin Table on a tab named 'SUMMARIES'

confidence_level <- quantile(Boot_method,c(0.6,0.65,0.7,0.75))
confidence_level_totals <- confidence_level$Totals
confidence_level_byorigin <- confidence_level$ByOrigin
view(confidence_level_byorigin)#Output this confidence_level_byorigin Table on a tab named 'SUMMARIES'
View(confidence_level_totals)#Output this confidence_level_totals Table on a tab named 'SUMMARIES'


plot <- plot(ecdf(B$IBNR.Totals))#Output this plot on a tab named 'SUMMARIES'
