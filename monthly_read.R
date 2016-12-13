#################################
######## monthly_read.R ########
################################

# Data Mining SYS 6018 Final Project
# Code to read in and clean/wrangle the monthly billing data into a useable format
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(purrr)
  library(data.table)
  library(lubridate)
})

# No scientific notation
options(scipen=999)

# Read in monthly billing data and perform data type corrections, mutations, and selections
monthly.bills <- read_excel('Building Billing History - West District.xlsx', col_names = TRUE) %>% 
  select(c(2:10,14)) %>% 
  mutate(BillDate = as.Date(format(BillMonth,"%Y-%m-%d"))) %>% 
  mutate(Month = factor(month(BillDate, label = FALSE))) %>% 
  mutate(Year = factor(year(BillDate))) %>% 
  select(-c(1)) %>% 
  mutate(Building = factor(Building)) %>% 
  mutate(MeterID = factor(MeterID)) %>% 
  mutate(PlantID = factor(PlantID)) %>% 
  mutate(Product = factor(Product)) %>% 
  mutate(Units = factor(Units)) %>% 
  select(-c(10)) %>% 
  select(c(11,10,1:9))

# Read in building info
buildingInfo <- function(path){
  df <- read_excel(path) %>% 
    select(c(1,2,6,7,8,9))
  names(df) <- c('Building','buildingName','square_foot','YearBuilt','ConstructionType','Category')
  return(df) 
}

buildings <- buildingInfo('Basic Building Info.xlsx') %>% 
  mutate(Building = factor(Building))

# Perform a group_by an summarise to get monthly summary info
monthly.bills %>% 
  group_by(Year, Month) %>% 
  summarise(MTeCO2 = sum(MTeCO2), nPlants = n_distinct(PlantID), nBuild = n_distinct(Building), total_cost = sum(Cost)/100000) %>% 
  select(1,2,4,5,3,6) -> footprint

# Exploratory Plots
lines(footprint$MTeCO2, main = "Monthly Carbon Expense", xlab = "Month", ylab="MTeC02 Demand")
lines(footprint$total_cost, main = "Monthly Expenditures", xlab = "Month", ylab="Cost (per Hundred Thousand $$)")
plot(footprint$total_cost, footprint$MTeCO2, main = "Cost Vs Carbon", xlab="Cost (per Hundred Thousand $$)", ylab = "MTeC02 Demand")


