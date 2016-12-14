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
  library(zoo)
  library(stringr)
})

# No scientific notation
options(scipen=999)

# Read in building info
buildingInfo <- function(path){
  df <- read_excel(path) %>% 
    select(c(1,2,6,7,8,9))
  names(df) <- c('Building','buildingName','square_foot','YearBuilt','ConstructionType','Category')
  return(df) 
}

buildings <- buildingInfo('Basic Building Info.xlsx') %>% 
  mutate(Building = factor(Building)) %>% 
  mutate(Category = factor(Category)) %>% 
  mutate(ConstructionType = factor(ConstructionType))

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
  mutate(year_mon = as.yearmon(paste(str_sub(as.character(Year), start= -2), str_sub(as.character(BillDate),-5,-4), sep = ''), "%y%m")) %>% 
  merge(buildings, by = 'Building', all.x = TRUE)

# Perform a group_by an summarise to get monthly summary info
monthly.bills %>% 
  group_by(BillDate) %>% 
  summarise(MTeCO2 = sum(MTeCO2), nPlants = n_distinct(PlantID), 
            nBuild = n_distinct(Building), total_cost = sum(Cost)/100000,
            sqft = sum(square_foot, na.rm = TRUE)) %>% 
  select(1,2,4,5,3,6) -> footprint


# Percentage of month in which class is in session
session <- as.data.frame(matrix(0, nrow = 36))
session$V1 <- c(0,(4/31),1,1,1,(14/31),(19/31),1,(24/31),1,(9/31),0,0,(6/31),1,1,(16/31),
              (20/31),1,(24/31),1,(8/31),0,0,(7/31),1,1,1,(18/31),(11/31),1,(24/31),
              1,(13/31),0,0)

footprint <- cbind(footprint, session)
colnames(footprint)[7] <- "session"

# Add in weather data
weather <- read.csv("Avg_Weather.csv", header = TRUE) %>% 
  select(-c(1))

footprint <- cbind(footprint,weather)

# Exploratory Plots
plot(footprint$year_mon, footprint$MTeCO2, main = "Monthly Carbon Expense", xlab = "Month", ylab="MTeC02 Demand")
plot(footprint$year_mon, footprint$total_cost, main = "Monthly Expenditures", xlab = "Month", ylab="Cost (per Hundred Thousand $$)")
plot(footprint$total_cost, footprint$MTeCO2, main = "Cost Vs Carbon", xlab="Cost (per Hundred Thousand $$)", ylab = "MTeC02 Demand")

# We need to get weather information and include in our footprint data frame
# Must conduct research to determine what weather info to include

# Save out RData file
rm(list=setdiff(ls(), c("footprint", "monthly.bills")))
save.image("monthly_read.RData")
