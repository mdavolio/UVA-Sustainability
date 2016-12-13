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


