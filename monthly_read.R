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