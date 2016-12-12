####################################
####### Time Series Modles #########
####################################

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
})

# view subset of building 0201
build_0201 <- subset(final, buildingID == '0201')

# convert to time series object
final.ts <- ts(data = build_0201, start = ())