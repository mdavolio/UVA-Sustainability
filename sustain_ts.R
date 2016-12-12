####################################
####### Time Series Modles #########
####################################

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(forecast)
  library(xts)
})

# view subset of building 0201
build_0201 <- subset(final, buildingID == '0201')

time_index <- seq(from = as.POSIXct("2012-05-15 07:00"), 
                  to = as.POSIXct("2012-05-17 18:00"), by = "hour")
set.seed(1)
value <- rnorm(n = length(time_index))

eventdata <- xts(value, order.by = time_index)
ets(eventdata)