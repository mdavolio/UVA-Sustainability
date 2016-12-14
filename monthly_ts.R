###################################
#### Time Series Investigation ####
###################################

# Data Mining SYS 6018 Final Project
# Code to read in conduct time series analysis
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(forecast)
})

# No scientific notation
options(scipen=999)

# Load monthly footprint data
load("monthly_read.RData")

# Convert data to time series object for further analysis, with a period of 12 months
footprint.ts = ts(data = footprint$MTeCO2, start = c(2013, 7), end = c(2016, 6), frequency = 12)

# Decompose into monthly subseries to inspect trends.
monthplot(footprint.ts)

# How do various time periods correlate? Plot autocorrelation to find out.
acf(footprint.ts)

# Decompose time series into seasonal, trend, and random components.
decomposition = decompose(footprint.ts)
plot(decomposition)

# The random component of the decomposition shouldn't have any structure to it. Does it?
acf(na.omit(decomposition$random))

# Fit exponential smoothing model to data.
holt.winters.fit = HoltWinters(footprint.ts, beta = FALSE, gamma = FALSE)
plot(holt.winters.fit)
holt.winters.fit$SSE

# An even better model:  Auto-regressive, integrated, moving average, with stepwise selection
covariates <- footprint[,c(3,5,6,7,10,11,12)]
auto.arima.fit = auto.arima(footprint.ts, d=NA, D=NA, max.p=5, max.q=5,
                            max.P=3, max.Q=3, max.order=10, max.d=3, max.D=3, 
                            start.p=1, start.q=1, start.P=1, start.Q=1, 
                            stationary=FALSE, seasonal=TRUE,
                            ic="aicc", stepwise=TRUE, trace=TRUE,
                            approximation=TRUE, 
                            truncate=NULL, xreg=covariates,
                            test="kpss", seasonal.test="ocsb",
                            allowdrift=TRUE, allowmean=TRUE, lambda=NULL)

mean(auto.arima.fit$residuals^2) # MSE = 48554.28

