###############################################
####### Random Forest Monthly Billing #########
###############################################

suppressPackageStartupMessages({
  library(caret)
  library(ranger)
  library(e1071)
  library(randomForest)
})

# 5-fold cross validation, repeated 5 times
cvControl <- trainControl(method = "repeatedCV",
                          repeats = 10,
                          number = 10)

# Run random forest
lm.mod <- train(MTeCO2 ~ nBuild +  nPlants + sqft + session + AvgTemp + AvgHum + Year + Month,
                  data = footprint,
                  method = 'lm',
                  trControl = cvControl)

# Time Series Random Forest

# Create Time slices
timeControl <- trainControl(method = 'timeslice',
                            initialWindow = 3,
                            horizon = 3,
                            fixedWindow = T)

# training model
lm.mod_ts <- train(MTeCO2 ~ nBuild + nPlants + sqft + session + AvgTemp + AvgHum,
                data = footprint,
                method = 'lm',
                trControl = timeControl)