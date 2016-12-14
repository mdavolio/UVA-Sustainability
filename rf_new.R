###############################################
####### Random Forest Monthly Billing #########
###############################################

suppressPackageStartupMessages({
  library(caret)
  library(e1071)
  library(randomForest)
  library(leaps)
  library(car)
  library(earth)
})


### Random Forest

# 10-fold cross validation, repeated 10 times
cvControl <- trainControl(method = "repeatedCV",
                          repeats = 10,
                          number = 10)

# Run random forest
rf.mod <- train(MTeCO2 ~ nBuild + nPlants + sqft + session + Year + Month + AvgTemp + AvgHum + sumPrep,
                  data = footprint,
                  method = 'rf',
                  ntree = 1000,
                  trControl = cvControl)

# Time Series Random Forest

# Create Time slices
timeControl <- trainControl(method = 'timeslice',
                            initialWindow = 12,
                            horizon = 6,
                            fixedWindow = T)

# training model
rf.mod_ts <- train(MTeCO2 ~ nBuild +  nPlants + sqft + session + Year + Month + AvgTemp + AvgHum + sumPrep,
                data = footprint,
                method = 'rf',
                ntree = 1000,
                trControl = timeControl)


### Linear Model

# 10-fold cross validation, repeated 10 times
cvControl <- trainControl(method = "LOOCV")

# Run random forest
lm.mod <- train(MTeCO2 ~ nBuild +  nPlants + sqft + session + AvgTemp + AvgHum + sumPrep,
                data = footprint,
                method = 'lm',
                trControl = cvControl)

# Time Series Random Forest

# Create Time slices
timeControl <- trainControl(method = 'timeslice',
                            initialWindow = 12,
                            horizon = 6,
                            fixedWindow = T)

# training model
lm.mod_ts <- train(MTeCO2 ~ nBuild +  nPlants + sqft + session + AvgTemp + AvgHum + sumPrep,
                   data = footprint,
                   method = 'lm',
                   trControl = timeControl)


### Spline Model

# 10-fold cross validation, repeated 10 times
cvControl <- trainControl(method = "LOOCV")

# Run random forest
spline.mod <- train(MTeCO2 ~ nBuild +  nPlants + sqft + session + AvgTemp + AvgHum + sumPrep,
                data = footprint,
                method = 'earth',
                trControl = cvControl)

# Time Series Random Forest

# Create Time slices
timeControl <- trainControl(method = 'timeslice',
                            initialWindow = 12,
                            horizon = 6,
                            fixedWindow = T)

# training model
lm.mod_ts <- train(MTeCO2 ~ nBuild +  nPlants + sqft + session + AvgTemp + AvgHum + sumPrep,
                   data = footprint,
                   method = 'lm',
                   trControl = timeControl)