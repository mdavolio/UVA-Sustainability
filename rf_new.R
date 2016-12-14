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

# Find best number of trees
range <- seq(from = 100, to = 3000, by = 150)

range %>% map(function(s){
  randomForest(MTeCO2 ~ nBuild + nPlants + sqft + session + Year + Month + AvgTemp + AvgHum + sumPrep,
               data=footprint, mtry=5, importance=TRUE, 
               na.action=na.exclude, ntree = s) %>% 
    get("mse",.) %>% 
    mean()
}) -> tuneTreeSize

plot(range,tuneTreeSize)

# Set ntrees to 2050

# 10-fold cross validation, repeated 10 times
cvControl <- trainControl(method = "repeatedCV",
                          repeats = 10,
                          number = 10)
# Run random forest 
rf.mod <- train(MTeCO2 ~ nBuild + nPlants + sqft + session + Year + Month + AvgTemp + AvgHum + sumPrep,
                  data = footprint,
                  method = 'rf',
                  ntree = 2050,
                  trControl = cvControl)
# best mtry was 9
# RMSE = 431.2622
# R^2 = 0.6538438
# Best Model MSE = 218978.1
# % Var explained in best model = 50.09%

# Create Time slices for ts cross validation
# Initial training window of 12 months, testing on next 6
# Fixed, moving window
timeControl <- trainControl(method = 'timeslice',
                            initialWindow = 12,
                            horizon = 6,
                            fixedWindow = T)

# cv by time series
rf.mod_ts <- train(MTeCO2 ~ nBuild +  nPlants + sqft + session + Year + Month + AvgTemp + AvgHum + sumPrep,
                data = footprint,
                method = 'rf',
                ntree = 2050,
                trControl = timeControl)

# Best mtry = 9
# RMSE = 398.2545
# R-Squared = 0.4056453
# Best model mse = 218038.1
# % Var explained in best model = 50.30%

### Linear Model

# LOO Cross Validation
cvControl <- trainControl(method = "LOOCV")

# Linear Model w/ LOO CV
# Year and Month removed as factors
lm.mod <- train(MTeCO2 ~ nBuild +  nPlants + sqft + session + AvgTemp + AvgHum + sumPrep,
                data = footprint,
                method = 'lm',
                trControl = cvControl)

# RMSE = 576.7353
# Only siginificant factor is AvgHum (p = 0.0160)
# Adj-R-Squared = 0.4255

# 

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