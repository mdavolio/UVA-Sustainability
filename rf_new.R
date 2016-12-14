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
                          repeats = 5,
                          number = 5)

# Run random forrest
rf.mod <- train(MTeCO2 ~ nBuild + total_cost + nPlants + sqft + session,
                  data = footprint,
                  method = 'rf',
                  trControl = cvControl)


