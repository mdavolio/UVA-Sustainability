#################################################
####### Random Forest Energy Prediction #########
#################################################

suppressPackageStartupMessages({
  library(randomForest)
  library(dplyr)
  library(purrr)
})

## Load in the R environment containing the full data, subsetted into training and testing sets
load("sustain_read.RData")
set.seed(12345)

final.train$e_per_sqft <- final.train$total_energy/final.train$square_foot
final.test$e_per_sqft <- final.test$total_energy/final.test$square_foot
# create subsets to tune parameters of the rf
final.train.sub <- final.train[sample.int(nrow(final.train),size = 2000),]
final.test.sub <- final.test[sample.int(nrow(final.test), size = 800),]


## Build the forest over a range of tree sizes
range <- seq(from = 100, to = 3000, by = 150)

range %>% map(function(s){
  randomForest(e_per_sqft~age+Hour+semester+ConstructionType+Category+INTERPOLATIVE_T+INTERPOLATIVE_H+MeanDew.PointF+
                           Mean.Sea.Level.PressureIn+Mean.VisibilityMiles+Mean.Wind.SpeedMPH+
                           CloudCover+WindDirDegrees+Rain+Snow,data=final.train.sub, mtry=4, importance=TRUE, 
                           na.action=na.exclude, ntree = s) %>% 
    get("mse",.) %>% 
    mean()
}) -> tuneTreeSize

plot(range,tuneTreeSize) # determine the appropriate tree size
tSize <- # best from above
  
## Build forest over a range of mtry (split variables) with ntree = tSize
mtry.range <- seq(from = 3, to = 14, by = 1)
mtry.range %>% map(function(s){
  randomForest(e_per_sqft~age+Hour+semester+ConstructionType+Category+INTERPOLATIVE_T+INTERPOLATIVE_H+MeanDew.PointF+
                 Mean.Sea.Level.PressureIn+Mean.VisibilityMiles+Mean.Wind.SpeedMPH+
                 CloudCover+WindDirDegrees+Rain+Snow,data=final.train.sub, mtry=s, importance=TRUE, 
               na.action=na.exclude, ntree = tSize) %>% 
    get("mse",.) %>% 
    mean()
}) -> tuneTreeSplitPred

plot(mtry.range,tuneTreeSplitPred)
tMtry <- # best from above
  
## Now combine to a full random forest run with a larger training set/testing set on tuned parameters

## Old attempt
energy.rf=randomForest(e_per_sqft~semester+ConstructionType+Category+INTERPOLATIVE_T+INTERPOLATIVE_H+MeanDew.PointF+
                         Mean.Sea.Level.PressureIn+Mean.VisibilityMiles+Mean.Wind.SpeedMPH+
                         CloudCover+WindDirDegrees+Rain+Snow,data=final.train.sub, mtry=3, importance=TRUE, 
                       na.action=na.exclude, ntree = s) #mtry controls the number of predictors to use in each split, using sqrt(p)
energy.rf


## lets check the test set performance
yhat = predict(energy.rf,newdata=final.test.sub)
plot(yhat,final.test.sub$e_per_sqft)
abline(0,1)
mean((yhat-final.test.sub$e_per_sqft)^2, na.rm = TRUE) # MSE = 8417607, very high

# look at the importance of each variable
importance(energy.rf)
#                          %IncMSE IncNodePurity
#square_foot               52.281166  444844253020
#semester                  15.735152   19320825493
#ConstructionType          22.625195  106795656421
#Category                  21.663634  194427721142
#INTERPOLATIVE_T           14.202057   83939915151
#INTERPOLATIVE_H            9.148559   59760499972
#MeanDew.PointF            11.823101   68038609402
#Mean.Sea.Level.PressureIn 18.056698   54782281657
#Mean.VisibilityMiles      39.470027   11395267802
#Mean.Wind.SpeedMPH        15.710784   32188374546
#CloudCover                18.607376   29447226273
#WindDirDegrees            18.225885   82295388149
#Rain                      20.778251    5064665827
#Snow                      14.113672    1296142792

# TWO measures of variable importance:
# %IncMSE = based on the mean decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model
# IncNodePurity = measure of total decrease in node purity that results from splits over that variable, avgeraged over all trees.
varImpPlot(energy.rf)

save.image("sustain_rf.RData")
