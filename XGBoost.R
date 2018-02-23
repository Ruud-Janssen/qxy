rm(list = ls())
library(tidyverse)
library(xgboost)
library(caret)
library(dummies)
source("functions.r")

train <- read_csv("input/train0.csv")
test  <- read_csv("input/test0.csv")

train <- ConvertFactors(train)
test  <- ConvertFactors(test)

trainDummies <- dummy.data.frame(data.frame(train),sep = ".") %>% as_tibble()
testDummies  <- dummy.data.frame(data.frame(test),sep = ".")  %>% as_tibble()

missingColumns <- setdiff(names(trainDummies), names(testDummies))
testDummies[, missingColumns] = 0

ctrlXGBTree <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
gridXGBTree <- expand.grid(eta              = seq(0.007, 0.011, 0.002), 
                           nrounds          = 2 * seq(1000, 2000, 500),
                           gamma            = 0,
                           colsample_bytree = 0.33,
                           max_depth        = 6 : 8,
                           min_child_weight = 0, 
                           subsample        = 1)

mdlXGBTree <- train(SalePriceLog ~., data = trainDummies, 
                              method = "xgbTree", family = "gaussian", 
                              trControl = ctrlXGBTree, tuneGrid = gridXGBTree, 
                              verbose = 1, print_every_n = 1)

importanceVars <- xgb.importance(feature_names = mdlXGBTree$finalModel$xNames, model = mdlXGBTree$finalModel)
importanceVars <- importanceVars %>% arrange(desc(Importance))

xgb.plot.importance(importanceVars)

trainDummies2 <- trainDummies %>% select(names(trainDummies)[importanceVars$Feature[1 : 300] %in% names(trainDummies)], 
                                         SalePriceLog)

mdlXGBTree2 <- train(SalePriceLog ~., data = trainDummies2, 
                    method = "xgbTree", family = "gaussian", 
                    trControl = ctrlXGBTree, tuneGrid = gridXGBTree, 
                    verbose = 1, print_every_n = 1)

importanceVars2 <- xgb.importance(feature_names = mdlXGBTree$finalModel$xNames, model = mdlXGBTree$finalModel)
importanceVars2 <- importanceVars %>% arrange(desc(Importance))

