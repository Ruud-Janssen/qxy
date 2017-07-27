rm(list = ls())
source("formulas.r")
source("hyperparametersfunctions.r")
library(leaps)
library(bestglm)
library(tictoc)

#Parallel
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = detectCores() - 3)

FinalResultsLogLossHard =  matrix(0,10, 50)
FinalBrHard =  matrix(0,10, 50)

train_modelwithRatings = read.table("Data/datasets/train_modelWithRatings.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)

Nt = nrow(train_modelwithRatings)

set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)

tic()

total = foreach(gb = 1 : 10) %do%{
  gamesBarier = 4 * (gb - 1)
  print(gamesBarier)
  return(foreach(days = 1 : 10, .combine = rbind) %do%{
    return(foreach(p = 1 : 10, .packages = c("leaps","bestglm"), .combine = cbind) %dopar% {
      
      source("hyperparametersfunctions.r")
      source("addRetiredandFatigueformulas.r")
  
      power = (3 * p + 60)/ 100

      train_modelwithRatings = read.table("Data/datasets/train_modelWithRatings.csv"
                                          , header = T, sep = ",", quote = "\"", fill = TRUE)
      train_modelwithRatings = CreateFatigueAndRecentGames(train_modelwithRatings, days, power, gamesBarier)
  
      xt_m = regressorvariables(yt_m, train_modelwithRatings)
      set.seed(42)
      train = sample(1:nrow(xt_m), nrow(xt_m)/2)
      validation = (-train)
      
      xTrain = xt_m[train, ]
      xValidation = xt_m[validation, ]
      
      resultsHard = as.data.frame(matrix(0, 20))
      resultsHard$LogLossOutOfSample= rep(0, 20)
      
      for(q in 1:20) {
        
        quantile = quantile(xTrain$Uncertainty, (q + 19) / 100)
        xTrainCurrent = removeUncertainMatches(xTrain, quantile)
        xValidationCurrent = removeUncertainMatches(xValidation, quantile)
  
        xTrainCurrentHard = getXThisSurface(xTrainCurrent, "Hard")
        xValidationCurrentHard = getXThisSurface(xValidationCurrent, "Hard")
  
        RegHard = glm(y ~ 0 + ratingdiff + ratingHarddiff +DummyBo5TimesAvgRatingdiff     
                      + RetiredDiff + FatigueDiff, data = xTrainCurrentHard, family = binomial)

        validationPredHard = predict(RegHard, xValidationCurrentHard, type = "response")
        resultsHard$LogLossOutOfSample[q] = LogLoss(validationPredHard, xValidationCurrentHard$y)
      }
      
      return(mean(resultsHard$LogLossOutOfSample))
    })
  })
}
toc()

stopCluster(cl)
write.csv(file = "FatigueParametersResults2.csv", total, row.names=FALSE)
