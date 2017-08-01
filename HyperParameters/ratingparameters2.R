rm(list = ls())
source("hyperparametersfunctions.r")
source("HyperParameters/hyperratingfunctions2.r")
library(leaps)
library(bestglm)
library(tictoc)

#Parallel
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = (detectCores() - 3))

FinalResultsLogLossHard =  matrix(0,20, 20)

Nt = nrow(read.table("Data/datasets/train_modelWithRatings.csv"
                     , header = T, sep = ",", quote = "\"", fill = TRUE))
set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)

tic()
total = foreach (k = 1 : 22, .combine = rbind) %do% {
  constant = 14 + 2 * k
  offset = 0.1
  #return(foreach(p = 1 : 1, .combine = rbind) %do%{
  #  power = 0.2
  return(foreach(p = 1 : 20, .packages = c("leaps","bestglm", "plyr"), .combine = cbind) %dopar% {
    source("hyperparametersfunctions.r")
    source("HyperParameters/hyperratingfunctions2.r")
    source("formulas.R")
    power = 0.06 + 0.02 * p
    
    train_modelwithRatings = GetRatings(offset, power, constant)
    
    xt_m = regressorvariables(yt_m, train_modelwithRatings)
    lastGame2011 = 19319
    
    
    xTrain = xt_m[1:lastGame2011, ]
    xValidation = xt_m[(lastGame2011 + 1) : nrow(xt_m), ]
    
    results = as.data.frame(matrix(0, 20))
    results$LogLossOutOfSampleHard = rep(0, 20)
    
    
    for(q in 1 : 20) {
      
      quantile = quantile(xTrain$Uncertainty, (q + 19) / 100)
      
      xTraincurrent = removeUncertainMatches(xTrain, quantile, "")
      xTraincurrentHard = getXThisSurface(xTraincurrent, "Hard")
      
      xValidationCurrent = removeUncertainMatches(xValidation, quantile, "")
      xValidationCurrentHard = getXThisSurface(xValidationCurrent, "Hard")
      
      RegHard = glm(y ~ 0 + ratingdiff + ratingHarddiff, data = xTraincurrentHard, family = binomial)
      
      validationPredHard = predict(RegHard, xValidationCurrentHard, type = "response")
      results$LogLossOutOfSampleHard[q] = LogLoss(actual = xValidationCurrentHard$y, predicted = validationPredHard)    
    }
    return(mean(results$LogLossOutOfSampleHard))
  })
  #})
}
stopCluster(cl)
toc()
#write.csv(file = "RatingParametersResult20.csv", total, row.names=FALSE)
