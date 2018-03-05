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

Nt = nrow(read.table("Data/datasets/train_modelWithRatings.csv"
                     , header = T, sep = ",", quote = "\"", fill = TRUE))
set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)

tic()
total = foreach (p = 1 : 1, .combine = rbind) %do% {
  power <- 0.05 * p
  offset = 0.1
  #return(foreach(p = 1 : 1, .combine = rbind) %do%{
  #  power = 0.2
  return(foreach(k = 1 : 21, .packages = c("leaps","bestglm", "plyr"), .combine = cbind) %dopar% {
    source("hyperparametersfunctions.r")
    source("HyperParameters/hyperratingfunctions2.r")
    constant = 14 + 0.2 * (k - 1)

    #constant = 16.8
    #winBonus <- 0.01 * (p - 1)
    
    train_modelwithRatings = GetRatings(offset, power, constant)
    # train_modelwithRatings$Winner_retired_last_game <- train_modelwithRatings$Winner_ratingNewHard
    # train_modelwithRatings$Loser_retired_last_game  <- train_modelwithRatings$Loser_ratingNewHard
     
    xt_m = regressorvariables(yt_m, train_modelwithRatings)
    
    df <- "%Y-%m-%d"
    lastGame2011 <- as.Date("2011-12-31", df)
    
    xTrain = xt_m[as.Date(xt_m$Date, df) <= lastGame2011, ]
    xValidation = xt_m[as.Date(xt_m$Date, df) > lastGame2011, ]
    
    
    results = as.data.frame(matrix(0, 20))
    results$LogLossOutOfSampleHard = rep(0, 20)
    
    
    for(q in 1 : 20) {
      
      quantile = quantile(xTrain$Uncertainty, (q + 19) / 100)
      
      xTraincurrent = removeUncertainMatches(xTrain, quantile, "")
      xTraincurrentHard = getXThisSurface(xTraincurrent, "Hard")
      
      xValidationCurrent = removeUncertainMatches(xValidation, quantile, "")
      xValidationCurrentHard = getXThisSurface(xValidationCurrent, "Hard")
      
      RegHard = glm(y ~ 0 + ratingdiff + ratingHarddiff, data = xTraincurrentHard, family = binomial)
      
      #RegHard = glm(y ~ 0 + ratingHarddiff + RetiredDiff, data = xTraincurrentHard, family = binomial)
      
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
