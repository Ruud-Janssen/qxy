rm(list = ls())
source("D:/Betting/Tennis/hyperparametersfunctions.r")
source("D:/Betting/Tennis/fatigueparametersfunctions.r")
library(leaps)
library(bestglm)

#Parallel
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = detectCores() - 3)


FinalResultsLogLossTotal = matrix(0,10, 50)
FinalResultsLogLossClay =  matrix(0,10, 50)
FinalResultsLogLossHard =  matrix(0,10, 50)
FinalResultsLogLossGrass =  matrix(0,10, 50)

FinalBrTotal = matrix(0,10, 50)
FinalBrClay =  matrix(0,10, 50)
FinalBrHard =  matrix(0,10, 50)
FinalBrGrass =  matrix(0,10, 50)

train_modelwithRatings = read.table("Data/datasets/train_modelWithRatings.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings = read.table("Data/datasets/cvWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]

Nt = nrow(train_modelwithRatings)
Ncv =nrow(cv_withRatings)

set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)
ycv = as.numeric(runif(Ncv, 0, 1) > 0.5)

total = foreach(days = 1 : 10, .combine = rbind) %do%{
  print(days)

  return(foreach(p = 1 : 15, .packages = c("leaps","bestglm"), .combine = cbind) %dopar% {
    
    source("D:/Betting/Tennis/hyperparametersfunctions.r")
    source("D:/Betting/Tennis/fatigueparametersfunctions.r")

    power = (p + 90)/ 100
    
    train_modelwithRatings = read.table("Data/datasets/train_modelWithRatings.csv"
                                        , header = T, sep = ",", quote = "\"", fill = TRUE)
    train_modelwithRatings = CreateFatigue(data = train_modelwithRatings, days = days, power = power)
    
    cv_withRatings = read.table("Data/datasets/cvWithRatings.csv"
                                , header = T, sep = ",", quote = "\"", fill = TRUE)
    
    cv_withRatings = CreateFatigue(data = cv_withRatings, days = days, power = power)
    

    xt_m = regressorvariables(yt_m, train_modelwithRatings)
    
    #apparantly there is one NA in BestOF, temporarily removal needs to be data cleansed
    cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]
    

    xcv = regressorvariables(ycv, cv_withRatings)
    
    results = as.data.frame(matrix(0, 20))
    #results$LogLossInSample = rep(0,20)
    #results$LogLossOutOfSample = rep(0,20)
    results$LogLossOutOfSampleHard = rep(0, 20)
    #results$LogLossOutOfSampleClay = rep(0, 20)
    #results$LogLossOutOfSampleGrass = rep(0, 20)
    
    results$PercentageRemovedt_m = rep(0,20)
    results$PercentageRemovedcv = rep(0,20)
    #results$ROI = rep(NA, 20)
    #results$Nrbets = rep(0, 20)
    #results$Br = rep(1, 20)
    #results$BrClay = rep(1, 20)
    #results$BrGrass = rep(1, 20)
    results$BrHard = rep(1, 20)
    
    resultsSeperate = results
    
    for(q in 1:20) {
      
      quantile = quantile(xt_m$Uncertainty, (q + 19) / 100)
      
      index_xt_m = (xt_m$Uncertainty < quantile)
      xt_mcurrent = xt_m[index_xt_m, ]
      yt_mcurrent = yt_m[index_xt_m]
      results$PercentageRemovedt_m[q] = 1 - length(yt_mcurrent) / length(yt_m)
      resultsSeperate$PercentageRemovedt_m[q] = results$PercentageRemovedt_m[q]
      
      #indexGrasst_m = (xt_mcurrent$Surface == "Grass")
      indexHardt_m = (xt_mcurrent$Surface == "Hard")
      #indexClayt_m = (xt_mcurrent$Surface == "Clay")
      
      #xt_mcurrentGrass = xt_mcurrent[indexGrasst_m, ]
      xt_mcurrentHard = xt_mcurrent[indexHardt_m, ]
      #xt_mcurrentClay = xt_mcurrent[indexClayt_m, ]
      
      #yt_mcurrentGrass = yt_mcurrent[indexGrasst_m]
      yt_mcurrentHard = yt_mcurrent[indexHardt_m]
      #yt_mcurrentClay = yt_mcurrent[indexClayt_m]
      
      index_xcv = (xcv$Uncertainty < quantile)
      xcvcurrent = xcv[index_xcv, ]
      ycvcurrent = ycv[index_xcv]
      
      #indexGrasscv = (xcvcurrent$Surface == "Grass")
      indexHardcv = (xcvcurrent$Surface == "Hard")
      #indexClaycv = (xcvcurrent$Surface == "Clay")
      
      #xcvcurrentGrass = xcvcurrent[indexGrasscv, ]
      xcvcurrentHard = xcvcurrent[indexHardcv, ]
      #xcvcurrentClay = xcvcurrent[indexClaycv, ]
      
      #ycvcurrentGrass = ycvcurrent[indexGrasscv]
      ycvcurrentHard = ycvcurrent[indexHardcv]
      #ycvcurrentClay = ycvcurrent[indexClaycv]
      
      #results$PercentageRemovedcv[q] = 1 - length(ycvcurrent) / length(ycv)
      
      #Reg = glm(yt_mcurrent ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
      #          +FatigueDiff + HeadtoHead, data = xt_mcurrent, family = binomial)
      
      #Good model individually chosen:
      #RegGrass = glm(yt_mcurrentGrass ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
      #               + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
      #               , data = xt_mcurrentGrass, family = binomial)
      RegHard = glm(yt_mcurrentHard ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff + 
                      DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                    +FatigueDiff, data = xt_mcurrentHard, family = binomial)
      
      #RegClay = glm(yt_mcurrentClay ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff +
      #                DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
      #              +FatigueDiff, data = xt_mcurrentClay, family = binomial)
      
      
      #print(q)
      #print(summary(Reg))
      #LogLoss(Reg$fitted.values, yt_mcurrent)
      
      #results$LogLossInSample[q] = LogLoss(Reg$fitted.values, yt_mcurrent)
      
      #results = cvpredictions(results, Reg, xcvcurrent, ycvcurrent, q)
      #resultsSeperate = cvpredictions(resultsSeperate, RegGrass, xcvcurrentGrass, ycvcurrentGrass, q)
      resultsSeperate = cvpredictions(resultsSeperate, RegHard, xcvcurrentHard, ycvcurrentHard, q)
      #resultsSeperate = cvpredictions(resultsSeperate, RegClay, xcvcurrentClay, ycvcurrentClay, q)
    }
    
    return(mean(resultsSeperate$LogLossOutOfSampleHard))
    
    #FinalResultsLogLossTotal[days, p]  = mean(results$LogLossOutOfSample)
    #FinalResultsLogLossClay[days, p] =  mean(resultsSeperate$LogLossOutOfSampleClay)
    #FinalResultsLogLossHard[days, p] =  mean(resultsSeperate$LogLossOutOfSampleHard)
    #FinalResultsLogLossGrass[days, p] =  mean(resultsSeperate$LogLossOutOfSampleGrass)
    
    #FinalBrTotal[days, p] = mean(results$Br)
    #FinalBrClay[days, p] =  mean(resultsSeperate$BrClay)
    #FinalBrHard[days, p] =  mean(resultsSeperate$BrHard)
    #FinalBrGrass[days, p] =  mean(resultsSeperate$BrGrass)
    
  }
  )
}

stopCluster(cl)
