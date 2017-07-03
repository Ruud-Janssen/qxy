rm(list = ls())
source("D:/Betting/Tennis/hyperparametersfunctions.r")
source("D:/Betting/Tennis/hyperratingfunctions.r")
library(leaps)
library(bestglm)

#Parallel
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = (detectCores() - 3))

#remember what the power means, since transformation
FinalResultsLogLossTotal = matrix(0,20, 20)
FinalResultsLogLossClay =  matrix(0,20, 20)
FinalResultsLogLossHard =  matrix(0,20, 20)
FinalResultsLogLossGrass =  matrix(0,20, 20)

FinalBrTotal = matrix(0,20, 20)
FinalBrClay =  matrix(0,20, 20)
FinalBrHard =  matrix(0,20, 20)
FinalBrGrass =  matrix(0,20, 20)

Nt = nrow(read.table("D:/Betting/Tennis/Data/train_modelWithRatings.csv"
                       , header = T, sep = ",", quote = "\"", fill = TRUE))

cv_withRatings = read.table("D:/Betting/Tennis/Data/cvWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]

Ncv = nrow(cv_withRatings)

set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)
ycv = as.numeric(runif(Ncv, 0, 1) > 0.5)

total = foreach(o = 1 : 1, .combine = rbind) %do% {
  offset = 20 * o + 21275

  print(offset)
  return(foreach(c = 1:60, .packages = c("leaps","bestglm"), .combine = cbind) %dopar% {
  #return(foreach(p = 1:1, .packages = c("leaps","bestglm"), .combine = cbind) %dopar% {
    
    #power = 0.25 + p / 1500
    power = 1
    
    constant = 220 + c
   
    Both = GetRatings(offset, power, constant)
    
    train_modelwithRatings = Both[1 : Nt,]
    cv_withRatings = Both[(Nt + 1) : nrow(Both),]
    
    rm(Both)

    xt_m = regressorvariables(y = yt_m, Data = train_modelwithRatings)
    
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
      
      
      RegHard = glm(yt_mcurrentHard ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff + 
                      DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                    +FatigueDiff, data = xt_mcurrentHard, family = binomial)
      
      #Good model individually chosen:
      #RegGrass = glm(yt_mcurrentGrass ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
      #               + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
      #               , data = xt_mcurrentGrass, family = binomial)

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
    #FinalResultsLogLossTotal[o, p]  = mean(results$LogLossOutOfSample)
    #FinalResultsLogLossClay[o, p] =  mean(resultsSeperate$LogLossOutOfSampleClay)
    #FinalResultsLogLossHard[o, p] =  mean(resultsSeperate$LogLossOutOfSampleHard)
    #FinalResultsLogLossGrass[o, p] =  mean(resultsSeperate$LogLossOutOfSampleGrass)
    
    #FinalBrTotal[o, p] = mean(results$Br)
    #FinalBrClay[o, p] =  mean(resultsSeperate$BrClay)
    #FinalBrHard[o, p] =  mean(resultsSeperate$BrHard)
    #FinalBrGrass[o, p] =  mean(resultsSeperate$BrGrass)
   
  })
}

stopCluster(cl)
