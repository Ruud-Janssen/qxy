rm(list = ls())
source("hyperparametersfunctions.r")
source("ugly analysis rating different k\\hyperratingdiffKfunctions.r")
library(leaps)
library(bestglm)

#Parallel
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = (detectCores() - 3))

#remember what the power means, since transformation
FinalResultsLogLossTotal = matrix(0, 20)
FinalResultsLogLossClay =  matrix(0, 20)
FinalResultsLogLossHard =  matrix(0, 20)
FinalResultsLogLossGrass =  matrix(0, 20)

FinalBrTotal = matrix(0, 20)
FinalBrClay =  matrix(0, 20)
FinalBrHard =  matrix(0, 20)
FinalBrGrass =  matrix(0, 20)



train_model = read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"", 
                         stringsAsFactors = FALSE, fill = TRUE)
cv = read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                stringsAsFactors = FALSE, fill = TRUE)


train_model = RemoveWalkOvers(train_model)
cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]
cv = RemoveWalkOvers(cv)

Nt = nrow(train_model)
Ncv = nrow(cv)

set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)
#ycv = as.numeric(runif(Ncv, 0, 1) > 0.5)
ycv = as.numeric(runif(5189, 0, 1) > 0.5)

total = foreach(k = 1 : 11, .combine = rbind) %dopar% {
    diffK = 0.5 * (k - 6)

    Both = GetRatings(diffK)
    
    train_modelwithRatings = Both[1 : Nt,]
    cv_withRatings = Both[(Nt + 1) : nrow(Both),]
    
    rm(Both)
    
    xt_m = regressorvariables(y = yt_m, Data = train_modelwithRatings)
    
    
    
    #apparantly there is one NA in BestOF, temporarily removal needs to be data cleansed
    indexnomissingBestOf = !is.na(cv_withRatings$Best.of)
    
    cv_withRatings = cv_withRatings[indexnomissingBestOf, ]
    
    
    
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
    
}

stopCluster(cl)
