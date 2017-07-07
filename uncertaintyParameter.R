rm(list = ls())
source("formulas.r")
source("hyperparametersfunctions.r")
library(leaps)
library(bestglm)

train_modelwithRatings = read.table("D:/Betting/Tennis/Data/train_modelWithRatings.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings = read.table("D:/Betting/Tennis/Data/cvWithRatings.csv"
                                      , header = T, sep = ",", quote = "\"", fill = TRUE)

#train_modelwithRatings = train_modelwithRatings[(train_modelwithRatings$Surface == "Grass" | 
#                                                   train_modelwithRatings$Surface == "Hard" ), ]
#cv_withRatings = cv_withRatings[(cv_withRatings$Surface == "Grass"| 
#                                  cv_withRatings$Surface == "Hard" ), ]


Nt = nrow(train_modelwithRatings)
cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]
Ncv =nrow(cv_withRatings)

set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)
ycv = as.numeric(runif(Ncv, 0, 1) > 0.5)


xt_m = regressorvariables(yt_m, train_modelwithRatings)

#apparantly there is one NA in BestOF, temporarily removal needs to be data cleansed

xcv = regressorvariables(ycv, cv_withRatings)

results = as.data.frame(matrix(0, 99))
results$LogLossInSample = rep(0,99)
results$LogLossOutOfSample = rep(0,99)
results$LogLossOutOfSampleHard = rep(0, 99)
results$LogLossOutOfSampleClay = rep(0, 99)
results$LogLossOutOfSampleGrass = rep(0, 99)

results$PercentageRemovedt_m = rep(0,99)
results$PercentageRemovedcv = rep(0,99)
results$ROI = rep(NA, 99)
results$ROIGrass = rep(NA, 99)
results$ROIHard = rep(NA, 99)
results$ROIClay = rep(NA, 99)
results$Nrbets = rep(0, 99)
results$Br = rep(1, 99)

resultsSeperate = results


for(q in 1:99) {
  quantile = quantile(xt_m$Uncertainty, q / 100)
  
  index_xt_m = (xt_m$Uncertainty < quantile)
  xt_mcurrent = xt_m[index_xt_m, ]
  yt_mcurrent = yt_m[index_xt_m]
  results$PercentageRemovedt_m[q] = 1 - length(yt_mcurrent) / length(yt_m)
  resultsSeperate$PercentageRemovedt_m[q] = results$PercentageRemovedt_m[q]
  
  indexGrasst_m = (xt_mcurrent$Surface == "Grass")
  indexHardt_m = (xt_mcurrent$Surface == "Hard")
  indexClayt_m = (xt_mcurrent$Surface == "Clay")
  
  xt_mcurrentGrass = xt_mcurrent[indexGrasst_m, ]
  xt_mcurrentHard = xt_mcurrent[indexHardt_m, ]
  xt_mcurrentClay = xt_mcurrent[indexClayt_m, ]
  
  yt_mcurrentGrass = yt_mcurrent[indexGrasst_m]
  yt_mcurrentHard = yt_mcurrent[indexHardt_m]
  yt_mcurrentClay = yt_mcurrent[indexClayt_m]
  
  index_xcv = (xcv$Uncertainty < quantile)
  xcvcurrent = xcv[index_xcv, ]
  ycvcurrent = ycv[index_xcv]
  
  indexGrasscv = (xcvcurrent$Surface == "Grass")
  indexHardcv = (xcvcurrent$Surface == "Hard")
  indexClaycv = (xcvcurrent$Surface == "Clay")
  
  xcvcurrentGrass = xcvcurrent[indexGrasscv, ]
  xcvcurrentHard = xcvcurrent[indexHardcv, ]
  xcvcurrentClay = xcvcurrent[indexClaycv, ]
  
  ycvcurrentGrass = ycvcurrent[indexGrasscv]
  ycvcurrentHard = ycvcurrent[indexHardcv]
  ycvcurrentClay = ycvcurrent[indexClaycv]
  
  results$PercentageRemovedcv[q] = 1 - length(ycvcurrent) / length(ycv)
  
  Reg = glm(yt_mcurrent ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
            +FatigueDiff + HeadtoHead, data = xt_mcurrent, family = binomial)
  
  #RegGrass = glm(yt_mcurrentGrass ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
  #               +FatigueDiff + HeadtoHead, data = xt_mcurrentGrass, family = binomial)
  #RegHard = glm(yt_mcurrentHard ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
  #              +FatigueDiff + HeadtoHead, data = xt_mcurrentHard, family = binomial)
  #RegClay = glm(yt_mcurrentClay ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
  #              +FatigueDiff + HeadtoHead, data = xt_mcurrentClay, family = binomial)
  
  #Good model individually chosen:
  RegGrass = glm(yt_mcurrentGrass ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
                 + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                 , data = xt_mcurrentGrass, family = binomial)
  RegHard = glm(yt_mcurrentHard ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff + 
                  DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                +FatigueDiff, data = xt_mcurrentHard, family = binomial)
  
  RegClay = glm(yt_mcurrentClay ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff +
                  DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                +FatigueDiff, data = xt_mcurrentClay, family = binomial)
  
  
  #print(q)
  #print(summary(Reg))
  #LogLoss(Reg$fitted.values, yt_mcurrent)
  
  results$LogLossInSample[q] = LogLoss(Reg$fitted.values, yt_mcurrent)
  
  results = cvpredictions(results, Reg, xcvcurrent, ycvcurrent, q)
  resultsSeperate = cvpredictions(resultsSeperate, RegGrass, xcvcurrentGrass, ycvcurrentGrass, q)
  resultsSeperate$ROIGrass[q] = resultsSeperate$ROI[q]
  resultsSeperate = cvpredictions(resultsSeperate, RegHard, xcvcurrentHard, ycvcurrentHard, q)
  resultsSeperate$ROIHard[q] = resultsSeperate$ROI[q]
  resultsSeperate = cvpredictions(resultsSeperate, RegClay, xcvcurrentClay, ycvcurrentClay, q)
  resultsSeperate$ROIClay[q] = resultsSeperate$ROI[q]
}

plot(1 : 99, results$LogLossOutOfSample, "l", col = "red", 
     ylim = c(0.50, 0.65), xlab = 'Quantile', ylab = 'logloss out sample')

title( main = 'loglosses out of sample')
points(1: 99, results$LogLossOutOfSampleClay, "l", lty = 2, col = "blue")
points(1: 99, results$LogLossOutOfSampleHard, "l", lty = 3, col = "black")
points(1: 99, results$LogLossOutOfSampleGrass, "l", lty = 4, col = "green")
legend("topright",col= c("red", "blue",  "black", "green"),lty=1:4,lwd=1,
       legend=c("all", "clay", "hard", "grass"), bty="n")

resultsSeperate$LogLossOutOfSample = (resultsSeperate$LogLossOutOfSampleHard + resultsSeperate$LogLossOutOfSampleClay
                                      +resultsSeperate$LogLossOutOfSampleGrass) / 3

plot(1 : 99, resultsSeperate$LogLossOutOfSample, "l", col = "red", 
     ylim = c(0.50, 0.65), xlab = 'Quantile', ylab = 'logloss out sample')

title( main = 'loglosses out of sample')
points(1: 99, resultsSeperate$LogLossOutOfSampleClay, "l", lty = 2, col = "blue")
points(1: 99, resultsSeperate$LogLossOutOfSampleHard, "l", lty = 3, col = "black")
points(1: 99, resultsSeperate$LogLossOutOfSampleGrass, "l", lty = 4, col = "green")
legend("topright",col= c("red", "blue",  "black", "green"),lty=1:4,lwd=1,
       legend=c("all", "clay", "hard", "grass"), bty="n")


resultsSeperate$Br = resultsSeperate$BrClay + resultsSeperate$BrGrass + resultsSeperate$BrHard

plot(1: 99, resultsSeperate$Br, "l", col = "red", xlab = "Quantile", ylab = "Bankroll", ylim = c(-25,25))
points(1 : 99, resultsSeperate$BrHard, "l", col = "blue")
points(1 : 99, resultsSeperate$BrGrass, "l", col = "green")
points(1 : 99, resultsSeperate$BrClay, "l", col = "black")



#points(1: 99, resultsAllRatings$Br, "l", lty = 2, col = "blue")
#title( main = 'all ratings vs filtered')
#legend("topright",col= c("red", "blue"),lty=1:2,lwd=1,legend=c("filtered","all ratings"), bty="n")

resultsSeperate$ROI = (resultsSeperate$ROIHard + resultsSeperate$ROIGrass + resultsSeperate$ROIClay)/3

plot(1 : 99, resultsSeperate$ROI, "l", col = "red", xlab = "Quantile", ylab = "ROI", ylim = c(-0.08, 0.25))
points(1 : 99, resultsSeperate$ROIHard, "l", col = "blue")
points(1 : 99, resultsSeperate$ROIGrass, "l", col = "green")
points(1 : 99, resultsSeperate$ROIClay, "l", col = "black")

#title(main = 'all ratings vs filtered')
#points(1: 99, resultsAllRatings$ROI, "l", lty = 2, col = "blue")
#legend("topright",col= c("red", "blue"),lty=1:2,lwd=1,legend=c("filtered","all ratings"), bty="n")

plot(1:99, resultsSeperate$LogLossOutOfSampleHard, "l", col = "red", 
     ylim = c(0.50, 0.65), xlab = 'Quantile', ylab = 'logloss out sample Hard')

plot(1 : 99, results$LogLossOutOfSample, "l", col = "red", 
     ylim = c(0.50, 0.65), xlab = 'Quantile', ylab = 'logloss out sample')

#title( main = 'all ratings versus filtered')
#points(1: 99, resultsAllRatings$LogLossOutOfSample, "l", lty = 2, col = "blue")
#legend("topright",col= c("red", "blue"),lty=1:2,lwd=1,legend=c("filtered","all ratings"), bty="n")

#plot(1 : 99, results$LogLossInSample, "l", col = "red",
#     ylim = c(0.50, 0.65), xlab = 'Quantile', ylab = 'logloss in sample')
#title(main = 'all ratings vs filtered')
#points(1: 99, resultsAllRatings$LogLossInSample, "l", lty = 2, col = "blue")
#legend("topright",col= c("red", "blue"),lty=1:2,lwd=1,legend=c("filtered","all ratings"), bty="n")
points(1 : 99, results$LogLossInSample, "l", col = "blue")

min = min(results$LogLossOutOfSample)
match(min, results$LogLossOutOfSample)
min
mean(results$LogLossOutOfSample)
