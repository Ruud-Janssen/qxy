rm(list = ls())
source("formulas.r")
source("hyperparametersfunctions.r")
library(leaps)
library(bestglm)

train_modelwithRatings = read.table("D:/Betting/Tennis/Data/train_modelWithRatings.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings = read.table("D:/Betting/Tennis/Data/cvWithRatings.csv"
                                      , header = T, sep = ",", quote = "\"", fill = TRUE)

Nt = nrow(train_modelwithRatings)
cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]
Ncv = nrow(cv_withRatings)

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
  
  xt_mcurrent = removeUncertainMatches(xt_m, quantile)

  results$PercentageRemovedt_m[q] = 1 - length(yt_mcurrent) / length(yt_m)
  resultsSeperate$PercentageRemovedt_m[q] = results$PercentageRemovedt_m[q]
  
  xt_mcurrentGrass = getXThisSurface(xt_mcurrent, "Grass")
  xt_mcurrentHard = getXThisSurface(xt_mcurrent, "Hard")
  xt_mcurrentClay = getXThisSurface(xt_mcurrent, "Clay")
  
  xcvcurrent = removeUncertainMatches(xcv, quantile)
  
  xcvcurrentGrass = getXThisSurface(xcvcurrent, "Grass")
  xcvcurrentHard = getXThisSurface(xcvcurrent, "Hard")
  xcvcurrentClay = getXThisSurface(xcvcurrent, "Clay")
  
  results$PercentageRemovedcv[q] = 1 - length(ycvcurrent) / length(ycv)
  
  Reg = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
            +FatigueDiff + HeadtoHead, data = xt_mcurrent, family = binomial)
  
  #RegGrass = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
  #               +FatigueDiff + HeadtoHead, data = xt_mcurrentGrass, family = binomial)
  #RegHard = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
  #              +FatigueDiff + HeadtoHead, data = xt_mcurrentHard, family = binomial)
  #RegClay = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
  #              +FatigueDiff + HeadtoHead, data = xt_mcurrentClay, family = binomial)
  
  #Good model individually chosen:
  RegGrass = glm(y ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
                 + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                 , data = xt_mcurrentGrass, family = binomial)
  RegHard = glm(y ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff + 
                  DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                +FatigueDiff, data = xt_mcurrentHard, family = binomial)
  
  RegClay = glm(y ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff +
                  DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                +FatigueDiff, data = xt_mcurrentClay, family = binomial)
  
  results$LogLossInSample[q] = LogLoss(Reg$fitted.values, yt_mcurrent)
  
  results = cvpredictions(results, Reg, xcvcurrent, xcvcurrent$y, q)
  resultsSeperate = cvpredictions(resultsSeperate, RegGrass, xcvcurrentGrass, xcvcurrentGrass$y, q)
  resultsSeperate$ROIGrass[q] = resultsSeperate$ROI[q]
  resultsSeperate = cvpredictions(resultsSeperate, RegHard, xcvcurrentHard, xcvcurrentHard$y, q)
  resultsSeperate$ROIHard[q] = resultsSeperate$ROI[q]
  resultsSeperate = cvpredictions(resultsSeperate, RegClay, xcvcurrentClay, xcvcurrentClay$y, q)
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
