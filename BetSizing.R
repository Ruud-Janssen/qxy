rm(list = ls())
source("D:/Betting/Tennis/formulas.r")
source("D:/Betting/Tennis/hyperparametersfunctions.r")
source("D:/Betting/Tennis/BetSizingFormulas.r")
library(leaps)
library(bestglm)
library(glmnet)

train_modelwithRatings = read.table("D:/Betting/Tennis/Data/train_modelWithRatings.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings = read.table("D:/Betting/Tennis/Data/cvWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)

Nt = nrow(train_modelwithRatings)
#apparantly there is one NA in BestOF, temporarily removal needs to be data cleansed
cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]
Ncv =nrow(cv_withRatings)

set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)
ycv = as.numeric(runif(Ncv, 0, 1) > 0.5)

xt_m = regressorvariables(yt_m, train_modelwithRatings)
xcv = regressorvariables(ycv, cv_withRatings)

q = 27

quantile = quantile(xt_m$Uncertainty, q / 100)

index_xt_m = (xt_m$Uncertainty < quantile)
xt_m = xt_m[index_xt_m, ]
yt_m = yt_m[index_xt_m]
results$PercentageRemovedt_m[q] = 1 - length(yt_m) / length(yt_m)
resultsSeperate$PercentageRemovedt_m[q] = results$PercentageRemovedt_m[q]

indexGrasst_m = (xt_m$Surface == "Grass")
indexHardt_m = (xt_m$Surface == "Hard")

xt_mGrass = xt_m[indexGrasst_m, ]
xt_mHard = xt_m[indexHardt_m, ]

yt_mGrass = yt_m[indexGrasst_m]
yt_mHard = yt_m[indexHardt_m]

index_xcv = (xcv$Uncertainty < quantile)
xcv = xcv[index_xcv, ]
ycv = ycv[index_xcv]

indexGrasscv = (xcv$Surface == "Grass")
indexHardcv = (xcv$Surface == "Hard")

xcvGrass = xcv[indexGrasscv, ]
xcvHard = xcv[indexHardcv, ]

ycvGrass = ycv[indexGrasscv]
ycvHard = ycv[indexHardcv]

xcvHard$ImpProb = xcvHard$PSLthisplayer/(xcvHard$PSLthisplayer + xcvHard$PSWthisplayer)
xcvGrass$ImpProb = xcvGrass$PSLthisplayer/(xcvGrass$PSLthisplayer + xcvGrass$PSWthisplayer)

iHard = !is.na(xcvHard$ImpProb)
iGrass = !is.na(xcvGrass$ImpProb)

xcvHard = xcvHard[iHard, ]
xcvGrass = xcvGrass[iGrass, ]

ycvHard = ycvHard[iHard]
ycvGrass = ycvGrass[iGrass]

regHard = glm(yt_mHard ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
                RetiredDiff + FatigueDiff, data = xt_mHard, family = binomial)

regGrass = glm(yt_mGrass ~ 0 + ratingdiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff +
                 FatigueDiff , data = xt_mGrass, family = binomial)

cvpredHard = predict(regHard, xcvHard, type = "response")
cvpredGrass = predict(regGrass, xcvGrass, type = "response")

xt_mHard$ImpProb = xt_mHard$PSLthisplayer/(xt_mHard$PSLthisplayer + xt_mHard$PSWthisplayer)
xt_mGrass$ImpProb = xt_mGrass$PSLthisplayer/(xt_mGrass$PSLthisplayer + xt_mGrass$PSWthisplayer)

combinedregHard = glm(ycvHard ~ 0 + cvpredHard +  ImpProb, data = xcvHard)
combinedregGrass = glm(ycvGrass ~ 0 + cvpredGrass + ImpProb, data = xcvGrass)

resultsHard = result(combinedregHard$fitted.values, xcvHard, ycvHard)
resultsGrass = result(combinedregGrass$fitted.values, xcvGrass, ycvGrass)