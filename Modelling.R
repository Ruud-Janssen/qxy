rm(list = ls())
source("D:/Betting/Tennis/formulas.r")
source("D:/Betting/Tennis/hyperparametersfunctions.r")
library(leaps)
library(bestglm)

train_modelwithRatings = read.table("Data/datasets/train_modelwithRatings.csv"
                         , header = T, sep = ",", quote = "\"", fill = TRUE)

Nt = nrow(train_modelwithRatings)

y = as.numeric(runif(Nt, 0, 1) > 0.5)
x = regressorvariables(y, train_modelwithRatings)

compareReg = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
                 +FatigueDiff + HeadtoHead, data = x, family = binomial)
summary(compareReg)
LogLoss(compareReg$fitted.values, y)

logistic1 = glm(y ~ 0 + ratingdiff,data = x, family = binomial)
summary(logistic1)
LogLoss(logistic1$fitted.values, y)

logistic2 = glm(y ~ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
                + ratingCarpetdiff, data = x, family = binomial)
summary(logistic2)

logistic3 = glm(y ~ 0 + ratingdiff + DummyBo5TimesRatingdiff,data = x, family = binomial)
summary(logistic3)

logistic3_nointeraction = glm(y ~ 0 + ratingdiff + DummyBo5, data = x, family = binomial)
summary(logistic3_nointeraction)

logistic4 = glm(y ~ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff + ratingCarpetdiff 
                + DummyBo5TimesRatingdiff, data = x, family = binomial)
summary(logistic4)

logistic5 = glm(y ~ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
                + DummyBo5TimesRatingdiff, data = x, family = binomial)
summary(logistic5)

logistic6 = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface, data = x, family = binomial)
summary(logistic6)
LogLoss(logistic6$fitted.value, y)

logistic7 = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesRatingdiff 
                , data = x, family = binomial)
summary(logistic7)

logistic7_DummyAverageRating = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + DummyBo5TimesAvgRatingdiff, 
                             data = x, family = binomial)
summary(logistic7_DummyAverageRating)


logistic7_DummyAverageRating_ret = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
                                         DummyBo5TimesAvgRatingdiff + retiredDiff, data = x, family = binomial)
summary(logistic7_DummyAverageRating_ret)

logistic7_DummyAverageRating_fat = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
                                         DummyBo5TimesAvgRatingdiff + FatigueDiff, data = x, family = binomial)
summary(logistic7_DummyAverageRating_fat)

logistic7_DummyAverageRating_ret_fat = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
                                         DummyBo5TimesAvgRatingdiff + retiredDiff +  
                                           FatigueDiff, data = x, family = binomial)
summary(logistic7_DummyAverageRating_ret_fat)

#only 110 points for walkover so yeah...
logistic7_DummyAverageRating_ret_walk_fat = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
                                             DummyBo5TimesAvgRatingdiff + retiredDiff + WalkoverDiff +
                                             FatigueDiff, data = x, family = binomial)
summary(logistic7_DummyAverageRating_ret_walk_fat)

logistic7_DummyAverageRating_retOrwalk_fat = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
                                                  DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff + 
                                                  FatigueDiff, data = x, family = binomial)
summary(logistic7_DummyAverageRating_retOrwalk_fat)

logistic7_DummyAverageRating_retOrwalk_fat_LastHeadtoHead = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
                                                                  DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff + 
                                                                  FatigueDiff + LastHeadtoHead, data = x, 
                                                                family = binomial)
summary(logistic7_DummyAverageRating_retOrwalk_fat_LastHeadtoHead)

logistic7_DummyAverageRating_retOrwalk_fat_HeadtoHead = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
                                                                  DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff + 
                                                                  FatigueDiff + HeadtoHead, data = x, 
                                                                family = binomial)
summary(logistic7_DummyAverageRating_retOrwalk_fat_HeadtoHead)

logistic7_DummyAverageRating_retOrwalk_fat_HeadtoHeadPercentweighted = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
                                                              DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff + 
                                                              FatigueDiff + HeadtoHeadPercentageWeightedsqN, data = x, 
                                                            family = binomial)
summary(logistic7_DummyAverageRating_retOrwalk_fat_HeadtoHeadPercentweighted )


#x$HeadtoHead = x$HeadtoHead - x$LastHeadtoHead
#
#logistic7_DummyAverageRating_retOrwalk_fat_LastHeadtoHead_HeadtoHead = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + 
#                                                                  DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff + 
#                                                                  FatigueDiff + LastHeadtoHead + HeadtoHead, 
#                                                                  data = x, family = binomial)
#summary(logistic7_DummyAverageRating_retOrwalk_fat_LastHeadtoHead_HeadtoHead)


logistic8 = glm(y ~ 0 + ratingdiff + DummyBo5TimesRatingdiffSquaredSameSign ,data = x, family = binomial)
summary(logistic8)

logistic9 = glm(y ~ 0 + ratingdiff + DummyBo5TimesRatingdiff + DummyBo5TimesRatingdiffSquaredSameSign ,data = x, family = binomial)
summary(logistic9)

#Split the 4 variables to estimate seperately
yH = y[x$DummyHard == 1]
xH = x[x$DummyHard == 1, ]

yG = y[x$DummyGrass == 1]
xG = x[x$DummyGrass == 1, ]

yCL = y[x$DummyClay == 1]
xCL = x[x$DummyClay == 1, ]

yCA = y[x$DummyCarpet == 1]
xCA = x[x$DummyCarpet == 1, ]

logisticyH = glm(yH ~ ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
                 + ratingCarpetdiff + DummyBo5TimesAvgRatingdiff, data = xH, family = binomial)
summary(logisticyH)

logisticyG = glm(yG ~ ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
                 + ratingCarpetdiff + DummyBo5TimesAvgRatingdiff, data = xG, family = binomial)
summary(logisticyG)

logisticyCL = glm(yCL ~ ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
                 + ratingCarpetdiff + DummyBo5TimesAvgRatingdiff, data = xCL, family = binomial)
summary(logisticyCL)

logisticyCA = glm(yCA ~ ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff 
                 + ratingCarpetdiff + DummyBo5TimesAvgRatingdiff, data = xCA, family = binomial)
summary(logisticyCA)

logLikH = logLoss(logisticyH$fitted.values, yH)
logLikG = logLoss(logisticyG$fitted.values, yG)
logLikCL = logLoss(logisticyCL$fitted.values, yCL)
logLikCA = logLoss(logisticyCA$fitted.values, yCA)
avgLogLoss = 1/Nt * (length(yH) * logLikH + length(yG) *logLikG + length(yCL) * logLikCL
                     + length(yCA) * logLikCA)


#Make a lot of variables to create 4 submodels
x$Hardratingdiff = x$DummyHard * x$ratingdiff 
x$Clayratingdiff = x$DummyClay * x$ratingdiff 
x$Grassratingdiff = x$DummyGrass * x$ratingdiff 
x$Carpetratingdiff = x$DummyCarpet * x$ratingdiff 

x$HardratingdiffCurrentSurface = x$DummyHard * x$ratingdiffCurrentSurface
x$ClayratingdiffCurrentSurface = x$DummyClay * x$ratingdiffCurrentSurface
x$GrassratingdiffCurrentSurface = x$DummyGrass * x$ratingdiffCurrentSurface
x$CarpetratingdiffCurrentSurface = x$DummyCarpet * x$ratingdiffCurrentSurface

x$Bo5TimesHardRatingdiff = x$DummyBo5 * x$HardratingdiffCurrentSurface
x$Bo5TimesClayRatingdiff = x$DummyBo5 * x$ClayratingdiffCurrentSurface
x$Bo5TimesGrassRatingdiff = x$DummyBo5 * x$GrassratingdiffCurrentSurface
x$Bo5TimesCarpetRatingdiff = x$DummyBo5 * x$CarpetratingdiffCurrentSurface

logistic10 = glm(y ~ Hardratingdiff + Grassratingdiff + Carpetratingdiff
                 +HardratingdiffCurrentSurface + ClayratingdiffCurrentSurface + GrassratingdiffCurrentSurface
                 +CarpetratingdiffCurrentSurface, data = x, family = binomial)
summary(logistic10)
logistic10_Bo5 = glm(y ~ Hardratingdiff + Grassratingdiff + Carpetratingdiff
                     +HardratingdiffCurrentSurface + ClayratingdiffCurrentSurface + GrassratingdiffCurrentSurface
                     +CarpetratingdiffCurrentSurface + x$Bo5TimesHardRatingdiff + x$Bo5TimesClayRatingdiff 
                     + x$Bo5TimesGrassRatingdiff + x$Bo5TimesCarpetRatingdiff, data = x, family = binomial)
summary(logistic10_Bo5)
## ALL NOT SIGNIFICANT, VERY SURPRISING


TrainBestSubset = as.data.frame(matrix(nrow = Nt, ncol = 0))

TrainBestSubset$ratingdiff = x$ratingdiff
TrainBestSubset$ratingdiffCurrentSurface =x$ratingdiffCurrentSurface
TrainBestSubset$ratingClaydiff = x$ratingClaydiff
TrainBestSubset$ratingHarddiff = x$ratingHarddiff
TrainBestSubset$ratingGrassdiff = x$ratingGrassdiff
TrainBestSubset$ratingCarpetdiff = x$ratingCarpetdiff
TrainBestSubset$DummyBo5TimesRatingdif = x$DummyBo5TimesRatingdiff
TrainBestSubset$DummyBo5TimesRatingdiffSquaredSameSign = x$DummyBo5TimesRatingdiffSquaredSameSign
TrainBestSubset$DummyBo5TimesRatingdiffthirdSameSign = x$DummyBo5TimesRatingdiffthirdSameSign
TrainBestSubset$DummyBo5TimesRatingdiff_5SameSign = x$DummyBo5TimesRatingdiff_5SameSign
TrainBestSubset$DummyBo5TimesRatingdiff1_5SameSign = x$DummyBo5TimesRatingdiff1_5SameSign
TrainBestSubset$y = y

Bestsubsets = bestglm(TrainBestSubset, family = binomial, IC = "AIC")

check = glm(y ~ 0 + ratingdiff + ratingdiffCurrentSurface + ratingClaydiff + ratingGrassdiff
           + DummyBo5TimesRatingdiffSquaredSameSign + x$DummyBo5TimesRatingdiff1_5SameSign + 
           x$DummyBo5TimesRatingdiffthirdSameSign, data = x, family = binomial)
summary(check)

#Lets analyse clay
i = (x$DummyClay == 1)

Nclay = sum(x$DummyClay)

TrainBestSubsetClay = as.data.frame(matrix(nrow = Nclay, ncol = 0))

TrainBestSubsetClay$ratingdiff = x$ratingdiff[i]
TrainBestSubsetClay$ratingClaydiff = x$ratingClaydiff[i]
TrainBestSubsetClay$ratingHarddiff = x$ratingHarddiff[i]
TrainBestSubsetClay$ratingGrassdiff = x$ratingGrassdiff[i]
TrainBestSubsetClay$ratingCarpetdiff = x$ratingCarpetdiff[i]
TrainBestSubsetClay$DummyBo5TimesRatingdif = x$DummyBo5TimesRatingdiff[i]
TrainBestSubsetClay$DummyBo5TimesRatingdiffSquaredSameSign = x$DummyBo5TimesRatingdiffSquaredSameSign[i]
TrainBestSubsetClay$DummyBo5TimesRatingdiffthirdSameSign = x$DummyBo5TimesRatingdiffthirdSameSign[i]
TrainBestSubsetClay$DummyBo5TimesRatingdiff_5SameSign = x$DummyBo5TimesRatingdiff_5SameSign[i]
TrainBestSubsetClay$DummyBo5TimesRatingdiff1_5SameSign = x$DummyBo5TimesRatingdiff1_5SameSign[i]
TrainBestSubsetClay$y = y[i]

BestsubsetsClay = bestglm(TrainBestSubsetClay, family = binomial, IC = "AIC")

BestsubsetsClay$BestModel

ModelClay = glm(y ~ 0 + ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff + ratingCarpetdiff 
            + DummyBo5TimesRatingdiffSquaredSameSign, data = TrainBestSubsetClay, family= binomial)
summary(ModelClay)
logLik(ModelClay) / Nclay

logLik(logistic7) / Nt

##Hard
i = (x$DummyHard == 1)

NHard = sum(x$DummyHard)

TrainBestSubsetHard = as.data.frame(matrix(nrow = NHard, ncol = 0))

TrainBestSubsetHard$ratingdiff = x$ratingdiff[i]
TrainBestSubsetHard$ratingClaydiff = x$ratingClaydiff[i]
TrainBestSubsetHard$ratingHarddiff = x$ratingHarddiff[i]
TrainBestSubsetHard$ratingGrassdiff = x$ratingGrassdiff[i]
TrainBestSubsetHard$ratingCarpetdiff = x$ratingCarpetdiff[i]
TrainBestSubsetHard$DummyBo5TimesRatingdiff = x$DummyBo5TimesRatingdiff[i]
TrainBestSubsetHard$DummyBo5TimesRatingdiffSquaredSameSign = x$DummyBo5TimesRatingdiffSquaredSameSign[i]
TrainBestSubsetHard$DummyBo5TimesRatingdiffthirdSameSign = x$DummyBo5TimesRatingdiffthirdSameSign[i]
TrainBestSubsetHard$DummyBo5TimesRatingdiff_5SameSign = x$DummyBo5TimesRatingdiff_5SameSign[i]
TrainBestSubsetHard$DummyBo5TimesRatingdiff1_5SameSign = x$DummyBo5TimesRatingdiff1_5SameSign[i]
TrainBestSubsetHard$y = y[i]

BestsubsetsHard = bestglm(TrainBestSubsetHard, family = binomial, IC = "AIC")

BestsubsetsHard$BestModel

ModelHard = glm(y ~ 0 + ratingdiff + ratingHarddiff + ratingGrassdiff + ratingCarpetdiff 
            + DummyBo5TimesRatingdiff, data = TrainBestSubsetHard, family= binomial)
summary(ModelHard)
logLik(ModelHard) / NHard

logLik(logistic7) / Nt

##Grass
i = (x$DummyGrass == 1)

NGrass = sum(x$DummyGrass)

TrainBestSubsetGrass = as.data.frame(matrix(nrow = NGrass, ncol = 0))

TrainBestSubsetGrass$ratingdiff = x$ratingdiff[i]
TrainBestSubsetGrass$ratingClaydiff = x$ratingClaydiff[i]
TrainBestSubsetGrass$ratingHarddiff = x$ratingHarddiff[i]
TrainBestSubsetGrass$ratingGrassdiff = x$ratingGrassdiff[i]
TrainBestSubsetGrass$ratingCarpetdiff = x$ratingCarpetdiff[i]
TrainBestSubsetGrass$DummyBo5TimesRatingdiff = x$DummyBo5TimesRatingdiff[i]
TrainBestSubsetGrass$DummyBo5TimesRatingdiffSquaredSameSign = x$DummyBo5TimesRatingdiffSquaredSameSign[i]
TrainBestSubsetGrass$DummyBo5TimesRatingdiffthirdSameSign = x$DummyBo5TimesRatingdiffthirdSameSign[i]
TrainBestSubsetGrass$DummyBo5TimesRatingdiff_5SameSign = x$DummyBo5TimesRatingdiff_5SameSign[i]
TrainBestSubsetGrass$DummyBo5TimesRatingdiff1_5SameSign = x$DummyBo5TimesRatingdiff1_5SameSign[i]
TrainBestSubsetGrass$y = y[i]

BestsubsetsGrass = bestglm(TrainBestSubsetGrass, family = binomial, IC = "AIC")

BestsubsetsGrass$BestModel

ModelGrass = glm(y ~ 0 + ratingdiff + ratingHarddiff + ratingGrassdiff + DummyBo5TimesRatingdiff
            + DummyBo5TimesRatingdiffSquaredSameSign, data = TrainBestSubsetGrass, family= binomial)
summary(ModelGrass)
logLik(ModelGrass) / NGrass

logLik(logistic7) / Nt

##Carpet
i = (x$DummyCarpet == 1)

NCarpet = sum(x$DummyCarpet)

TrainBestSubsetCarpet = as.data.frame(matrix(nrow = NCarpet, ncol = 0))

TrainBestSubsetCarpet$ratingdiff = x$ratingdiff[i]
TrainBestSubsetCarpet$ratingClaydiff = x$ratingClaydiff[i]
TrainBestSubsetCarpet$ratingHarddiff = x$ratingHarddiff[i]
TrainBestSubsetCarpet$ratingGrassdiff = x$ratingGrassdiff[i]
TrainBestSubsetCarpet$ratingCarpetdiff = x$ratingCarpetdiff[i]
TrainBestSubsetCarpet$DummyBo5TimesRatingdiff = x$DummyBo5TimesRatingdiff[i]
TrainBestSubsetCarpet$DummyBo5TimesRatingdiffSquaredSameSign = x$DummyBo5TimesRatingdiffSquaredSameSign[i]
TrainBestSubsetCarpet$DummyBo5TimesRatingdiffthirdSameSign = x$DummyBo5TimesRatingdiffthirdSameSign[i]
TrainBestSubsetCarpet$DummyBo5TimesRatingdiff_5SameSign = x$DummyBo5TimesRatingdiff_5SameSign[i]
TrainBestSubsetCarpet$DummyBo5TimesRatingdiff1_5SameSign = x$DummyBo5TimesRatingdiff1_5SameSign[i]
TrainBestSubsetCarpet$y = y[i]

BestsubsetsCarpet = bestglm(TrainBestSubsetCarpet, family = binomial, IC = "AIC")

BestsubsetsCarpet$BestModel

ModelCarpet = glm(y ~ 0 + ratingdiff + ratingHarddiff + ratingGrassdiff
            + DummyBo5TimesRatingdiffSquaredSameSign, data = TrainBestSubsetCarpet, family= binomial)
summary(ModelCarpet)
logLik(ModelCarpet) / NCarpet

sumLogLik= logLik(ModelClay) + logLik(ModelHard) + logLik(ModelGrass) + logLik(ModelCarpet) 

sumLogLik
logLik(logistic7)

#Though I will definitely CV test it, it appears that the general model7 could be better
#Than the slightly better 4 different models since the loglik appears to be only slightly better
#With a huge df even though the formula doesn't see it
