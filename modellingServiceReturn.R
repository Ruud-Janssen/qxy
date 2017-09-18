rm(list = ls())
source("formulas.R")
source("modellingServiceReturnFormulas.R")
source("addratingsformulas.r")
source("hyperparametersfunctions.R")
library(leaps)
library(bestglm)
library(glmnet)
library(plotmo)
library(glmulti)
library(caret)
library(ggplot2)

threshold <- 1e-9

train_modelwithRatings <- read.table("Data/datasets/train_modelWithRatings.csv"
                                     , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings         <- read.table("Data/datasets/cvWithRatings.csv"
                                     , header = T, sep = ",", quote = "\"", fill = TRUE)

train_modelwithRatings <- train_modelwithRatings[!is.na(train_modelwithRatings$matched), ]
cv_withRatings <- cv_withRatings[!is.na(cv_withRatings$matched), ]

Nt <- nrow(train_modelwithRatings)
#apparantly there is one NA in BestOF, temporarily removal needs to be data cleansed
cv_withRatings <- cv_withRatings[!is.na(cv_withRatings$Best.of), ]
Ncv            <- nrow(cv_withRatings)

set.seed(42)
numberOfTrainingPerspectives <- as.numeric(runif(Nt, 0, 1) > 0.5)
numberOfCvPerspectives       <- as.numeric(runif(Ncv, 0, 1) > 0.5)

xtm <- serviceReturnVariables(numberOfTrainingPerspectives, train_modelwithRatings)
xcv <- serviceReturnVariables(numberOfCvPerspectives, cv_withRatings)

q <- 27

xtmHard  <- getXThisSurface(xtm, "Hard")
quantileHard <- quantile(xtmHard$UncertaintySurface, q / 100)
xtmHard <- removeUncertainMatches(xtmHard, quantileHard, "Surface")
xtmHard <- xtmHard[!is.na(xtmHard$gameAresult), ]
xtmHard <- xtmHard[!is.na(xtmHard$gameBresult), ]

xcvHard  <- getXThisSurface(xcv, "Hard")
xcvHard <- removeUncertainMatches(xcvHard, quantileHard, "Surface")

xtmHardDouble                <- data.frame(result = c(xtmHard$gameAresult, xtmHard$gameBresult))
xtmHardDouble$ratingdiff     <- c(xtmHard$gameAratingdiff, xtmHard$gameBratingdiff)
xtmHardDouble$ratingHarddiff <- c(xtmHard$gameAratingHarddiff, xtmHard$gameBratingHarddiff)

regHard <- glm(result ~ 0 + ratingHarddiff + ratingdiff
                    , data = xtmHardDouble, family = quasibinomial, weights = rep(1, nrow(xtmHardDouble)))

summary(regHard)

xcvHardDouble                 <- data.frame(result = xcvHard$result)
xcvHardDouble$ratingHarddiffA <- xcvHard$gameAratingHarddiff
xcvHardDouble$ratingHarddiffB <- xcvHard$gameBratingHarddiff
xcvHardDouble$ratingdiffA     <- xcvHard$gameAratingdiff
xcvHardDouble$ratingdiffB     <- xcvHard$gameBratingdiff
xcvHardDouble$bo5             <- xcvHard$Best.of

a <- data.frame(ratingHarddiff = xcvHardDouble$ratingHarddiffA, ratingdiff = xcvHardDouble$ratingdiffA)
b <- data.frame(ratingHarddiff = xcvHardDouble$ratingHarddiffB, ratingdiff = xcvHardDouble$ratingdiffB) 

xcvHardDouble$predictedPointsGameA <- predict(regHard, a, type = "response")
xcvHardDouble$predictedPointsGameB <- predict(regHard, b, type = "response")

xcvHardDouble$probWin <- 0

simul  <- 19
weight <- simul ^ 2

approxTrials = NULL
approxTrials[c(as.character(3),as.character(5))] = c(71, 110)

for(i in 1 : nrow(xcvHardDouble)){
  Trials <- approxTrials[as.character(xcvHardDouble$bo5[i])]
  
  for(j in 1 : simul) {
    sc <- qbinom(p = 0.05 * j, size = Trials, 
                               prob = xcvHardDouble$predictedPointsGameA[i]) / Trials
    for(k in 1 : simul) {
      rc <- qbinom(p = 0.05 * k, size = Trials, 
                   prob = xcvHardDouble$predictedPointsGameB[i]) / Trials
      
      xcvHardDouble$probWin[i] <- xcvHardDouble$probWin[i] +
        1 / weight * matchProb(s = sc, t = rc, sets = xcvHardDouble$bo5[i])
    }
  }
}

# for(i in 1 : nrow(xcvHardDouble)){
#   for(j in 1 : simul) {
#     sb <- (j - 3) * 0.02
#     for(k in 1 : simul) {
#       sc <- (k - 3) * 0.02
#       xcvHardDouble$probWin[i] <- xcvHardDouble$probWin[i] +
#         1 / weight * matchProb(s = xcvHardDouble$predictedPointsGameA[i] + sb,
#                                t = xcvHardDouble$predictedPointsGameB[i] + sc,
#                                sets = xcvHardDouble$bo5[i])
#     }
#   }
# }
# 
# for(i in 1 : nrow(xcvHardDouble)){
#  xcvHardDouble$probWin[i] <- 1 / 5 * (
#    matchProb(s = xcvHardDouble$predictedPointsGameA[i] - 0.05,
#              t = xcvHardDouble$predictedPointsGameB[i] - 0.05,
#              sets = xcvHardDouble$bo5[i]) +
#    matchProb(s = xcvHardDouble$predictedPointsGameA[i] - 0.02,
#              t = xcvHardDouble$predictedPointsGameB[i] - 0.02 ,
#              sets = xcvHardDouble$bo5[i]) +
#    matchProb(s = xcvHardDouble$predictedPointsGameA[i],
#              t = xcvHardDouble$predictedPointsGameB[i],
#              sets = xcvHardDouble$bo5[i]) +
# 
#    matchProb(s = xcvHardDouble$predictedPointsGameA[i] + 0.02,
#              t = xcvHardDouble$predictedPointsGameB[i] + 0.02,
#              sets = xcvHardDouble$bo5[i]) +
#    matchProb(s = xcvHardDouble$predictedPointsGameA[i] + 0.05,
#              t = xcvHardDouble$predictedPointsGameB[i] + 0.05,
#              sets = xcvHardDouble$bo5[i]))
# }

LogLoss(xcvHardDouble$result, xcvHardDouble$probWin)