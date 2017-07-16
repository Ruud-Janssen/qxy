rm(list = ls())

source("formulas.r")
source("hyperparametersfunctions.r")
source("bestsubsetsformulas.r")

library(neuralnet)

train_modelwithRatings = read.table("Data/datasets/train_modelWithRatings.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings = read.table("Data/datasets/cvWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)

Nt = nrow(train_modelwithRatings)
#apparantly there is one NA in BestOF, temporarily removal needs to be data cleansed
cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]
Ncv = nrow(cv_withRatings)

set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)
ycv  = as.numeric(runif(Ncv, 0, 1) > 0.5)

xt_m = regressorvariables(yt_m, train_modelwithRatings)
xcv = regressorvariables(ycv, cv_withRatings)

q = 27

xt_mHard = getXThisSurface(xt_m, "Hard")
xt_mGrass = getXThisSurface(xt_m, "Grass")
xt_mClay = getXThisSurface(xt_m, "Clay")

#quantileHard = quantile(xt_mHard$UncertaintyCOSurface, q / 100)
quantileHard = quantile(xt_mHard$UncertaintySurface, q / 100)

#xt_mHard = removeUncertainMatches(xt_mHard, quantileHard, "COSurface")
xt_mHard = removeUncertainMatches(xt_mHard, quantileHard, "Surface")

#quantile = quantile(xcv$Uncertainty, q / 100)

xcvHard = getXThisSurface(xcv, "Hard")
xcvGrass = getXThisSurface(xcv, "Grass")
xcvClay = getXThisSurface(xcv, "Clay")

#xcvHard = removeUncertainMatches(xcvHard, quantileHard, "COSurface")
xcvHard = removeUncertainMatches(xcvHard, quantileHard, "Surface")

xtmHardRel = relevantVariables(xt_mHard)
xcvHardRel = relevantVariables(xcvHard)

#xtmHardRel = xtmHardRel[!is.na(xtmHardRel$COPercentPointsDiff), ]
#xcvHardRel = xcvHardRel[!is.na(xcvHardRel$COPercentPointsDiff), ]

#Normalization
sdtrain <- apply(xtmHardRel[, 1:(length(xtmHardRel) - 1)] , 2, sd) 
#meantrain <- apply(xtmHardRel[, 1:(length(xtmHardRel) - 1)] , 2, mean) 
#xtmHardRel[, 1:(length(xtmHardRel) - 1)] = xtmHardRel[, 1:(length(xtmHardRel) - 1)] - meantrain
#xcvHardRel[, 1:(length(xcvHardRel) - 1)] = xcvHardRel[, 1:(length(xcvHardRel) - 1)] - meantrain

xtmHardRel[, 1:(length(xtmHardRel) - 1)] = 
  as.data.frame(scale(xtmHardRel[, 1:(length(xtmHardRel) - 1)], center = FALSE,scale = sdtrain))
xcvHardRel[, 1:(length(xcvHardRel) - 1)] = 
  as.data.frame(scale(xcvHardRel[, 1:(length(xcvHardRel) - 1)], center = FALSE,scale = sdtrain))

n <- names(xtmHardRel)
f <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))

nn = neuralnet(f, data = xtmHardRel, hidden=c(1), act.fct = 'logistic' ,linear.output=F, lifesign = 'full'
               , stepmax = 5000000)

predictionscv <- compute(nn, xcvHardRel[, 1 : (length(xcvHardRel) - 1)])

LogLoss(predictionscv$net.result, xcvHardRel$y)
