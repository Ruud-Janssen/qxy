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
Ncv =nrow(cv_withRatings)

set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)
ycv = as.numeric(runif(Ncv, 0, 1) > 0.5)

xt_m = regressorvariables(yt_m, train_modelwithRatings)

q = 27

quantile = quantile(xt_m$Uncertainty, q / 100)

xt_m = removeUncertainMatches(xt_m, quantile)
xt_mHard = getXThisSurface(xt_m, "Hard")
xtmHardRel = relevantVariables(xt_mHard)

maxs <- apply(xtmHardRel, 2, max) 
mins <- apply(xtmHardRel, 2, min)

xtmHardScaled <- as.data.frame(scale(xtmHardRel, center = rep(0,length(xtmHardRel)), scale = 0.5 *(maxs - mins)))
xtmHardScaled$y = xtmHardScaled$y / 2

xcv = regressorvariables(ycv, cv_withRatings)
xcv = removeUncertainMatches(xcv, quantile)
xcvHard = getXThisSurface(xcv, "Hard")
xcvHardRel = relevantVariables(xcvHard)

xcvHardScaled <- as.data.frame(scale(xcvHardRel, center = rep(0,length(xcvHardRel)), scale = 0.5 *(maxs - mins)))
xcvHardScaled$y = xcvHardScaled$y / 2

n <- names(xtmHardScaled)
f <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))

nn = neuralnet(f,data=xtmHardScaled,hidden=c(1, 1), act.fct = 'logistic' ,linear.output=F, lifesign = 'full'
               , stepmax = 5000000)




predictionscv <- compute(nn, xcvHardScaled[, 1 : (length(xcvHardScaled) - 1)])

LogLoss(predictionscv$net.result, xcvHardScaled$y)
