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
indexClayt_m = (xt_m$Surface == "Clay")

xt_mGrass = xt_m[indexGrasst_m, ]
xt_mHard = xt_m[indexHardt_m, ]
xt_mClay = xt_m[indexClayt_m, ]

yt_mGrass = yt_m[indexGrasst_m]
yt_mHard = yt_m[indexHardt_m]
yt_mClay = yt_m[indexClayt_m]

index_xcv = (xcv$Uncertainty < quantile)
xcv = xcv[index_xcv, ]
ycv = ycv[index_xcv]

indexGrasscv = (xcv$Surface == "Grass")
indexHardcv = (xcv$Surface == "Hard")
indexClaycv = (xcv$Surface == "Clay")

xcvGrass = xcv[indexGrasscv, ]
xcvHard = xcv[indexHardcv, ]
xcvClay = xcv[indexClaycv, ]

ycvGrass = ycv[indexGrasscv]
ycvHard = ycv[indexHardcv]
ycvClay = ycv[indexClaycv]

xtmHardRel = relevantVariables(xt_mHard)


maxs <- apply(xtmHardRel, 2, max) 
mins <- apply(xtmHardRel, 2, min)

xtmHardScaled <- as.data.frame(scale(xtmHardRel, center = rep(0,length(xtmHardRel)), scale = 0.5 *(maxs - mins)))
xtmHardScaled$y = yt_mHard



n <- names(xtmHardScaled)
f <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))

nn = neuralnet(f,data=xtmHardScaled,hidden=c(8,5), act.fct = 'logistic' ,linear.output=F, lifesign = 'full'
               , stepmax = 5000000)

xcvHardRel = relevantVariables(xcvHard)

xcvHardScaled <- as.data.frame(scale(xcvHardRel, center = rep(0,length(xcvHardRel)), scale = 0.5 *(maxs - mins)))


predictionscv <- compute(nn, xcvHardScaled)
