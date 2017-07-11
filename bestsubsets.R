rm(list = ls())
source("formulas.r")
source("hyperparametersfunctions.r")
source("bestsubsetsformulas.r")
library(leaps)
library(bestglm)
library(glmnet)
library(plotmo)
library(glmulti)

threshold = 1e-9

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

xt_m = removeUncertainMatches(xt_m, quantile)

xt_mHard = getXThisSurface(xt_m, "Hard")
xt_mGrass = getXThisSurface(xt_m, "Grass")
xt_mClay = getXThisSurface(xt_m, "Clay")

#quantile = quantile(xcv$Uncertainty, q / 100)
xcv = removeUncertainMatches(xcv, quantile)

xcvHard = getXThisSurface(xcv, "Hard")
xcvGrass = getXThisSurface(xcv, "Grass")
xcvClay = getXThisSurface(xcv, "Clay")

BestSubsetHardt_m = relevantVariables(xt_mHard)
xcvHardRel = relevantVariables(xcvHard)


#Normalization
sdtrain <- apply(BestSubsetHardt_m[, 1:(length(BestSubsetHardt_m) - 1)] , 2, sd) 
#meantrain <- apply(BestSubsetHardt_m[, 1:(length(BestSubsetHardt_m) - 1)] , 2, mean) 
#BestSubsetHardt_m[, 1:(length(BestSubsetHardt_m) - 1)] = BestSubsetHardt_m[, 1:(length(BestSubsetHardt_m) - 1)] - meantrain
#xcvHardRel[, 1:(length(xcvHardRel) - 1)] = xcvHardRel[, 1:(length(xcvHardRel) - 1)] - meantrain

BestSubsetHardt_m[, 1:(length(BestSubsetHardt_m) - 1)] = 
  as.data.frame(scale(BestSubsetHardt_m[, 1:(length(BestSubsetHardt_m) - 1)], center = FALSE,scale = sdtrain))
xcvHardRel[, 1:(length(xcvHardRel) - 1)] = 
  as.data.frame(scale(xcvHardRel[, 1:(length(xcvHardRel) - 1)], center = FALSE,scale = sdtrain))
  

#Modelling
regLamMin = glm(y ~ 0 + ratingNotHarddiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff2 + RetiredDiff
                + FatigueDiff
                 , data = BestSubsetHardt_m, family = binomial)

summary(regLamMin)

cvpredLamMin = predict(regLamMin, xcvHardRel, type = "response")
LogLoss(cvpredLamMin, xcvHardRel$y)


grid = 10 ^ seq(0, -10, length = 100)
#####RIDGE
ridge.mod = glmnet(as.matrix(BestSubsetHardt_m[ , 1:(length(BestSubsetHardt_m)-1)]), xt_mHard$y, 
                   family = "binomial", alpha = 0, lambda = grid, intercept = FALSE, thresh = threshold)

plot_glmnet(ridge.mod, xvar = "lambda")

ridge.out = cv.glmnet(as.matrix(BestSubsetHardt_m[ , 1:(length(BestSubsetHardt_m)-1)])
            , xt_mHard$y, alpha = 0, nfolds = 10, family = "binomial", lambda = grid, 
            intercept = FALSE , thresh = threshold)
plot(ridge.out)

##random stackoverflow says the second one is preferred!!!
##https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
bestridgelam = ridge.out$lambda.min
#bestlam = ridge.out$lambda.1se

ridge = glmnet(as.matrix(BestSubsetHardt_m[ , 1:(length(BestSubsetHardt_m)-1)]), xt_mHard$y, alpha = 0, lambda = bestridgelam, family = "binomial",
               intercept = FALSE, thresh = threshold)
ridge.coef = predict(ridge.out, type = "coefficients", s = bestridgelam)
ridge.coef
bestridgelam

cvpredRidgeMinTrue = predict.cv.glmnet(ridge.out, newx = as.matrix(xcvHardRel[, 1:(length(xcvHardRel)-1)])
                                       , type = "response", s = bestridgelam, thresh = threshold)
LogLoss(cvpredRidgeMinTrue, xcvHardRel$y)

#####LASSO
cvlam.out = cv.glmnet(as.matrix(BestSubsetHardt_m[ , 1:(length(BestSubsetHardt_m)-1)]), BestSubsetHardt_m$y, alpha = 0, nfolds = 10, family = "binomial", 
                      intercept = FALSE, thresh = threshold, lambda = grid)
plot(cvlam.out)


lasso.mod = glmnet(as.matrix(BestSubsetHardt_m[ , 1:(length(BestSubsetHardt_m)-1)]), BestSubsetHardt_m$y, family = "binomial", alpha = 1, lambda = grid
               , intercept = FALSE, thresh = threshold)
plot_glmnet(lasso.mod, xvar = "lambda")

#Lambda subset selection
cvlas.out = cv.glmnet(as.matrix(BestSubsetHardt_m[ , 1:(length(BestSubsetHardt_m)-1)]), BestSubsetHardt_m$y, alpha = 1, nfolds = 10, family = "binomial", 
                      lambda = grid, intercept = FALSE, thresh = threshold)
plot(cvlas.out)

##random stackoverflow says the second one is preferred!!!
##https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
bestlam = cvlas.out$lambda.min
#bestlam = cvlas.out$lambda.1se

lasso = glmnet(as.matrix(BestSubsetHardt_m[ , 1:(length(BestSubsetHardt_m)-1)]), BestSubsetHardt_m$y, alpha = 1, lambda = bestlam, family = "binomial",
               intercept = FALSE, thresh = threshold)
lasso.coef = predict(cvlas.out, type = "coefficients", s = bestlam)
lasso.coef
bestlam

cvpredLamMinTrue = predict.cv.glmnet(cvlas.out, newx = as.matrix(xcvHardRel[ , 1:(length(xcvHardRel)-1)]), type = "response", s = bestlam)
LogLoss(cvpredLamMinTrue, xcvHard$y)

#################Interaction
f <- as.formula( ~ .^2)
BestSubsetHardt_mInteraction = as.data.frame(model.matrix(f, BestSubsetHardt_m[ , 1:(length(BestSubsetHardt_m)-1)])[, -1])
xcvHardRelInteraction = as.data.frame(model.matrix(f, xcvHardRel[ , 1:(length(xcvHardRel)-1)])[, -1])

#################Ridge
lasso.modINT = glmnet(as.matrix(BestSubsetHardt_mInteraction), xt_mHard$y, family = "binomial", alpha = 0, lambda = grid
                      , intercept = FALSE, thresh = threshold)

cv.outINT = cv.glmnet(as.matrix(BestSubsetHardt_mInteraction), xt_mHard$y, alpha = 0, nfolds = 10, family = "binomial", 
                      intercept = FALSE)
plot(cv.outINT)

##random stackoverflow says the second one is preferred!!!
##https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
bestlam = cv.outINT$lambda.min
#bestlam = cv.outINT$lambda.1se

lasso = glmnet(as.matrix(BestSubsetHardt_mInteraction), xt_mHard$y, alpha = 0, lambda = bestlam, family = "binomial",
               intercept = FALSE, thresh = threshold)
lasso.coef = predict(cv.outINT, type = "coefficients", s = bestlam)
lasso.coef
bestlam

cvpredINTLamMinTrue = predict.cv.glmnet(cv.outINT, newx = as.matrix(xcvHardRelInteraction), type = "response", s = bestlam)
LogLoss(cvpredINTLamMinTrue, xcvHardRel$y)

################Lasso
lasso.modINT = glmnet(as.matrix(BestSubsetHardt_mInteraction), xt_mHard$y, family = "binomial", alpha = 1, lambda = grid
                      , intercept = FALSE, thresh = threshold)

cv.outINT = cv.glmnet(as.matrix(BestSubsetHardt_mInteraction), xt_mHard$y, alpha = 1, nfolds = 10, family = "binomial", 
                       intercept = FALSE)
plot(cv.outINT)

##random stackoverflow says the second one is preferred!!!
##https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
bestlam = cv.outINT$lambda.min
#bestlam = cv.outINT$lambda.1se

lasso = glmnet(as.matrix(BestSubsetHardt_mInteraction), xt_mHard$y, alpha = 1, lambda = bestlam, family = "binomial",
               intercept = FALSE, thresh = threshold)
lasso.coef = predict(cv.outINT, type = "coefficients", s = bestlam)
lasso.coef
bestlam

cvpredINTLamMinTrue = predict.cv.glmnet(cv.outINT, newx = as.matrix(xcvHardRelInteraction), type = "response", s = bestlam)
LogLoss(cvpredINTLamMinTrue, xcvHardRel$y)


#lambda min interaction

regLamMinInter = glm(xt_mHard$y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff + RetiredDiff 
                     + FatigueDiff  + FatigueDiffTimesBo5 + ratingdiff:ratingClaydiff 
                     + ratingClaydiff:DummyBo5TimesratingHarddiff + ratingClaydiff:WalkoverDiff 
                     + ratingHarddiff:DummyBo5TimesratingClaydiff + ratingGrassdiff:RetiredDiff 
                     + ratingGrassdiff:FatigueDiffTimesBo5 + DummyBo5TimesAvgRatingdiff:WalkoverDiff 
                     + DummyBo5TimesratingGrassdiff:FatigueDiff + DummyBo5TimesratingfClaydiff:LastHeadtoHead 
                     + RetiredDiff:FatigueDiffTimesBo5 + WalkoverDiff:RetiredOrWalkoverDiff
                     + WalkoverDiff:HeadtoHead 
                     , data = as.data.frame(BestSubsetHardt_m), family = binomial)

bestsubxcvHard = relevantVariables(xcvHard)

cvpredLamMin = predict(regLamMinInter, bestsubxcvHard, type = "response")
LogLoss(cvpredLamMin, ycvHard)

#lambda min
regLamMin = glm(y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff + RetiredDiff
          + FatigueDiff, data = BestSubsetHardt_m, family = binomial)

summary(regLamMin)

cvpredLamMin = predict(regLamMin, xcvHardRel, type = "response")
LogLoss(cvpredLamMin, xcvHardRel$y)


#regLamMin2 = glm(xt_mHard$y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff + 
#                   DummyBo5TimesratingHarddiff + RetiredDiff + FatigueDiff, 
#                 data = BestSubsetHardt_m, family = binomial)
#
#regLamMin3 = glm(xt_mHard$y ~ 0 + ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff + 
#                 DummyBo5TimesAvgRatingdiff + DummyBo5TimesratingHarddiff + DummyBo5TimesratingClaydiff +
#                   RetiredDiff + WalkoverDiff + FatigueDiff, data = BestSubsetHardt_m, family = binomial)



#cvpredLamMin2 = predict(regLamMin2, xcvHardRel, type = "response")
#LogLoss(cvpredLamMin2, ycvHard)

#cvpredLamMin3 = predict(regLamMin3, xcvHardRel, type = "response")
#LogLoss(cvpredLamMin3, ycvHard)

#lamda 1se
regLam1se = glm(xt_mHard$y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff, 
                data = BestSubsetHardt_m, family = binomial)

cvpredLam1se = predict(regLam1se, xcvHard, type = "response")
LogLoss(cvpredLam1se, ycvHard)


reg = glm(xt_mHard$y ~ 0 + ratingClaydiff + ratingHarddiff + ratingGrassdiff + 
                DummyBo5TimesAvgRatingdiff + RetiredOrWalkoverDiff
              +FatigueDiff, data = BestSubsetHardt_m, family = binomial)

BestSubsetHardt_m$y = xt_mHard$y
reg = bestglm(Xy = BestSubsetHardt_m, family = binomial, IC = "AIC", intercept = FALSE)

#Best GLM with intercept removed for this tho
reg = glm(xt_mHard$y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff, data = BestSubsetHardt_m,
          family = binomial)

summary(reg)
                         
cvpred = predict(reg, xcvHard, type = "response")
LogLoss(cvpred, ycvHard)


#Grass
BestSubsetGrasst_m = relevantVariables(xt_mGrass)
xcvGrassRel = relevantVariables(xcvGrass)

lasso.mod = glmnet(as.matrix(BestSubsetGrasst_m), yt_mGrass, family = "binomial", alpha = 1, lambda = grid
                   , intercept = FALSE)
plot(lasso.mod)
plot_glmnet(lasso.mod, xvar = "lambda")

#Lambda subset selection
cv.out = cv.glmnet(as.matrix(BestSubsetGrasst_m), yt_mGrass, alpha = 1, nfolds = 10, family = "binomial", 
                   intercept = FALSE)
plot(cv.out)

##random stackoverflow says the second one is preferred!!!
##https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
bestlam = cv.out$lambda.min
#bestlam = cv.out$lambda.1se

lasso = glmnet(as.matrix(BestSubsetGrasst_m), yt_mGrass, alpha = 1, lambda = bestlam, family = "binomial",
               intercept = FALSE)
lasso.coef = predict(cv.out, type = "coefficients", s = bestlam)
lasso.coef
bestlam

cvpredLamMinTrue = predict.cv.glmnet(cv.out, newx = as.matrix(xcvGrassRel), type = "response", s = bestlam)
LogLoss(cvpredLamMinTrue, ycvGrass)

##random stackoverflow says the second one is preferred!!!
##https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
bestlam = cv.out$lambda.min
#bestlam = cv.out$lambda.1se

#lambda min
regLamMin = glm(yt_mGrass ~ 0 + ratingdiff + ratingHarddiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff
                , data = BestSubsetGrasst_m, family = binomial)
summary(regLamMin)
#adding fatigue diff does improve the validation logloss and remove ratingHarddiff too, since it is so
#insignificant

regLamImprove = glm(y ~ 0 + ratingdiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff + FatigueDiff
                    , data = BestSubsetGrasst_m, family = binomial)
summary(regLamImprove)
#adding fatigue or not is a difficult decision, since it is still not significant at all,
#yet when you add the cv data it becomes closer so I guess I am just gonna add it

cvpredLamMin = predict(regLamMin, xcvGrassRel, type = "response")
LogLoss(cvpredLamMin, xcvGrassRel$y)

cvpredLamImprove = predict(regLamImprove, xcvGrassRel, type = "response")
LogLoss(cvpredLamImprove, ycvGrass)

#lamda 1se is the same as lamda min

BestSubsetGrasst_m$y = yt_mGrass
reg = bestglm(BestSubsetGrasst_m, family = binomial, IC = "CV", intercept = TRUE)

#Best GLM with intercept removed for this tho
#oh no, BESTGLM is retarded, not the lowest AIC if you compare the bestGLM (AIC) with regLamMin
reg = glm(yt_mGrass ~ 0 + ratingHarddiff +DummyBo5TimesAvgRatingdiff, data = BestSubsetGrasst_m,
          family = binomial)

#Best GLM  with IC = AIC seems aight tho
reg = bestglm(BestSubsetGrasst_m, family = binomial, IC = "AIC", intercept = TRUE)

reg = glm(yt_mGrass ~ 0 + ratingdiff + ratingClaydiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff, 
          data = BestSubsetGrasst_m, family = binomial)

summary(reg)

cvpred = predict(reg, xcvGrass, type = "response")
LogLoss(cvpred, ycvGrass)

#################CLAY$##############
BestSubsetClayt_m = relevantVariables(xt_mClay)
xcvClayRel = relevantVariables(xcvClay)

#interaction
f <- as.formula( ~ .^2)

BestSubsetClayt_mInteraction = as.data.frame(model.matrix(f, BestSubsetClayt_m)[, -1])
xcvClayRelInteraction = as.data.frame(model.matrix(f, xcvClayRel)[, -1])

grid = 10 ^ seq(1, -5, length = 600)

lasso.mod = glmnet(as.matrix(BestSubsetClayt_m), yt_mClay, family = "binomial", alpha = 1, lambda = grid
                   , intercept = FALSE)
plot(lasso.mod)
plot_glmnet(lasso.mod, xvar = "lambda")

#Lambda subset selection
cv.out = cv.glmnet(as.matrix(BestSubsetClayt_m), yt_mClay, alpha = 1, nfolds = 10, family = "binomial", 
                   intercept = FALSE)
plot(cv.out)

##random stackoverflow says the second one is preferred!!!
##https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
bestlam = cv.out$lambda.min
#bestlam = cv.out$lambda.1se

lasso = glmnet(as.matrix(BestSubsetClayt_m), yt_mClay, alpha = 1, lambda = bestlam, family = "binomial",
               intercept = FALSE)
lasso.coef = predict(cv.out, type = "coefficients", s = bestlam)
lasso.coef
bestlam

cvpredLamMinTrue = predict.cv.glmnet(cv.out, newx = as.matrix(xcvClayRel), type = "response", s = bestlam)
LogLoss(cvpredLamMinTrue, ycvClay)

lasso.modINT = glmnet(as.matrix(BestSubsetClayt_mInteraction), yt_mClay, family = "binomial", alpha = 1, lambda = grid
                      , intercept = FALSE)

cv.outINT = cv.glmnet(as.matrix(BestSubsetClayt_mInteraction), yt_mClay, alpha = 1, nfolds = 10, family = "binomial", 
                      intercept = FALSE)
plot(cv.outINT)

#lambda min
regLamMin = glm(y ~ 0 + ratingdiff + ratingClaydiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff +
                  DummyBo5TimesratingClaydiff + WalkoverDiff + RetiredOrWalkoverDiff 
                + FatigueDiff + HeadtoHead + LastHeadtoHead, data = BestSubsetClayt_m, family = binomial)

#regLamMin2 = glm(xt_mHard$y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff + 
#                   DummyBo5TimesratingHarddiff + RetiredDiff + FatigueDiff, 
#                 data = BestSubsetHardt_m, family = binomial)
#
#regLamMin3 = glm(xt_mHard$y ~ 0 + ratingdiff + ratingClaydiff + ratingHarddiff + ratingGrassdiff + 
#                 DummyBo5TimesAvgRatingdiff + DummyBo5TimesratingHarddiff + DummyBo5TimesratingClaydiff +
#                   RetiredDiff + WalkoverDiff + FatigueDiff, data = BestSubsetHardt_m, family = binomial)


summary(regLamMin)

cvpredLamMin = predict(regLamMin, xcvClayRel, type = "response")
LogLoss(cvpredLamMin, xcvClayRel$y)
