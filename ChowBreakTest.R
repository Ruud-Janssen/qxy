rm(list = ls())
source("formulas.r")
source("hyperparametersfunctions.r")
source("bestsubsetsformulas.r")
library(leaps)
library(bestglm)
library(glmnet)
library(plotmo)
library(glmulti)
library(MASS)
library(qpcR)

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

xt_mHard = relevantVariables(xt_mHard)
xcvHard = relevantVariables(xcvHard)

TotalX = rbind(xt_mHard, xcvHard)
#time for a good old chowbreaktest
#CONCLUSIONS PROBABLY SUCK BECAUSE CHOW BREAK TEST IS FOR LINEAR REGRESSION YO
split = 1/2

dataHardFirst = TotalX[1 : floor(split  * nrow(TotalX)), ]
dataHardLast = TotalX[( 1 + floor(split * nrow(TotalX))):nrow(TotalX), ]

regTotal =  glm(y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
                  RetiredDiff + FatigueDiff + ThisBoxSkillDiffPlusScores 
                , data = TotalX, family = binomial)

regHardFirst = glm(y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
                     RetiredDiff + FatigueDiff + ThisBoxSkillDiffPlusScores 
                   , data = dataHardFirst, family = binomial)
regHardLast = glm(y ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
                    RetiredDiff + FatigueDiff + ThisBoxSkillDiffPlusScores
                  , data = dataHardLast, family = binomial)

#Chow
Sc = RSS(regTotal)
S1 = RSS(regHardFirst)
S2 = RSS(regHardLast)

k = regTotal$rank

N1 = nrow(dataHardFirst)
N2 = nrow(dataHardLast)

CB = (Sc - (S1 + S2)) / k /((S1+ S2)/(N1 + N2 - 2 * k))
print(CB)
qf(0.95, k, N1 + N2 - 2*k)

#Wald
diffB = (regHardFirst$coefficients-regHardLast$coefficients)
diffBHomeDiff = c(0,0,0,0,0,diffB[6])
 

inversesumvar = ginv(vcov(regHardFirst) + vcov(regHardLast)) 
WaldT = t(diffBHomeDiff) %*% inversesumvar %*% diffBHomeDiff
WaldT
qchisq(0.95, df=1)

