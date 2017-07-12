rm(list = ls())
source("formulas.r")
source("hyperparametersfunctions.r")
source("BetSizingFormulas.r")
source("bestsubsetsformulas.r")
library(leaps)
library(bestglm)
library(glmnet)
library(boot)
library(MASS)

train_modelwithRatings = read.table("Data/datasets/train_modelWithRatings.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)
cv_withRatings = read.table("Data/datasets/testWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)

cv_withRatings = cv_withRatings[!is.na(cv_withRatings$Best.of), ]

xtrain = rbind(train_modelwithRatings, cv_withRatings)

rm(train_modelwithRatings, cv_withRatings)

test = read.table("Data/datasets/testWithRatings.csv"
                  , header = T, sep = ",", quote = "\"", fill = TRUE)

test = test[!is.na(test$Best.of), ]

Ntrain = nrow(xtrain)
Ntest = nrow(test)

set.seed(42)
ytrain = as.numeric(runif(Ntrain, 0, 1) > 0.5)
ytest = as.numeric(runif(Ntest, 0, 1) > 0.5)

xtrain = regressorvariables(ytrain, xtrain)

q = 27
quantile = quantile(xtrain$Uncertainty, q / 100)

xtrain = removeUncertainMatches(xtrain, quantile)

xtrainHard = getXThisSurface(xtrain, "Hard")
xtrainGrass = getXThisSurface(xtrain, "Grass")

xtrainHard$ImpProb = xtrainHard$PSLthisplayer/(xtrainHard$PSLthisplayer + xtrainHard$PSWthisplayer)
xtrainGrass$ImpProb = xtrainGrass$PSLthisplayer/(xtrainGrass$PSLthisplayer + xtrainGrass$PSWthisplayer)

regHard = glm(y ~ 0 + ratingdiff + ratingHarddiff +
                DummyBo5TimesAvgRatingdiff     
                + RetiredDiff + FatigueDiff#+ HeadtoHeadPercentageWeightedsqN #this one even better
              #+ HeadtoHead  #appears to improve the loglikelihood, so maybe interesting, also improve
              #BR a lot but doesn't work for grass
                , data = xtrainHard, family = binomial)

regGrass = glm(y ~ 0 + ratingdiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff 
               +FatigueDiff 
                 , data = xtrainGrass, family = binomial)

xtest = regressorvariables(ytest, test)
xtest = removeUncertainMatches(xtest, quantile)

xtestHard = getXThisSurface(xtest, "Hard")
xtestGrass = getXThisSurface(xtest, "Grass")

xtestHard$ImpProb = xtestHard$PSLthisplayer/(xtestHard$PSLthisplayer + xtestHard$PSWthisplayer)
xtestGrass$ImpProb = xtestGrass$PSLthisplayer/(xtestGrass$PSLthisplayer + xtestGrass$PSWthisplayer)

iHard = !is.na(xtestHard$ImpProb)
iGrass = !is.na(xtestGrass$ImpProb)

xtestHard = xtestHard[iHard, ]
xtestGrass = xtestGrass[iGrass, ]

testpredHard = predict(regHard, xtestHard, type = "response")
LogLoss(testpredHard, xtestHard$y)

testpredGrass = predict(regGrass, xtestGrass, type = "response")



#predictionsHard = 0.4314 * testpredHard + 0.5790 * xtestHard$ImpProb
#predictionsGrass =  0.8582 * testpredGrass + 0.1607 * xtestGrass$ImpProb

#This is kind a random for Hard, but it seems to go to infinity so whatever?
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#predictionsHard = 0.85 * testpredHard  + 0.15 * xtestHard$ImpProb
#predictionsGrass = 0.85 * testpredGrass + 0.15 * xtestGrass$ImpProb

predictionsHard = 0.2 * testpredHard + 0.8 * xtestHard$ImpProb
predictionsGrass = 0.2 *testpredGrass + 0.8 * xtestGrass$ImpProb

combinedResultsHard = result(predictionsHard, xtestHard, xtestHard$y)
combinedResultsGrass = result(predictionsGrass, xtestGrass, xtestGrass$y)

resultsHard = combinedResultsHard$results
resultsGrass = combinedResultsGrass$results

betsHard = combinedResultsHard$bets
betsGrass = combinedResultsGrass$bets

betsHard$bet = replace(betsHard$bet, betsHard$bet == 0, -500)
betsHard$result = replace(betsHard$result, betsHard$result == 0, -500)

plot(1 : length(betsHard$br), betsHard$br, ylim = c(-30, 60), "l")
points(betsHard$bet, col = "red")
points(betsHard$result, col = "green")

betsGrass$bet = replace(betsGrass$bet, betsGrass$bet == 0, -500)
betsGrass$result = replace(betsGrass$result, betsGrass$result == 0, -500)

plot(1 : length(betsGrass$br), betsGrass$br, ylim = c(-20, 20), "l")
points(betsGrass$bet, col = "red")
points(betsGrass$result, col = "green")

getLogLoss = function(data, index){
  
  predictionsB = data[index, 1]
  ytestB = data[index, 2]
  
  return(LogLoss(predictionsB, ytestB))
}

dataHard = as.data.frame(cbind(predictionsHard, ytestHard))
bootHard = boot(data = dataHard, statistic = getLogLoss, R = 1000)
upperBoundHard = 0.5486395 + 1.96 * 0.01533176
lowerBoundHard = 0.5486395 - 1.96 * 0.01533176

dataGrass = as.data.frame(cbind(predictionsGrass, ytestGrass))
bootGrass = boot(data = dataGrass, statistic = getLogLoss, R = 1000)
upperBoundGrass = 0.5541796 + 1.96 * 0.05029078
lowerBoundGrass = 0.5541796 - 1.96 * 0.05029078

getLogLoss2Hard = function(data, index){

  xtrainB = data[index, ]
  ytrainB = data$ytrainHard[index]
  
  regB = glm(ytrainB ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
                  RetiredDiff + FatigueDiff, data = xtrainB, family = binomial)
  
  testpredB = predict(regB, xtestHard, type = "response")
  predictionsB = 0.85 * testpredB  + 0.15 * xtestHard$ImpProb
 
  return(LogLoss(predictionsB, ytestHard))
}

getLogLoss2Grass = function(data, index){
  
  xtrainB = data[index, ]
  ytrainB = data$ytrainGrass[index]
  
  regB = glm(ytrainB ~ 0 + ratingdiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff +
               FatigueDiff, data = xtrainB, family = binomial)
  
  testpredB = predict(regB, xtestGrass, type = "response")
  predictionsB = 0.85 * testpredB  + 0.15 * xtestGrass$ImpProb
  
  return(LogLoss(predictionsB, ytestGrass))
}

dataHard2 = as.data.frame(cbind(xtrainHard, ytrainHard))
bootHard2 = boot(data = dataHard2, statistic = getLogLoss2Hard, R = 1000)
upperBoundHard = 0.5486395 + 1.96 * 0.0008083271
lowerBoundHard = 0.5486395 - 1.96 * 0.0008083271  

dataGrass2 = as.data.frame(cbind(xtrainGrass, ytrainGrass))
bootGrass2 = boot(data = dataGrass2, statistic = getLogLoss2Grass, R = 1000)
upperBoundGrass = 5541796 + 1.96 * 0.003812915
lowerBoundGrass = 5541796 - 1.96 * 0.003812915

#only the most recent half data
dataHard3 = dataHard2[floor(nrow(dataHard2)/2):nrow(dataHard2), ]
dataGrass3 = dataGrass2[floor(nrow(dataGrass2)/2):nrow(dataGrass2), ]

bootHard3 = boot(data = dataHard3, statistic = getLogLoss2Hard, R = 1000)
bootGrass3 = boot(data = dataGrass3, statistic = getLogLoss2Grass, R = 1000)

#time for a good old chowbreaktest
#CONCLUSIONS PROBABLY SUCK BECAUSE CHOW BREAK TEST IS FOR LINEAR REGRESSION YO
dataHardFirst = dataHard2[1 : floor(nrow(dataHard2)/2), ]
dataHardLast = dataHard2[floor(nrow(dataHard2)/2):nrow(dataHard2), ]

regHardFirst = glm(ytrainHard ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
                    RetiredDiff + FatigueDiff, data = dataHardFirst, family = binomial)
regHardLast = glm(ytrainHard ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
             RetiredDiff + FatigueDiff, data = dataHardLast, family = binomial)


diffB = (regHardFirst$coefficients-regHardLast$coefficients)
inversesumvar = ginv(vcov(regHardFirst) + vcov(regHardLast)) 
WaldT = t(diffB) %*% inversesumvar %*% diffB
qchisq(0.95, df=5)

#WaldT is a lot smaller than the criticial value so probably no break in hard

dataGrassFirst = dataGrass2[1 : floor(nrow(dataGrass2)/2), ]
dataGrassLast = dataGrass2[floor(nrow(dataGrass2)/2):nrow(dataGrass2), ]

regGrassFirst = glm(ytrainGrass ~ 0 + ratingdiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff +
                      FatigueDiff, data = dataGrassFirst, family = binomial)
regGrassLast = glm(ytrainGrass ~ 0 + ratingdiff + ratingGrassdiff + DummyBo5TimesAvgRatingdiff +
                     FatigueDiff, data = dataGrassLast, family = binomial)


diffB = (regGrassFirst$coefficients-regGrassLast$coefficients)
inversesumvar = ginv(vcov(regGrassFirst) + vcov(regGrassLast)) 
WaldT = t(diffB) %*% inversesumvar %*% diffB
qchisq(0.95, df=5)
#WaldT is a lot smaller than the criticial value so probably no break in grass

#lets do a breaktest between hard and grass for fun/checking whether it works
regHard = glm(ytrainHard ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
                     RetiredDiff + FatigueDiff, data = dataHard2, family = binomial)
regGrass =  glm(ytrainGrass ~ 0 + ratingdiff + ratingHarddiff + DummyBo5TimesAvgRatingdiff +    
                     RetiredDiff + FatigueDiff, data = dataGrass2, family = binomial)
diffB = (regHard$coefficients-regGrass$coefficients)
inversesumvar = ginv(vcov(regHard) + vcov(regGrass)) 
WaldT = t(diffB) %*% inversesumvar %*% diffB
qchisq(0.95, df=5)

#Bootstrap roi
getROI = function(data, index){
  xtestB = data[index, 1 : (length(data) - 2)]
  ytestB = data[index, (length(data) - 1)]
  predictionsB = data[index,length(data)]
  
  combinedResultsB = result(predictionsB, xtestB, ytestB)
  resultsB = combinedResultsB$results
  return(resultsB$ROI)
}

dataHardForROI = cbind(xtestHard, ytestHard, predictionsHard)
bootROIHard = boot(data = dataHardForROI, statistic = getROI, R = 1000)

dataGrassForROI = cbind(xtestGrass, ytestGrass, predictionsGrass)
bootROIGrass = boot(data = dataGrassForROI, statistic = getROI, R = 1000)
