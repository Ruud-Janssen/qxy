rm(list = ls())
source("formulas.R")
source("hyperparametersfunctions.R")
source("bestsubsetsformulas.R")
#install.packages('devtools')
#library(devtools)
#install_github("gbm-developers/gbm3")
library(gbm3)
library(ggplot2)


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
quantileHard = quantile(xt_mHard$UncertaintySurface, q / 100)
xt_mHard = removeUncertainMatches(xt_mHard, quantileHard, "Surface")

xcvHard = getXThisSurface(xcv, "Hard")
xcvHard = removeUncertainMatches(xcvHard, quantileHard, "Surface")

xtmHardRel = relevantVariables(xt_mHard)
xcvHardRel = relevantVariables(xcvHard)

for(i in 1 : 10) {

  f = y ~ 0 + ratingdiff + ratingHarddiff + Bo5 + FatigueDiff + HomeDiff + RetiredDiff + LeftieDiff
  
  train_params <- training_params(num_trees = 5000,
                                     shrinkage = 0.00175,
                                     bag_fraction = 0.5,
                                     num_train = nrow(xtmHardRel) / 2,
                                     id=seq_len(nrow(xtmHardRel)),
                                     min_num_obs_in_node = 10,
                                     interaction_depth = 3,
                                     num_features = 2)
  
  gbm1 = gbmt(formula = f, data = xtmHardRel, distribution=gbm_dist("Bernoulli"), train_params = train_params, 
              keep_gbm_data = TRUE)
  plot(gbm1$valid.error)
  min(gbm1$valid.error)
  
  y = as.numeric(xcvHardRel$y)
  xcvHardRel$y = -20
  
  p = predict(gbm1, xcvHardRel, n.trees = 3000, type = "response")
  
  xcvHardRel$y = y
  xcvH
  print(LogLoss(p, xcvHardRel$y))
  print(mean((p > 0.5) == xcvHardRel$y))
}


#LogLoss(cvpredRating, xcvHardRel$y)
#[1] 0.5408071

#mean(abs(xcvHardRel$y - cvpredRating))
#[1] 0.3708295

#mean((xcvHardRel$y - cvpredRating)^2)
#[1] 0.1877846



