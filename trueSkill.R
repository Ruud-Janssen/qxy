library(tidyverse)
rm(list = ls())

v <- function(t) {
  dnorm(t) / pnorm(t)
}

w <- function(t) {
  v(t) * (v(t) + t)
}

updateMu <- function (muPlayer, muOpponent, varPlayer, c, result) {
  muPlayer + result * (varPlayer / c) * v(result * (muPlayer - muOpponent) / c)
} 

updateVar <- function (muPlayer, muOpponent, varPlayer, c, result) {
  varPlayer * (1 - varPlayer / c ^ 2 * w(result * (muPlayer - muOpponent) / c))
}

ComputeC <- function(B, var1, var2) {
  sqrt(2 * B ^ 2 + var1 + var2)
}

source("formulas.R")

getTrueSkill <- function(var, varHard) {

  train_rating = read.table("Data/datasets/train_ratingWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)
  train_model = read.table("Data/datasets/train_modelWithRatings.csv"
                           , header = T, sep = ",", quote = "\"", fill = TRUE)
  
  Nt_r = nrow(train_rating)
  Nt_m = nrow(train_model)
  
  Ntot = Nt_r + Nt_m
  allGames = rbind(train_rating, train_model)
  
  player   <- getPlayers()
  
  # Create Ratings for all players, ratings are adapted after each match
  trueSkill <- player

  #Add start rating and number of games
  trueSkill <- trueSkill %>% mutate(mu = 25,
                                    var = var,
                                    muHard = 25,
                                    varHard = varHard)
  
  allGames <- allGames %>% mutate(Winner_trueSkill = NA,
                                  Winner_trueSkillHard = NA,
                                  Loser_trueSkill = NA,
                                  Loser_trueSkillHard = NA)  
  
  B     <- 25 / 3 / 2           #sqrt(var / 2)
  BHard <- 25 / 3 / 2       #sqrt(varHard / 2)
  
  for(i in 1 : nrow(allGames)) {
    row_nr_winner <- which(trueSkill$id == allGames$idWinner[i])
    row_nr_loser  <- which(trueSkill$id == allGames$idLoser[i])
    
    c <- ComputeC(B, trueSkill$var[row_nr_winner], trueSkill$var[row_nr_loser])
    allGames$Winner_trueSkill[i] <- trueSkill$mu[row_nr_winner] / c
    allGames$Loser_trueSkill[i]  <- trueSkill$mu[row_nr_loser] / c
    
    cHard <- ComputeC(BHard, trueSkill$varHard[row_nr_winner], trueSkill$varHard[row_nr_loser])
    
    allGames$Winner_trueSkillHard[i] <- trueSkill$muHard[row_nr_winner] / cHard
    allGames$Loser_trueSkillHard[i]  <- trueSkill$muHard[row_nr_loser] / cHard
    
    ##update trueskill
    muWinner  <- trueSkill$mu[row_nr_winner]
    muLoser   <- trueSkill$mu[row_nr_loser]
    varWinner <- trueSkill$var[row_nr_winner]
    varLoser  <- trueSkill$var[row_nr_loser]
    
    trueSkill$mu[row_nr_winner]  <- updateMu(muWinner, muLoser, varWinner, c, 1)
    trueSkill$mu[row_nr_loser]   <- updateMu(muLoser, muWinner, varLoser, c, -1)
    trueSkill$var[row_nr_winner] <- updateVar(muWinner, muLoser, varWinner, c, 1)
    trueSkill$var[row_nr_loser]  <- updateVar(muLoser, muWinner, varLoser, c, -1)
    
    if(allGames$Surface[i] == "Hard") {
      muWinner  <- trueSkill$muHard[row_nr_winner]
      muLoser   <- trueSkill$muHard[row_nr_loser]
      varWinner <- trueSkill$varHard[row_nr_winner]
      varLoser  <- trueSkill$varHard[row_nr_loser]
      
      trueSkill$muHard[row_nr_winner]  <- updateMu(muWinner, muLoser, varWinner, cHard, 1)
      trueSkill$muHard[row_nr_loser]   <- updateMu(muLoser, muWinner, varLoser, cHard, -1)
      trueSkill$varHard[row_nr_winner] <- updateVar(muWinner, muLoser, varWinner, cHard, 1)
      trueSkill$varHard[row_nr_loser]  <- updateVar(muLoser, muWinner, varLoser, cHard, -1)
    }
  }
  return(allGames[(Nt_r + 1) : Ntot, ])
}

source("hyperparametersfunctions.r")
library(leaps)
library(bestglm)
library(tictoc)

#Parallel
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = (detectCores() - 3))

Nt = nrow(read.table("Data/datasets/train_modelWithRatings.csv"
                     , header = T, sep = ",", quote = "\"", fill = TRUE))
set.seed(42)
yt_m = as.numeric(runif(Nt, 0, 1) > 0.5)

tic()
total = foreach (s1 = 1 : 10, .combine = rbind) %do% {
  varGeneral <- 0.5 * s1

  return(foreach(s2 = 1 : 8, .packages = c("leaps","bestglm", "plyr"), .combine = rbind) %dopar% {
    varHardSurface <- 0.3 * s2
    source("hyperparametersfunctions.r")
    
    train_modelwithRatings <- getTrueSkill(varGeneral, varHardSurface)
    train_modelwithRatings$Winner_rating     <- train_modelwithRatings$Winner_trueSkill
    train_modelwithRatings$Loser_rating      <- train_modelwithRatings$Loser_trueSkill
    train_modelwithRatings$Winner_ratingHard <- train_modelwithRatings$Winner_trueSkillHard
    train_modelwithRatings$Loser_ratingHard  <- train_modelwithRatings$Loser_trueSkillHard
    
    xt_m = regressorvariables(yt_m, train_modelwithRatings)
    
    df <- "%Y-%m-%d"
    lastGame2011 <- as.Date("2011-12-31", df)
    
    xTrain = xt_m[as.Date(xt_m$Date, df) <= lastGame2011, ]
    xValidation = xt_m[as.Date(xt_m$Date, df) > lastGame2011, ]

    results <- data.frame(p = rep(NA, 20), 
                          LogLossOutOfSample = rep(0, 20), 
                          LogLossOutOfSampleCDF = rep(NA, 20))

    
    for(q in 1 : 20) {
      
      quantile = quantile(xTrain$Uncertainty, (q + 19) / 100)
      
      xTraincurrent = removeUncertainMatches(xTrain, quantile, "")
      xTraincurrentHard = getXThisSurface(xTraincurrent, "Hard")
      
      xValidationCurrent = removeUncertainMatches(xValidation, quantile, "")
      xValidationCurrentHard = getXThisSurface(xValidationCurrent, "Hard")
      
      RegHard = glm(y ~ 0 + ratingdiff + ratingHarddiff, data = xTraincurrentHard, family = binomial)
      
      p = pnorm(xValidationCurrentHard$ratingdiff)
      
      validationPredHard = predict(RegHard, xValidationCurrentHard, type = "response")
      results$LogLossOutOfSample[q] = LogLoss(actual = xValidationCurrentHard$y, predicted = validationPredHard)
      results$LogLossOutOfSampleCDF[q] = LogLoss(actual = xValidationCurrentHard$y, predicted = p)
    }
    return(c(varGeneral, varHardSurface, mean(results$LogLossOutOfSample), mean(results$LogLossOutOfSampleCDF)))
  })
  #})
}
stopCluster(cl)
toc()

results <- data.frame(var = total[, 1], varHard = total[, 2], LogLoss = total[, 3], LogLossCdf = total[, 4])
g1 <- ggplot(results, aes(x = var, y = varHard, z = LogLoss)) + geom_raster(aes(fill = LogLoss)) +
  geom_contour(colour = "white", bins = 10)

g2 <- ggplot(results, aes(x = var, y = varHard, z = LogLossCdf)) + geom_raster(aes(fill = LogLossCdf)) +
  geom_contour(colour = "white", bins = 10)


library(gridExtra)
grid.arrange(g1, g2)

