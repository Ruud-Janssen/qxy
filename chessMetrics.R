rm(list = ls())
source("formulas.R")
source("chessMetricsFormulas.R")
library(tictoc)
library(parallel)
library(foreach)
library(doParallel)

cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = detectCores() - 3)

maxDays                <- 360
weightReducedPer30Days <- 0.08

allGames <- getAllGamesWithRating()

allGames$Winner_tpr     <- NA
allGames$Loser_tpr      <- NA
allGames$Winner_tprHard <- NA
allGames$Loser_tprHard  <- NA

Nstart <- nrow(read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = TRUE, fill = TRUE))
Nall   <- nrow(allGames)

tic()

allGames[Nstart : Nall, ] = foreach(i = Nstart : Nall, .combine = rbind) %dopar% {
#for (i in Nstart: Nall) {
  allGames$Winner_tpr[i] <- 
    calculateChessMetric(allGames[1 : (i - 1), ], allGames[i, ], maxDays, weightReducedPer30Days, winner = TRUE)
  allGames$Loser_tpr[i] <- 
    calculateChessMetric(allGames[1 : (i - 1), ], allGames[i, ], maxDays, weightReducedPer30Days, winner = FALSE)
  
  allGames$Winner_tprHard[i] <- 
    calculateChessMetric(allGames[1 : (i - 1), ], allGames[i, ], maxDays, weightReducedPer30Days, winner = TRUE,
                         surface = TRUE)
  allGames$Loser_tprHard[i] <- 
    calculateChessMetric(allGames[1 : (i - 1), ], allGames[i, ], maxDays, weightReducedPer30Days, winner = FALSE,
                         surface = TRUE)
  
  return(allGames[i, ])
}

toc()
stopCluster(cl)

saveDatasetsWithRating(allGames, NULL)
