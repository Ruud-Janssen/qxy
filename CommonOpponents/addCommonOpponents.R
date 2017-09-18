library(tictoc)
source("formulas.R")
source("CommonOpponents/addCommonOpponentsFormulas.R")
#Parallel
library(parallel)
library(foreach)
library(doParallel)
library(dplyr)

cl <- makeCluster(detectCores() - 4)
registerDoParallel(cl, cores = detectCores() - 4)

#HyperParameters
maxdays <- 365

allGames <- getAllGamesWithoutRating()
allGames <- InitializeCommonOpponentsVariables(allGames)
allGames <- allGames %>% mutate(COQualified = NrGames > 10, 
                                COPointsQualified = NrGames > 10 & !is.na(w_svpt) & !is.na(l_svpt))

Nstart = nrow(read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = TRUE, fill = TRUE))
Nall   = nrow(allGames)

allGames <- allGames %>% mutate(indexNumber = seq(nrow(allGames)))
tic()

allGames[Nstart : Nall, ] = foreach(i = Nstart : Nall, .combine = rbind, .packages = "dplyr") %dopar% {
#for(i in Nstart : Nall){
  source("CommonOpponents/addCommonOpponentsFormulas.R")                          
  coi     <- getCommonOpponentIndexes(allGames[1 : (i - 1), ], allGames[i, ], maxdays)
  weights <- calculateWeights(allGames, coi)
  
  if(length(coi$WinnerWon) > 0 | length(coi$WinnerLost) > 0) {
    allGames$Winner_COPercentMatchesWon[i] <- 
      sum(weights$WinnerWon) / sum(c(weights$WinnerWon, weights$WinnerLost))
    allGames$Winner_COPercentSetsWon[i]    <- 
      calculatePercentSetsWon(allGames, coi$WinnerWon, coi$WinnerLost, weights$WinnerWon, weights$WinnerLost)
    allGames$Winner_COPercentGamesWon[i]   <- 
      calculatePercentGamesWon(allGames, coi$WinnerWon, coi$WinnerLost, weights$WinnerWon, weights$WinnerLost)
    allGames$Winner_COGames[i]             <- length(coi$WinnerWon) + length(coi$WinnerLost)
    
    allGames$Loser_COPercentMatchesWon[i] <- 
      sum(weights$LoserWon) / sum(c(weights$LoserWon, weights$LoserLost))
    allGames$Loser_COPercentSetsWon[i]    <- 
      calculatePercentSetsWon(allGames, coi$LoserWon, coi$LoserLost, weights$LoserWon, weights$LoserLost)
    allGames$Loser_COPercentGamesWon[i]   <- 
      calculatePercentGamesWon(allGames, coi$LoserWon, coi$LoserLost, weights$LoserWon, weights$LoserLost)
    allGames$Loser_COGames[i]             <- length(coi$LoserWon) + length(coi$LoserLost)
    
    if(length(coi$WinnerWonPoints) > 0 | length(coi$WinnerLostPoints) > 0) {
    allGames$Winner_COPercentPointsWon[i]  <- 
      calculatePercentPointsWon(allGames, coi$WinnerWonPoints, coi$WinnerLostPoints, 
                                weights$WinnerWonPoints, weights$WinnerLostPoints)
    
    allGames$Loser_COPercentPointsWon[i]  <- 
      calculatePercentPointsWon(allGames, coi$LoserWonPoints, coi$LoserLostPoints, 
                                weights$LoserWonPoints, weights$LoserLostPoints)
    }
    
    #And now for surface
    coi <- getCommonOpponentIndexes(allGames[1 : (i - 1), ], allGames[i, ], maxdays, surface = TRUE)
    weights <- calculateWeights(allGames, coi)
    if(length(coi$WinnerWon) > 0 | length(coi$WinnerLost) > 0) {
      allGames$Winner_COPercentMatchesThisSurfaceWon[i] <- 
        sum(weights$WinnerWon) / sum(c(weights$WinnerWon, weights$WinnerLost))
      allGames$Winner_COPercentSetsThisSurfaceWon[i]    <- 
        calculatePercentSetsWon(allGames, coi$WinnerWon, coi$WinnerLost, weights$WinnerWon, weights$WinnerLost)
      allGames$Winner_COPercentGamesThisSurfaceWon[i]   <- 
        calculatePercentGamesWon(allGames, coi$WinnerWon, coi$WinnerLost, weights$WinnerWon, weights$WinnerLost)
      allGames$Winner_COThisSurfaceGames[i]             <- length(coi$WinnerWon) + length(coi$WinnerLost)
      
      allGames$Loser_COPercentMatchesThisSurfaceWon[i] <- 
        sum(weights$LoserWon) / sum(c(weights$LoserWon, weights$LoserLost))
      allGames$Loser_COPercentSetsThisSurfaceWon[i]    <- 
        calculatePercentSetsWon(allGames, coi$LoserWon, coi$LoserLost, weights$LoserWon, weights$LoserLost)
      allGames$Loser_COPercentGamesThisSurfaceWon[i]   <- 
        calculatePercentGamesWon(allGames, coi$LoserWon, coi$LoserLost, weights$LoserWon, weights$LoserLost)
      allGames$Loser_COThisSurfaceGames[i]             <- length(coi$LoserWon) + length(coi$LoserLost)
      
      if(length(coi$WinnerWonPoints) > 0 | length(coi$WinnerLostPoints) > 0) {
        allGames$Winner_COPercentPointsThisSurfaceWon[i]  <- 
          calculatePercentPointsWon(allGames, coi$WinnerWonPoints, coi$WinnerLostPoints, 
                                    weights$WinnerWonPoints, weights$WinnerLostPoints)
        
        allGames$Loser_COPercentPointsThisSurfaceWon[i]  <- 
          calculatePercentPointsWon(allGames, coi$LoserWonPoints, coi$LoserLostPoints, 
                                    weights$LoserWonPoints, weights$LoserLostPoints)
      }
    }
  }
  return(allGames[i, ])
}

toc()
stopCluster(cl)

saveDatasetsWithoutRating(allGames)
