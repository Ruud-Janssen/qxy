rm(list = ls())
source("formulas.R")

calculatePercentGamesWonWinner <- function(row) {
  winnerWonGames  <- sum(c(row$W1, row$W2, row$W3, row$W4, row$W5), na.rm = T)
  winnerLostGames <- sum(c(row$L1, row$L2, row$L3, row$L4, row$L5), na.rm = T)
  winnerWonGames / (winnerWonGames + winnerLostGames)
}

calculatePercentPointsWonWinner <- function(row) {
  pointsWon   <- (row$w_1stWon + row$w_2ndWon) + (row$l_svpt - row$l_1stWon - row$l_2ndWon)
  totalPoints <- row$w_svpt + row$l_svpt
  pointsWon / totalPoints
}

allGames <- getAllGamesWithoutRating()
Nall <- nrow(allGames)

allGames$Winner_percentsetswon   <- NA
allGames$Winner_percentgameswon  <- NA
allGames$Winner_percentpointswon <- NA

for(i in 1 : nrow(allGames)) {
  if(calculateGames(allGames[i, ]) <= 10) {
    next()
  }
  
  allGames$Winner_percentsetswon[i]  <- allGames$W1set[i] / (allGames$W1set[i] + allGames$L1set[i])
  allGames$Winner_percentgameswon[i] <- calculatePercentGamesWonWinner(allGames[i, ])
  
  if(is.na(allGames$w_svpt[i]) | is.na(allGames$l_svpt[i])) {
    next()
  }
  allGames$Winner_percentpointswon[i] <- calculatePercentPointsWonWinner(allGames[i, ])
}

saveDatasetsWithoutRating(allGames)