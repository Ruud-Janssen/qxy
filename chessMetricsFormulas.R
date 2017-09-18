calculateChessMetric <- function(previousGames, row, maxDays, weightReducedPer30Days, winner, surface = FALSE) {
  wrd <- weightReducedPer30Days
  rGames <- getGamesLastXDays(previousGames, row$Date, maxDays)
  
  if (surface == TRUE) {
    rGames <- rGames[rGames$Surface == row$Surface, ]
  } 
  
  if (winner == TRUE) {
    idPlayer <- row$idWinner
  } else {
    idPlayer <- row$idLoser
  }
  
  WonIndexes  <- grep(idPlayer, rGames$idWinner)
  LostIndexes <- grep(idPlayer, rGames$idLoser)
  
  df <- "%Y-%m-%d"
  
  weightsWon   <- as.numeric(1 - wrd * (as.Date(rGames$Date[WonIndexes], df) - as.Date(row$Date, df)) / 30)
  weightsLost  <- as.numeric(1 - wrd * (as.Date(rGames$Date[LostIndexes], df) - as.Date(row$Date, df)) / 30)
  
  if(length(weightsWon) == 0 & length(weightsLost) == 0) {
    return(NA)
  }
  
  avgRatingOpponents <- 
    mean(c(rGames$Loser_rating[WonIndexes] * weightsWon, 
           rGames$Winner_rating[LostIndexes] * weightsLost) / (sum(weightsWon) + sum(weightsLost)))
  percentWon         <- sum(weightsWon) / (sum(weightsWon) + sum(weightsLost))
  
  avgRatingOpponents + (percentWon - 0.5) * 850
}

getGamesLastXDays <- function(Games, currentDate, maxdays) {
  df = "%Y-%m-%d"
  Mindate = as.Date(as.character(currentDate), format = df ) - maxdays
  Games[Games$Date >= Mindate & Games$Date <= currentDate, ]
}