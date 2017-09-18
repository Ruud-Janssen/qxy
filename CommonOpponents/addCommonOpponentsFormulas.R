InitializeCommonOpponentsVariables = function(Games) {
  Games %>%  mutate(Winner_COPercentMatchesWon = NA, 
                    Winner_COPercentSetsWon   = NA,
                    Winner_COPercentGamesWon  = NA, 
                    Winner_COPercentPointsWon = NA,
                    Winner_COGames            = 0, 
                    
                    Loser_COPercentMatchesWon = NA,
                    Loser_COPercentSetsWon    = NA, 
                    Loser_COPercentGamesWon   = NA, 
                    Loser_COPercentPointsWon  = NA,
                    Loser_COGames             = 0,
                    
                    Winner_COPercentMatchesThisSurfaceWon = NA, 
                    Winner_COPercentSetsThisSurfaceWon    = NA, 
                    Winner_COPercentGamesThisSurfaceWon   = NA, 
                    Winner_COPercentPointsThisSurfaceWon  = NA, 
                    Winner_COThisSurfaceGames             = 0,
                    
                    Loser_COPercentMatchesThisSurfaceWon = NA,
                    Loser_COPercentSetsThisSurfaceWon    = NA,
                    Loser_COPercentGamesThisSurfaceWon   = NA, 
                    Loser_COPercentPointsThisSurfaceWon  = NA,
                    Loser_COThisSurfaceGames             = 0)
}

getCommonOpponentIndexes <- function(previousGames, row, maxdays, surface = FALSE) {
  rGames <- previousGames %>% filter(COQualified == T)  
  rGames <- getGamesLastXDays(rGames, row$Date, maxdays)
  
  if(surface == TRUE) {
    rGames <- rGames %>% filter(Surface == row$Surface)
  } 
  
  winnerWonIndexes  <- grep(row$idWinner, rGames$idWinner)
  winnerLostIndexes <- grep(row$idWinner, rGames$idLoser)
  loserWonIndexes   <- grep(row$idLoser, rGames$idWinner)
  loserLostIndexes  <- grep(row$idLoser, rGames$idLoser)
  
  winnerOpponentsLastYear    <- unique(c(rGames$idLoser[winnerWonIndexes], rGames$idWinner[winnerLostIndexes]))
  loserOpponentsLastYear     <- unique(c(rGames$idLoser[loserWonIndexes], rGames$idWinner[loserLostIndexes]))
  commonOpponents            <- data.frame(id = intersect(winnerOpponentsLastYear, loserOpponentsLastYear))
  
  commonOpponentsIndexes <- list()
  
  commonOpponentsIndexes$WinnerWon <- 
    rGames$indexNumber[winnerWonIndexes[rGames$idLoser[winnerWonIndexes] %in% commonOpponents$id]]
  commonOpponentsIndexes$WinnerLost <- 
    rGames$indexNumber[winnerLostIndexes[rGames$idWinner[winnerLostIndexes] %in% commonOpponents$id]]
  
  commonOpponentsIndexes$LoserWon <- 
    rGames$indexNumber[loserWonIndexes[rGames$idLoser[loserWonIndexes] %in% commonOpponents$id]]
  commonOpponentsIndexes$LoserLost <- 
    rGames$indexNumber[loserLostIndexes[rGames$idWinner[loserLostIndexes] %in% commonOpponents$id]]
  
  #Now again for points
  rGames <- rGames %>% filter(COPointsQualified == T)
  rGames <- getGamesLastXDays(rGames, row$Date, maxdays)
  
  if(surface == TRUE) {
    rGames <- rGames %>% filter(Surface == row$Surface)
  }
  
  winnerWonIndexes  <- grep(row$idWinner, rGames$idWinner)
  winnerLostIndexes <- grep(row$idWinner, rGames$idLoser)
  loserWonIndexes   <- grep(row$idLoser, rGames$idWinner)
  loserLostIndexes  <- grep(row$idLoser, rGames$idLoser)
  
  winnerOpponentsLastYear <- unique(c(rGames$idLoser[winnerWonIndexes], rGames$idWinner[winnerLostIndexes]))
  loserOpponentsLastYear  <- unique(c(rGames$idLoser[loserWonIndexes], rGames$idWinner[loserLostIndexes]))
  commonOpponents         <- data.frame(id = intersect(winnerOpponentsLastYear, loserOpponentsLastYear))
  
  commonOpponentsIndexes$WinnerWonPoints <- 
    rGames$indexNumber[winnerWonIndexes[rGames$idLoser[winnerWonIndexes] %in% commonOpponents$id]]
  commonOpponentsIndexes$WinnerLostPoints <- 
    rGames$indexNumber[winnerLostIndexes[rGames$idWinner[winnerLostIndexes] %in% commonOpponents$id]]
  commonOpponentsIndexes$LoserWonPoints <- 
    rGames$indexNumber[loserWonIndexes[rGames$idLoser[loserWonIndexes] %in% commonOpponents$id]]
  commonOpponentsIndexes$LoserLostPoints <- 
    rGames$indexNumber[loserLostIndexes[rGames$idWinner[loserLostIndexes] %in% commonOpponents$id]]
  
  return(commonOpponentsIndexes)
}

calculateWeights <- function(Games, coi) {
  weights <- list()
  opponentFreqsWinners <- 
    table(c(Games$idLoser[coi$WinnerWon], Games$idWinner[coi$WinnerLost]))
  opponentFreqsLosers <- 
    table(c(Games$idLoser[coi$LoserWon], Games$idWinner[coi$LoserLost]))
  
  freqs <- bind_rows(opponentFreqsWinners, opponentFreqsLosers)
  w <- apply(freqs, 2, mean)
  winnerweights <- w / freqs[1, ]
  loserweights  <- w / freqs[2, ]
  
  weights$WinnerWon  <- 
   as.numeric(winnerweights[match(Games$idLoser[coi$WinnerWon], names(winnerweights))])
  weights$WinnerLost <- 
    as.numeric(winnerweights[match(Games$idWinner[coi$WinnerLost], names(winnerweights))])
  weights$LoserWon   <- 
    as.numeric(loserweights[match(Games$idLoser[coi$LoserWon], names(loserweights))])
  weights$LoserLost  <- 
    as.numeric(loserweights[match(Games$idWinner[coi$LoserLost], names(loserweights))])
  
  opponentFreqsWinners <- 
    table(c(Games$idLoser[coi$WinnerWonPoints], Games$idWinner[coi$WinnerLostPoints]))
  opponentFreqsLosers <- 
    table(c(Games$idLoser[coi$LoserWonPoints], Games$idWinner[coi$LoserLostPoints]))
  
  freqs <- bind_rows(opponentFreqsWinners, opponentFreqsLosers)
  w <- apply(freqs, 2, mean)
  winnerweights <- w / freqs[1, ]
  loserweights  <- w / freqs[2, ]
  
  weights$WinnerWonPoints  <- 
    as.numeric(winnerweights[match(Games$idLoser[coi$WinnerWonPoints], names(winnerweights))])
  weights$WinnerLostPoints <- 
    as.numeric(winnerweights[match(Games$idWinner[coi$WinnerLostPoints], names(winnerweights))])
  weights$LoserWonPoints   <- 
    as.numeric(loserweights[match(Games$idLoser[coi$LoserWonPoints], names(loserweights))])
  weights$LoserLostPoints  <- 
    as.numeric(loserweights[match(Games$idWinner[coi$LoserLostPoints], names(loserweights))])
  
  return(weights)
}

getGamesLastXDays <- function(Games, currentDate, maxdays) {
  df = "%Y-%m-%d"
  Mindate = as.Date(as.character(currentDate), format = df ) - maxdays
  Games[Games$Date >= Mindate & Games$Date <= currentDate, ]
}

getIndexesCurrentSurface = function(allIndexes, previousGames, surface) {
  allIndexes[previousGames$Surface[allIndexes] == surface]
}

calculatePercentSetsWon = function(Games, wonIndexes, lostIndexes, wonWeights, lostWeights) {
  sum(c(Games$Winner_percentsetswon[wonIndexes] * wonWeights, 
    (1 - Games$Winner_percentsetswon[lostIndexes]) * lostWeights)) / sum(c(wonWeights, lostWeights))
}

calculatePercentGamesWon = function(Games, wonIndexes, lostIndexes, wonWeights, lostWeights) {
  sum(c(Games$Winner_percentgameswon[wonIndexes] * wonWeights, 
        (1 - Games$Winner_percentgameswon[lostIndexes]) * lostWeights)) / sum(c(wonWeights, lostWeights))
}

calculatePercentPointsWon = function(Games, wonIndexes, lostIndexes, wonWeights, lostWeights) {
  sum(c(Games$Winner_percentpointswon[wonIndexes] * wonWeights, 
        (1 - Games$Winner_percentpointswon[lostIndexes]) * lostWeights)) / sum(c(wonWeights, lostWeights))
}
