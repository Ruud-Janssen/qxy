library(plyr)

CreateRetiredWalkoverAndFatigue = function(dataset, maxdays, base, gamesBarier){
  #Create Retired and Walkover
  dataset <- SetRetired(dataset)
  dataset <- SetWalkover(dataset)
  #dataset <- SetLastGamePercentWon(dataset)
  
  #Create variable NrGames for all matches
  dataset <- adply(dataset, 1, SetNrGamesRow)
  
  #Create Fatigue
  CreateFatigueAndRecentGames(dataset, maxdays, base, gamesBarier)
}

SetLastGamePercentWon <- function(dataset) {
  dataset$Winner_LastGamePercentGamesWon <- 0.5
  dataset$Loser_LastGamePercentGamesWon  <-  0.5
  
  for(i in 1 : (nrow(dataset) - 1)) {
    FractionGamesWinner  <- calculateFractionNetBreakGamesWinnerWon(allGames[i, ])
    FractionGamesLoser   <- 1 - FractionGamesWinner
    
    if(is.nan(FractionGamesWinner)) {
      next()
    } else if(calculateGames(allGames[i , ]) <= 10) {
      next()
    }

    dataset <- SetFractionGames(dataset, FractionGamesWinner, allGames$Winner[i], i)
    dataset <- SetFractionGames(dataset, FractionGamesLoser , allGames$Loser[i] , i)
  }
  return(dataset)
}

SetFractionGames <- function(dataset, FractionGamesPlayer, playerName, indexGame) {
  Nt       <- nrow(dataset)
  nextGame <- FindNextGame(playerName, dataset$Winner[indexGame + 1 : Nt], 
                           dataset$Loser[indexGame + 1 : Nt], indexGame)
  
  if(nextGame$player == 1) {
    dataset$Winner_LastGamePercentGamesWon[nextGame$number] <- FractionGamesPlayer
  } else if(nextGame$player == 2) {
    dataset$Loser_LastGamePercentGamesWon[nextGame$number]  <- FractionGamesPlayer 
  }
  return(dataset)
}


SetRetired = function(dataset) {
  retiredIndexes <- which(dataset$Comment %in% c("Retired", "Retied", "retired"))
  
  dataset$Winner_retired_last_game <- rep(0, nrow(dataset))
  dataset$Loser_retired_last_game  <- rep(0, nrow(dataset))
  
  Nt <- length(dataset$Winner)
  
  for(currentIndex in retiredIndexes){
    retiredPlayerIndex <- dataset$idLoser[currentIndex]
    nextGame      <- FindNextGame(retiredPlayerIndex, dataset$idWinner[currentIndex + 1 : Nt], 
                            dataset$idLoser[currentIndex + 1 : Nt], currentIndex)
    
    if(nextGame$player == 1) {
      dataset$Winner_retired_last_game[nextGame$number] <- 1
    } else if(nextGame$player == 2) {
      dataset$Loser_retired_last_game[nextGame$number]  <- 1 
    }
  }
  return(dataset)
}

SetWalkover = function(dataset){
  walkoverIndexes <- which(dataset$Comment %in% c("Walkover", "Walover"))
  
  dataset$Winner_walkover_last_game <- rep(0, nrow(dataset))
  dataset$Loser_walkover_last_game  <- rep(0, nrow(dataset))
  
  Nt <- length(dataset$Winner)
  
  for(currentIndex in walkoverIndexes){
    walkoverPlayerIndex <- dataset$idLoser[currentIndex]
    nextGame       <- FindNextGame(walkoverPlayerIndex, dataset$idWinner[currentIndex + 1 : Nt], 
                            dataset$idLoser[currentIndex + 1 : Nt], currentIndex)
    
    if(nextGame$player == 1) {
      dataset$Winner_walkover_last_game[nextGame$number] <- 1
    } else if(nextGame$player == 2) {
      dataset$Loser_walkover_last_game[nextGame$number]  <- 1 
    }
  }
  return(dataset)
}

SetNrGamesRow = function(row){
  row$Winner_Games <- calculatePlayerGames(row, 1)
  row$Loser_Games  <- calculatePlayerGames(row, 0)
  row$NrGames      <- row$Winner_Games + row$Loser_Games
  return(row)
}

calculatePlayerGames = function(row, winner) {
  if (winner == 1) {
    games <- as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5))
  } else {
    games <- as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5))
  }
  calculateNrGames(games)
}

calculateNrGames = function(games){
  NrGames <- 0
  for(game in games){
    if(!is.na(game)){
        NrGames <- NrGames + game
    }
  }
  return(NrGames)
}

CreateFatigueAndRecentGames = function(dataset, maxdays, base, gamesBarier){
  
  Nt <- nrow(dataset)
  
  dataset$Winner_fatigue <- rep(0, Nt)
  dataset$Loser_fatigue  <- rep(0, Nt)
  
  dataset$Winner_recentGames <- rep(0 , Nt)
  dataset$Loser_recentGames  <- rep(0 , Nt)
  
  for(i in 1 : (Nt - 1)){
    dataset <- setFatigueMatchPlayer(dataset$idWinner[i], dataset, i, maxdays, base, gamesBarier)
    dataset <- setFatigueMatchPlayer(dataset$idLoser[i], dataset, i, maxdays, base, gamesBarier)
  }
  return(dataset)
}

setFatigueMatchPlayerPartWinsOrLosses = function(dataset, NextGamesIndexes, MatchDate, maxdays, base,
                                                 GamesOverBarier, df, winnerDummy){
  
  nextGamesDaysafter <- abs(MatchDate - as.Date(as.character(dataset$Date[NextGamesIndexes]), format = df ))
  
  for(i in 1:length(NextGamesIndexes)) {
    if(nextGamesDaysafter[i] <= maxdays) {
      
      dayWeight <- base ^ as.numeric(nextGamesDaysafter[i])
      fatigue   <- dayWeight * GamesOverBarier
      if(winnerDummy == 1){
        dataset$Winner_fatigue[NextGamesIndexes[i]]     <- dataset$Winner_fatigue[NextGamesIndexes[i]] + fatigue
        dataset$Winner_recentGames[NextGamesIndexes[i]] <- dataset$Winner_recentGames[NextGamesIndexes[i]] + dayWeight
      } else {
        dataset$Loser_fatigue[NextGamesIndexes[i]]     <- dataset$Loser_fatigue[NextGamesIndexes[i]] + fatigue
        dataset$Loser_recentGames[NextGamesIndexes[i]] <- dataset$Loser_recentGames[NextGamesIndexes[i]] + dayWeight
      }
    }
  }
  return(dataset)
}

setFatigueMatchPlayer = function(idPlayer, dataset, matchnumber, maxdays, base, gamesBarier) {
  df    <- "%Y-%m-%d"
  Ndata <- nrow(dataset)
  MatchDate <- as.Date(as.character(dataset$Date[matchnumber]), format = df)
  GamesOverBarier <- max(dataset$NrGames[matchnumber] - gamesBarier, 0)
  
  WinningGamesIndex <- which(dataset$idWinner[(matchnumber + 1) : Ndata] %in% idPlayer) + matchnumber
  if(length(WinningGamesIndex) > 0) {
    dataset <- setFatigueMatchPlayerPartWinsOrLosses(dataset, WinningGamesIndex, MatchDate, maxdays, base, 
                                                    GamesOverBarier, df, 1)
  }
  
  LosingGamesIndex <- which(dataset$idLoser[(matchnumber + 1) : Ndata] %in% idPlayer) + matchnumber
  if(length(LosingGamesIndex) > 0) {
    dataset <- setFatigueMatchPlayerPartWinsOrLosses(dataset, LosingGamesIndex, MatchDate, maxdays, base, 
                                                    GamesOverBarier, df, 0)
  }
  return(dataset)
}