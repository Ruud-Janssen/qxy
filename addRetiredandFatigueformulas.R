library(plyr)

CreateRetiredWalkoverAndFatigue = function(dataset, maxdays, base, gamesBarier){
  #Create Retired and Walkover
  dataset = SetRetired(dataset)
  datset = SetWalkover(dataset)
  
  #Create variable NrGames for all matches
  dataset = adply(dataset, 1, SetNrGamesRow)
  
  #Create Fatigue
  dataset = CreateFatigue(dataset, maxdays, base, gamesBarier)
  
  return(dataset)
}

SetRetired = function(dataset){
  retiredIndexes = which(dataset$Comment %in% c("Retired", "Retied", "retired"))
  
  dataset$Winner_retired_last_game = rep(0, nrow(dataset))
  dataset$Loser_retired_last_game = rep(0, nrow(dataset))
  
  Nt = length(dataset$Winner)
  
  for(currentIndex in retiredIndexes){
    retiredPlayer = dataset$Loser[currentIndex]
    nextGame = FindNextGame(retiredPlayer, dataset$Winner[currentIndex + 1 : Nt], 
                            dataset$Loser[currentIndex + 1 : Nt], currentIndex)
    
    if(nextGame$player == 1) {
      dataset$Winner_retired_last_game[nextGame$number] = 1
    } else if(nextGame$player == 2) {
      dataset$Loser_retired_last_game[nextGame$number] = 1 
    }
  }
  return(dataset)
}

SetWalkover = function(dataset){
  walkoverIndexes = which(dataset$Comment %in% c("Walkover", "Walover"))
  
  dataset$Winner_walkover_last_game = rep(0, nrow(dataset))
  dataset$Loser_walkover_last_game = rep(0, nrow(dataset))
  
  Nt = length(dataset$Winner)
  
  for(currentIndex in walkoverIndexes){
    walkoverPlayer = dataset$Loser[currentIndex]
    nextGame = FindNextGame(walkoverPlayer, dataset$Winner[currentIndex + 1 : Nt], 
                            dataset$Loser[currentIndex + 1 : Nt], currentIndex)
    
    if(nextGame$player == 1) {
      dataset$Winner_walkover_last_game[nextGame$number] = 1
    } else if(nextGame$player == 2) {
      dataset$Loser_walkover_last_game[nextGame$number] = 1 
    }
  }
  return(dataset)
}

SetNrGamesRow = function(row){
  row$NrGames = CalculateNrGames(row)
  return(row)
}

CalculateNrGames = function(row){
  NrGames = 0 
  
  games = as.numeric(c(row$W1, row$L1, row$W2, row$L2, row$W3, 
                       row$L3, row$W4, row$L4, row$W5, row$L5))
  for(game in games){
    if(!is.na(game)){
      if(game >= 100){
        NrGames = NrGames + 100
      } else {
      NrGames = NrGames + game
      }
    }
  }
  return(NrGames)
}

CreateFatigue = function(dataset, maxdays, base, gamesBarier){
  
  Nt = nrow(dataset)
  
  dataset$Winner_fatigue = rep(0, Nt)
  dataset$Loser_fatigue = rep(0, Nt)
  
  for(i in 1 : (Nt - 1)){
    dataset = setFatigueMatchPlayer(dataset$Winner[i], dataset,i, maxdays, base, gamesBarier)
    dataset = setFatigueMatchPlayer(dataset$Loser[i], dataset, i, maxdays, base, gamesBarier)
  }
  return(dataset)
}

setFatigueMatchPlayerPartWinsOrLosses = function(dataset, NextGamesIndexes, MatchDate, maxdays, base,
                                                 GamesOverBarier, df, winnerDummy){
  
  nextGamesDaysafter = abs(MatchDate - as.Date(as.character(dataset$Date[NextGamesIndexes]), format = df ))
  
  for(i in 1:length(NextGamesIndexes)) {
    if(nextGamesDaysafter[i] <= maxdays) {
      fatigue = base ^ as.numeric(nextGamesDaysafter[i]) * GamesOverBarier
      if(winnerDummy == 1){
        dataset$Winner_fatigue[NextGamesIndexes[i]] = dataset$Winner_fatigue[NextGamesIndexes[i]] + fatigue
      } else {
        dataset$Loser_fatigue[NextGamesIndexes[i]] = dataset$Loser_fatigue[NextGamesIndexes[i]] + fatigue
      }
    }
  }
  return(dataset)
}

setFatigueMatchPlayer = function(name, dataset, matchnumber, maxdays, base, gamesBarier) {
  df = "%m/%d/%Y"
  Ndata = nrow(dataset)
  MatchDate = as.Date(as.character(dataset$Date[matchnumber]), format = df )
  GamesOverBarier = max(dataset$NrGames[matchnumber] - gamesBarier, 0)
  
  WinningGamesIndex = which(dataset$Winner[(matchnumber + 1) : Ndata] %in% name) + matchnumber
  if(length(WinningGamesIndex) > 0) {
    dataset = setFatigueMatchPlayerPartWinsOrLosses(dataset, WinningGamesIndex, MatchDate, maxdays, base, 
                                                    GamesOverBarier, df, 1)
  }
  
  LosingGamesIndex = which(dataset$Loser[(matchnumber + 1) : Ndata] %in% name) + matchnumber
  if(length(LosingGamesIndex) > 0) {
    dataset = setFatigueMatchPlayerPartWinsOrLosses(dataset, LosingGamesIndex, MatchDate, maxdays, base, 
                                                    GamesOverBarier, df, 0)
  }
  return(dataset)
}