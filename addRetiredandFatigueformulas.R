CreateRetiredWalkoverAndFatigue = function(dataset){
  
  retired = which(dataset$Comment %in% c("Retired", "Retied", "retired"))
  Nret = length(retired)
  
  dataset$Winner_retired_last_game = rep(0, nrow(dataset))
  dataset$Loser_retired_last_game = rep(0, nrow(dataset))
  
  Nt = length(dataset$Winner)
  
  for(i in 1 : Nret){
    currentIndex = retired[i]
    retiredPlayer = dataset$Loser[currentIndex]
    nextGame = FindNextGame(retiredPlayer, dataset$Winner[currentIndex + 1 : Nt], 
                            dataset$Loser[currentIndex + 1 : Nt], currentIndex)
    
    if(nextGame$player == 1) {
      dataset$Winner_retired_last_game[nextGame$number] = 1
    } else if(nextGame$player == 2) {
      dataset$Loser_retired_last_game[nextGame$number] = 1 
    }
  }
  
  walkover = which(dataset$Comment %in% c("Walkover", "Walover"))
  Nwalkover = length(walkover)
  
  dataset$Winner_walkover_last_game = rep(0, nrow(dataset))
  dataset$Loser_walkover_last_game = rep(0, nrow(dataset))
  
  Nt = length(dataset$Winner)
  
  for(i in 1 : Nwalkover){
    currentIndex = walkover[i]
    walkoverPlayer = dataset$Loser[currentIndex]
    nextGame = FindNextGame(walkoverPlayer, dataset$Winner[currentIndex + 1 : Nt], 
                            dataset$Loser[currentIndex + 1 : Nt], currentIndex)
    
    if(nextGame$player == 1) {
      dataset$Winner_walkover_last_game[nextGame$number] = 1
    } else if(nextGame$player == 2) {
      dataset$Loser_walkover_last_game[nextGame$number] = 1 
    }
  }
  
  dataset$NrGames = rep(0, nrow(dataset))
  
  #Create variable NrGames for all matches
  for(i in 1 : nrow(dataset)) {
    
    NrGames = 0 
    
    games = as.numeric(c(dataset$W1[i], dataset$L1[i], dataset$W2[i], dataset$L2[i], dataset$W3[i], 
              dataset$L3[i], dataset$W4[i], dataset$L4[i], dataset$W5[i], dataset$L5[i]))
    for(game in games){
      #if(j == 0) {
      #  next
      #}
      if(!is.na(game) & game < 8 & game >=0){
        NrGames = NrGames + game
      }
    }
    
    dataset$NrGames[i] = NrGames
  }
  
  #HyperParameters
  maxdays = 5
  base = 0.86
  
  dataset$Winner_fatigue = rep(0, nrow(dataset))
  dataset$Loser_fatigue = rep(0, nrow(dataset))
  
  for(i in 1 : (nrow(dataset) - 1)){
    winner = dataset$Winner[i]
    loser = dataset$Loser[i]
    dataset = setFatigueMatchPlayer(name = winner, dataset, matchnumber = i, maxdays = maxdays, base = base)
    dataset = setFatigueMatchPlayer(name = loser, dataset, matchnumber = i, maxdays = maxdays, base = base)
  }
  
  return(dataset)
}

#Sets the fatigue for the next matches created by this match for this player
setFatigueMatchPlayer = function(name, dataset, matchnumber, maxdays, base) {
  Ndata = nrow(dataset)
  df = "%m/%d/%Y"
  gamesPlayed = dataset$NrGames[matchnumber]
  
  nextWinningGames = list()
  nextWinningGames$index = which(dataset$Winner[(matchnumber + 1) : Ndata] %in% name) + matchnumber
  nextWinningGames$daysafter = abs(as.Date(as.character(dataset$Date[matchnumber]), format = df ) - 
    as.Date(as.character(dataset$Date[nextWinningGames$index]), format = df ))
  
  if(length(nextWinningGames$index) > 0){
    for(i in 1:length(nextWinningGames$index)) {
      if(nextWinningGames$daysafter[i] <= maxdays) {
        fatigue = base ^ as.numeric(nextWinningGames$daysafter[i]) * gamesPlayed
        dataset$Winner_fatigue[nextWinningGames$index[i]] = dataset$Winner_fatigue[nextWinningGames$index[i]] + 
          fatigue
      }
    }
  }
  
  nextLosingGames = list()
  nextLosingGames$index = which(dataset$Loser[(matchnumber + 1) : Ndata] %in% name) + matchnumber
  nextLosingGames$daysafter = abs(as.Date(as.character(dataset$Date[matchnumber]), format = df ) - 
    as.Date(as.character(dataset$Date[nextLosingGames$index]), format = df ))
  
  relevantLosingGames = nextLosingGames$index[(nextLosingGames$daysafter < maxdays) ]
  
  if(length(nextLosingGames$index) > 0){
    for(i in 1:length(nextLosingGames$index)) {
      if(nextLosingGames$daysafter[i] <= maxdays) {
        fatigue = base ^ as.numeric(nextLosingGames$daysafter[i]) * gamesPlayed
        dataset$Loser_fatigue[nextLosingGames$index[i]] = dataset$Loser_fatigue[nextLosingGames$index[i]] + 
          fatigue
      }
    }
  }
  return(dataset)
}