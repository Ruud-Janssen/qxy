

CreateFatigue = function(dataset, days, power){
  maxdays = days
  base = power
  
  Nt = nrow(dataset)
  
  dataset$Winner_fatigue = rep(0, Nt)
  dataset$Loser_fatigue = rep(0, Nt)
  
  for(i in 1 : (Nt - 1)){
    winner = dataset$Winner[i]
    loser = dataset$Loser[i]
    dataset = setFatigueMatchPlayer(name = winner, dataset, matchnumber = i, maxdays = maxdays, base = base)
    dataset = setFatigueMatchPlayer(name = loser, dataset, matchnumber = i, maxdays = maxdays, base = base)
  }
  
  return(dataset)
}

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


FindNextGame = function(name, winners, losers, nrPreviousGames){
  nextWin = match(name, winners)
  nextLoss = match(name, losers)
  
  nextGame = list()
  
  if(is.na(nextWin) & is.na(nextLoss)){
    nextGame$number = NA
    nextGame$player = 0
    return(nextGame)
  }
  
  if(is.na(nextWin) & !is.na(nextLoss)) {
    nextGame$number = nextLoss + nrPreviousGames
    nextGame$player = 2
    return(nextGame)
  } else if(!is.na(nextWin) & is.na(nextLoss)){
    nextGame$number = nextWin + nrPreviousGames
    nextGame$player = 1
    return(nextGame)
  }
  
  if(nextWin < nextLoss) {
    nextGame$number = nextWin + nrPreviousGames
    nextGame$player = 1
  } else if (nextLoss < nextWin){
    nextGame$number = nextLoss + nrPreviousGames
    nextGame$player = 2
  }
  return(nextGame)
}

FindDaysDiff = function(dates){
  date1 = as.Date(as.character(dates[1]), format = "%m/%d/%Y" )
  date2 = as.Date(as.character(dates[2]), format = "%m/%d/%Y" )
  
  return(abs(as.numeric(date2-date1)))
}
FindnextGameSamePlayers = function(winner, loser, winners, losers, previousMatches) {
  winner_innext_winner = previousMatches + which(winners %in% winner) 
  winner_innext_loser = previousMatches + which(losers %in% winner) 
  
  loser_innext_winner = previousMatches + which(winners %in% loser) 
  loser_innext_loser = previousMatches + which(losers %in% loser) 
  
  sameOrders = intersect(winner_innext_winner, loser_innext_loser)
  oppOrders = intersect(winner_innext_loser, loser_innext_winner)
  
  sameOrders = sameOrders[1]
  oppOrders = oppOrders[1]
  
  nextGame = list()
  
  if(!is.na(sameOrders) & !is.na(oppOrders)) {
    if(sameOrders < oppOrders) {
      nextGame$Number = sameOrders
      nextGame$Order = 1
    } else {
      nextGame$Number = oppOrders
      nextGame$Order = 2
    }
  } else if(!is.na(sameOrders)) {
    nextGame$Number = sameOrders
    nextGame$Order = 1
  } else if(!is.na(oppOrders)) {
    nextGame$Number = oppOrders
    nextGame$Order = 2
  } else {
    nextGame$Order = 0
  }  
  
  return(nextGame)
}


LogLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}