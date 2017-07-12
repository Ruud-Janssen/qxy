InitializeCommonOpponentsVariables = function(Games) {
  #rows of games
  rg = nrow(Games)
  
  Games$Winner_COPercentMatchesWon = rep(NA, rg)
  Games$Winner_COPercentSetsWon = rep(NA, rg)
  Games$Winner_COPercentGamesWon = rep(NA, rg)
  Games$Winner_COPercentPointsWon = rep(NA, rg)
  
  Games$Loser_COPercentMatchesWon = rep(NA, rg)
  Games$Loser_COPercentSetsWon = rep(NA, rg)
  Games$Loser_COPercentGamesWon = rep(NA, rg)
  Games$Loser_COPercentPointsWon = rep(NA, rg)
  
  return(Games)
}

setCommonOpponentVariables = function(Games, i, matchDetails, maxdays) {
  winnerWonIndexes  = getLastYearsGames(Games[1 : (i - 1), ], matchDetails$Winner, matchDetails$Date, maxdays, 1)
  winnerLostIndexes = getLastYearsGames(Games[1 : (i - 1), ], matchDetails$Winner, matchDetails$Date, maxdays, 0)
  
  loserWonIndexes  = getLastYearsGames(Games[1 : (i - 1), ], matchDetails$Loser, matchDetails$Date, maxdays, 1)
  loserLostIndexes = getLastYearsGames(Games[1 : (i - 1), ], matchDetails$Loser, matchDetails$Date, maxdays, 0)
  
  winnerOpponentsLastYear = unique(c(Games$Loser[winnerWonIndexes], Games$Winner[winnerLostIndexes]))
  loserOpponentsLastYear  = unique(c(Games$Loser[loserWonIndexes], Games$Winner[loserLostIndexes]))
  
  commonOpponents = intersect(winnerOpponentsLastYear, loserOpponentsLastYear)
  
  winnerWonIndexesCommonOpponents  = winnerWonIndexes[Games$Loser[winnerWonIndexes] %in% commonOpponents]
  winnerLostIndexesCommonOpponents = winnerLostIndexes[Games$Winner[winnerLostIndexes] %in% commonOpponents]
  
  loserWonIndexesCommonOpponents  = loserWonIndexes[Games$Loser[loserWonIndexes] %in% commonOpponents]
  loserLostIndexesCommonOpponents = loserLostIndexes[Games$Winner[loserLostIndexes] %in% commonOpponents]
  
  Games = setCommonOpponentVariablesPlayer(Games, i, winnerWonIndexesCommonOpponents
                                           , winnerLostIndexesCommonOpponents, 1)
  Games = setCommonOpponentVariablesPlayer(Games, i, loserWonIndexesCommonOpponents
                                           , loserLostIndexesCommonOpponents, 0)
  
  return(Games)
}

setCommonOpponentVariablesPlayer = function(Games, i, wonIndexes, lostIndexes, winner) {
  if (winner == 1) {
    Games$Winner_COPercentMatchesWon[i] = calculatePercentMatchesWon(wonIndexes, lostIndexes)
    Games$Winner_COPercentSetsWon[i]    = calculatePercentSetsWon(Games, wonIndexes, lostIndexes)
    Games$Winner_COPercentGamesWon[i]   = calculatePercentGamesWon(Games, wonIndexes, lostIndexes)
    Games$Winner_COPercentPointsWon[i]  = calculatePercentPointsWon(Games, wonIndexes, lostIndexes)
  } else { 
    Games$Loser_COPercentMatchesWon[i] = calculatePercentMatchesWon(wonIndexes, lostIndexes)
    Games$Loser_COPercentSetsWon[i]    = calculatePercentSetsWon(Games, wonIndexes, lostIndexes)
    Games$Loser_COPercentGamesWon[i]   = calculatePercentGamesWon(Games, wonIndexes, lostIndexes)
    Games$Loser_COPercentPointsWon[i]  = calculatePercentPointsWon(Games, wonIndexes, lostIndexes)
  }
  return(Games)
}

calculatePercentMatchesWon = function(wonIndexes, lostIndexes) {
  length(wonIndexes) / (length(wonIndexes) + length(lostIndexes))
}

calculatePercentSetsWon = function(Games, wonIndexes, lostIndexes) {
  setsWon  = sum(as.numeric(Games$Wsets[wonIndexes])) + sum(as.numeric(Games$Lsets[lostIndexes]))
  setsLost = sum(as.numeric(Games$Lsets[wonIndexes])) + sum(as.numeric(Games$Wsets[lostIndexes]))
  setsWon / (setsWon + setsLost)
}

calculatePercentGamesWon = function(Games, wonIndexes, lostIndexes) {
  gamesWon  = sum(as.numeric(Games$Winner_Games[wonIndexes])) + sum(as.numeric(Games$Loser_Games[lostIndexes]))
  gamesLost = sum(as.numeric(Games$Loser_Games[wonIndexes])) + sum(as.numeric(Games$Winner_Games[lostIndexes]))
  gamesWon / (gamesWon + gamesLost)
}

calculatePercentPointsWon = function(Games, wonIndexes, lostIndexes) {
  #wonIndexes  = wonIndexes[is.numeric(Games$WPts[wonIndexes])]
  #wonIndexes  = wonIndexes[is.numeric(Games$LPts[wonIndexes])]
  #lostIndexes = lostIndexes[is.numeric(Games$WPts[lostIndexes])]
  #lostIndexes = lostIndexes[is.numeric(Games$LPts[lostIndexes])]  
    
  pointsWon  = sum(as.numeric(Games$WPts[wonIndexes])) + sum(as.numeric(Games$LPts[lostIndexes]))
  pointsLost = sum(as.numeric(Games$LPts[wonIndexes])) + sum(as.numeric(Games$WPts[lostIndexes]))
  pointsWon / (pointsWon + pointsLost)
}

getLastYearsGames = function(Games, name, date, maxdays, won) {
  df = "%m/%d/%Y"
  Mindate = as.Date(as.character(date), format = df ) - maxdays
  
  if(won == 1) {
    previousGames = grep(name, Games$Winner)
  } else {
    previousGames = grep(name, Games$Loser)
  }
  
  previousDates = as.Date(as.character(Games$Date[previousGames]), format = df)
  
  previousGames[previousDates >= Mindate]
}