InitializeCommonOpponentsVariables = function(Games) {
  #rows of games
  rg = nrow(Games)
  
  Games$Winner_COPercentMatchesWon = rep(NA, rg)
  Games$Winner_COPercentSetsWon    = rep(NA, rg)
  Games$Winner_COPercentGamesWon   = rep(NA, rg)
  Games$Winner_COPercentPointsWon  = rep(NA, rg)
  Games$Winner_COGames             = rep(0, rg)
  
  Games$Loser_COPercentMatchesWon = rep(NA, rg)
  Games$Loser_COPercentSetsWon    = rep(NA, rg)
  Games$Loser_COPercentGamesWon   = rep(NA, rg)
  Games$Loser_COPercentPointsWon  = rep(NA, rg)
  Games$Loser_COGames             = rep(0, rg)
  
  Games$Winner_COPercentMatchesThisSurfaceWon = rep(NA, rg)
  Games$Winner_COPercentSetsThisSurfaceWon    = rep(NA, rg)
  Games$Winner_COPercentGamesThisSurfaceWon   = rep(NA, rg)
  Games$Winner_COPercentPointsThisSurfaceWon  = rep(NA, rg)
  Games$Winner_COThisSurfaceGames             = rep(0, rg)
  
  Games$Loser_COPercentMatchesThisSurfaceWon = rep(NA, rg)
  Games$Loser_COPercentSetsThisSurfaceWon    = rep(NA, rg)
  Games$Loser_COPercentGamesThisSurfaceWon   = rep(NA, rg)
  Games$Loser_COPercentPointsThisSurfaceWon  = rep(NA, rg)
  Games$Loser_COThisSurfaceGames             = rep(0, rg)
  
  return(Games)
}

setCommonOpponentVariables = function(Games, i, matchDetails, maxdays) {
  #Common Opponent Indexes
  coi = getPreviousGamesIndexes(Games[1 : (i - 1), ], matchDetails, maxdays)
  
  Games = setCommonOpponentVariablesPlayer(Games, i, coi$WinnerWon, coi$WinnerLost, winner = 1, surface = 0)
  Games = setCommonOpponentVariablesPlayer(Games, i, coi$WinnerWonSurface, coi$WinnerLostSurface
                                           , winner = 1, surface = 1)
  Games = setCommonOpponentVariablesPlayer(Games, i, coi$LoserWon, coi$LoserLost, winner = 0, surface = 0)
  Games = setCommonOpponentVariablesPlayer(Games, i, coi$LoserWonSurface, coi$LoserLostSurface
                                           , winner = 0, surface = 1)
}

getPreviousGamesIndexes = function(previousGames, matchDetails, maxdays) {
  commonOpponentsIndexes = list()

  winnerWonIndexes  = getLastYearsGames(previousGames, matchDetails$Winner, matchDetails$Date, maxdays, 1)
  winnerLostIndexes = getLastYearsGames(previousGames, matchDetails$Winner, matchDetails$Date, maxdays, 0)
  loserWonIndexes   = getLastYearsGames(previousGames, matchDetails$Loser, matchDetails$Date, maxdays, 1)
  loserLostIndexes  = getLastYearsGames(previousGames, matchDetails$Loser, matchDetails$Date, maxdays, 0)
 
  winnerOpponentsLastYear = unique(c(previousGames$Loser[winnerWonIndexes], previousGames$Winner[winnerLostIndexes]))
  loserOpponentsLastYear  = unique(c(previousGames$Loser[loserWonIndexes], previousGames$Winner[loserLostIndexes]))
  commonOpponents         = intersect(winnerOpponentsLastYear, loserOpponentsLastYear)
  
  commonOpponentsIndexes$WinnerWon  = winnerWonIndexes[previousGames$Loser[winnerWonIndexes] %in% commonOpponents]
  commonOpponentsIndexes$WinnerLost = winnerLostIndexes[previousGames$Winner[winnerLostIndexes] %in% commonOpponents]
  commonOpponentsIndexes$LoserWon   = loserWonIndexes[previousGames$Loser[loserWonIndexes] %in% commonOpponents]
  commonOpponentsIndexes$LoserLost  = loserLostIndexes[previousGames$Winner[loserLostIndexes] %in% commonOpponents]
  
  winnerWonThisSurfaceIndexes  = getIndexesCurrentSurface(winnerWonIndexes, previousGames, matchDetails$Surface)
  winnerLostThisSurfaceIndexes = getIndexesCurrentSurface(winnerLostIndexes, previousGames, matchDetails$Surface)
  loserWonThisSurfaceIndexes  = getIndexesCurrentSurface(loserWonIndexes, previousGames, matchDetails$Surface)
  loserLostThisSurfaceIndexes = getIndexesCurrentSurface(loserLostIndexes, previousGames, matchDetails$Surface)
  
  winnerOpponentsLastYearSurface = unique(c(previousGames$Loser[winnerWonThisSurfaceIndexes], 
                                            previousGames$Winner[winnerLostThisSurfaceIndexes]))
  loserOpponentsLastYearSurface  = unique(c(previousGames$Loser[loserWonThisSurfaceIndexes], 
                                            previousGames$Winner[loserLostThisSurfaceIndexes]))
  commonOpponentsSurface = intersect(winnerOpponentsLastYearSurface, loserOpponentsLastYearSurface)
  
  commonOpponentsIndexes$WinnerWonSurface  = 
    winnerWonThisSurfaceIndexes[previousGames$Loser[winnerWonThisSurfaceIndexes] %in% commonOpponentsSurface]
  commonOpponentsIndexes$WinnerLostSurface = 
    winnerLostThisSurfaceIndexes[previousGames$Winner[winnerLostThisSurfaceIndexes] %in% commonOpponentsSurface]
  commonOpponentsIndexes$LoserWonSurface   = 
    loserWonThisSurfaceIndexes[previousGames$Loser[loserWonThisSurfaceIndexes] %in% commonOpponentsSurface]
  commonOpponentsIndexes$LoserLostSurface  = 
    loserLostThisSurfaceIndexes[previousGames$Winner[loserLostThisSurfaceIndexes] %in% commonOpponentsSurface]
   
  return(commonOpponentsIndexes)
}

getIndexesCurrentSurface = function(allIndexes, previousGames, surface) {
  allIndexes[previousGames$Surface[allIndexes] == surface]
}

setCommonOpponentVariablesPlayer = function(Games, i, wonIndexes, lostIndexes, winner, surface) {
  if (winner == 1) {
    if(surface == 0) {
      Games$Winner_COPercentMatchesWon[i] = calculatePercentMatchesWon(wonIndexes, lostIndexes)
      Games$Winner_COPercentSetsWon[i]    = calculatePercentSetsWon(Games, wonIndexes, lostIndexes)
      Games$Winner_COPercentGamesWon[i]   = calculatePercentGamesWon(Games, wonIndexes, lostIndexes)
      Games$Winner_COPercentPointsWon[i]  = calculatePercentPointsWon(Games, wonIndexes, lostIndexes)
      Games$Winner_COGames[i]             = length(wonIndexes) + length(lostIndexes)
    } else {
      Games$Winner_COPercentMatchesThisSurfaceWon[i] = calculatePercentMatchesWon(wonIndexes, lostIndexes)
      Games$Winner_COPercentSetsThisSurfaceWon[i]    = calculatePercentSetsWon(Games, wonIndexes, lostIndexes)
      Games$Winner_COPercentGamesThisSurfaceWon[i]   = calculatePercentGamesWon(Games, wonIndexes, lostIndexes)
      Games$Winner_COPercentPointsThisSurfaceWon[i]  = calculatePercentPointsWon(Games, wonIndexes, lostIndexes)
      Games$Winner_COThisSurfaceGames[i]             = length(wonIndexes) + length(lostIndexes)
    }
  } else { 
    if (surface == 0) {
      Games$Loser_COPercentMatchesWon[i] = calculatePercentMatchesWon(wonIndexes, lostIndexes)
      Games$Loser_COPercentSetsWon[i]    = calculatePercentSetsWon(Games, wonIndexes, lostIndexes)
      Games$Loser_COPercentGamesWon[i]   = calculatePercentGamesWon(Games, wonIndexes, lostIndexes)
      Games$Loser_COPercentPointsWon[i]  = calculatePercentPointsWon(Games, wonIndexes, lostIndexes)
      Games$Loser_COGames[i]             = length(wonIndexes) + length(lostIndexes)
    } else {
      Games$Loser_COPercentMatchesThisSurfaceWon[i] = calculatePercentMatchesWon(wonIndexes, lostIndexes)
      Games$Loser_COPercentSetsThisSurfaceWon[i]    = calculatePercentSetsWon(Games, wonIndexes, lostIndexes)
      Games$Loser_COPercentGamesThisSurfaceWon[i]   = calculatePercentGamesWon(Games, wonIndexes, lostIndexes)
      Games$Loser_COPercentPointsThisSurfaceWon[i]  = calculatePercentPointsWon(Games, wonIndexes, lostIndexes)
      Games$Loser_COThisSurfaceGames[i]             = length(wonIndexes) + length(lostIndexes)
    }
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