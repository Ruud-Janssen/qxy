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

setCommonOpponentVariablesRow = function(Games, i, matchDetails, maxdays) {
  previousGames = Games[1 : (i - 1), ]
  
  #Common Opponent Indexes
  coi = getPreviousGamesIndexes(previousGames, matchDetails, maxdays)
  
  row = Games[i, ]
  
  row = setCommonOpponentVariablesPlayerRow(previousGames, row, coi$WinnerWon, coi$WinnerLost, 
                                            winner = 1, surface = 0)
  row = setCommonOpponentVariablesPlayerRow(previousGames, row, coi$WinnerWonSurface, coi$WinnerLostSurface
                                           , winner = 1, surface = 1)
  row = setCommonOpponentVariablesPlayerRow(previousGames, row, coi$LoserWon, coi$LoserLost, 
                                            winner = 0, surface = 0)
  row = setCommonOpponentVariablesPlayerRow(previousGames, row, coi$LoserWonSurface, coi$LoserLostSurface
                                           , winner = 0, surface = 1)
  return(row)
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

setCommonOpponentVariablesPlayerRow = function(previousGames, row, wonIndexes, lostIndexes, winner, surface) {
  if (winner == 1) {
    if(surface == 0) {
      row$Winner_COPercentMatchesWon = calculatePercentMatchesWon(wonIndexes, lostIndexes)
      row$Winner_COPercentSetsWon    = calculatePercentSetsWon(previousGames, wonIndexes, lostIndexes)
      row$Winner_COPercentGamesWon   = calculatePercentGamesWon(previousGames, wonIndexes, lostIndexes)
      row$Winner_COPercentPointsWon  = calculatePercentPointsWon(previousGames, wonIndexes, lostIndexes)
      row$Winner_COGames             = length(wonIndexes) + length(lostIndexes)
    } else {
      row$Winner_COPercentMatchesThisSurfaceWon = calculatePercentMatchesWon(wonIndexes, lostIndexes)
      row$Winner_COPercentSetsThisSurfaceWon    = calculatePercentSetsWon(previousGames, wonIndexes, lostIndexes)
      row$Winner_COPercentGamesThisSurfaceWon   = calculatePercentGamesWon(previousGames, wonIndexes, lostIndexes)
      row$Winner_COPercentPointsThisSurfaceWon  = calculatePercentPointsWon(previousGames, wonIndexes, lostIndexes)
      row$Winner_COThisSurfaceGames             = length(wonIndexes) + length(lostIndexes)
    }
  } else { 
    if (surface == 0) {
      row$Loser_COPercentMatchesWon = calculatePercentMatchesWon(wonIndexes, lostIndexes)
      row$Loser_COPercentSetsWon    = calculatePercentSetsWon(previousGames, wonIndexes, lostIndexes)
      row$Loser_COPercentGamesWon   = calculatePercentGamesWon(previousGames, wonIndexes, lostIndexes)
      row$Loser_COPercentPointsWon  = calculatePercentPointsWon(previousGames, wonIndexes, lostIndexes)
      row$Loser_COGames             = length(wonIndexes) + length(lostIndexes)
    } else {
      row$Loser_COPercentMatchesThisSurfaceWon = calculatePercentMatchesWon(wonIndexes, lostIndexes)
      row$Loser_COPercentSetsThisSurfaceWon    = calculatePercentSetsWon(previousGames, wonIndexes, lostIndexes)
      row$Loser_COPercentGamesThisSurfaceWon   = calculatePercentGamesWon(previousGames, wonIndexes, lostIndexes)
      row$Loser_COPercentPointsThisSurfaceWon  = calculatePercentPointsWon(previousGames, wonIndexes, lostIndexes)
      row$Loser_COThisSurfaceGames             = length(wonIndexes) + length(lostIndexes)
    }
  }
  return(row)
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
  #gamesWon  = sum(as.numeric(Games$Winner_Games[wonIndexes])) + sum(as.numeric(Games$Loser_Games[lostIndexes]))
  #gamesLost = sum(as.numeric(Games$Loser_Games[wonIndexes])) + sum(as.numeric(Games$Winner_Games[lostIndexes]))
  #gamesWon / (gamesWon + gamesLost)
  
  
  percentGamesInWonGames  = as.numeric(Games$Winner_Games[wonIndexes]) / 
    (as.numeric(Games$Winner_Games[wonIndexes]) + as.numeric(Games$Loser_Games[wonIndexes]))
  percentGamesInLostGames = as.numeric(Games$Loser_Games[lostIndexes]) / 
    (as.numeric(Games$Loser_Games[lostIndexes]) + as.numeric(Games$Winner_Games[lostIndexes]))
  return(mean(c(percentGamesInWonGames, percentGamesInLostGames)))
}

calculatePercentPointsWon = function(Games, wonIndexes, lostIndexes) {
  wonIndexes  = wonIndexes[!is.na(as.numeric(Games$WPts[wonIndexes]))]
  wonIndexes  = wonIndexes[!is.na(as.numeric(Games$LPts[wonIndexes]))]
  lostIndexes = lostIndexes[!is.na(as.numeric(Games$WPts[lostIndexes]))]
  lostIndexes = lostIndexes[!is.na(as.numeric(Games$LPts[lostIndexes]))]  

  percentPointsInWonGames  = as.numeric(Games$WPts[wonIndexes]) / 
    (as.numeric(Games$WPts[wonIndexes]) + as.numeric(Games$LPts[wonIndexes]))
  percentPointsInLostGames = as.numeric(Games$LPts[lostIndexes]) / 
    (as.numeric(Games$LPts[lostIndexes]) + as.numeric(Games$WPts[lostIndexes]))
   
  #pointsWon  = sum(as.numeric(Games$WPts[wonIndexes])) + sum(as.numeric(Games$LPts[lostIndexes]))
  #pointsLost = sum(as.numeric(Games$LPts[wonIndexes])) + sum(as.numeric(Games$WPts[lostIndexes]))
  #if (pointsWon == 0 | pointsLost == 0) {
  # return(NA)
  #}
  #pointsWon / (pointsWon + pointsLost)
  percentPoints = mean(c(percentPointsInWonGames, percentPointsInLostGames))
  if (length(percentPoints) == 0) {
    return(NA)
  } else {
    return(percentPoints)
  }
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