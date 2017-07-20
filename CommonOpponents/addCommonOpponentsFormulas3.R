library(foreach)

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

setCommonOpponentVariablesRow = function(games, i, matchDetails, maxdays, wd) {
  previousGames = games[1 : (i - 1), ]
  
  #Common Opponent Indexes
  coi     = getPreviousGamesIndexes(previousGames, matchDetails, maxdays)
  daysdiff = getweightedDaysDiff(previousGames, coi, matchDetails$Date)
  
  row = games[i, ]
  
  row = setCommonOpponentVariablesPlayerRow(previousGames, row, coi$WinnerWon, coi$WinnerLost, wd,  
                                            daysdiff$OpponentsAll, daysdiff$All, winner = 1, surface = 0)
  row = setCommonOpponentVariablesPlayerRow(previousGames, row, coi$WinnerWonSurface, coi$WinnerLostSurface, wd,
                                            daysdiff$OpponentsSurface, daysdiff$Surface, winner = 1, surface = 1)
  row = setCommonOpponentVariablesPlayerRow(previousGames, row, coi$LoserWon, coi$LoserLost, wd,
                                            daysdiff$OpponentsAll, daysdiff$All, winner = 0, surface = 0)
  row = setCommonOpponentVariablesPlayerRow(previousGames, row, coi$LoserWonSurface, coi$LoserLostSurface, wd,
                                            daysdiff$OpponentsSurface, daysdiff$Surface, winner = 0, surface = 1)
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
  commonOpponentsIndexes$Opponents  = commonOpponents
  
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
  commonOpponentsIndexes$OpponentsSurface  = commonOpponentsSurface
   
  return(commonOpponentsIndexes)
}

getweightedDaysDiff = function(previousGames, coi, date) {
  df = "%m/%d/%Y"
  
  daysdiff = list()
  daysdiff$OpponentsAll = coi$Opponents
  daysdiff$All      = foreach(player = daysdiff$OpponentsAll, .combine = c) %do% {
    winnerDates = c(previousGames$Date[coi$WinnerWon[previousGames$Loser[coi$WinnerWon] %in% player]],
                    previousGames$Date[coi$WinnerLost[previousGames$Winner[coi$WinnerLost] %in% player]])
    loserDates = c(previousGames$Date[coi$LoserWon[previousGames$Loser[coi$LoserWon] %in% player]],
                   previousGames$Date[coi$LoserLost[previousGames$Winner[coi$LoserLost] %in% player]])
    abs(1/2 * ((as.Date(winnerDates, format = df) - date) + (as.Date(loserDates, format = df) - date)))
  }
  
  daysdiff$OpponentsSurface = coi$OpponentsSurface
  daysdiff$Surface      = foreach(player = daysdiff$OpponentsSurface, .combine = c) %do% {
    winnerDatesSurface = 
      c(previousGames$Date[coi$WinnerWonSurface[previousGames$Loser[coi$WinnerWonSurface] %in% player]],
        previousGames$Date[coi$WinnerLostSurface[previousGames$Winner[coi$WinnerLostSurface] %in% player]])
    loserDatesSurface = 
      c(previousGames$Date[coi$LoserWonSurface[previousGames$Loser[coi$LoserWonSurface] %in% player]],
        previousGames$Date[coi$LoserLostSurface[previousGames$Winner[coi$LoserLostSurface] %in% player]])
    abs(mean(c((as.Date(winnerDatesSurface, format = df) - date),(as.Date(loserDatesSurface, format = df) - date))))
  }
  return(daysdiff)
}

getIndexesCurrentSurface = function(allIndexes, previousGames, surface) {
  allIndexes[previousGames$Surface[allIndexes] == surface]
}

setCommonOpponentVariablesPlayerRow = 
  function(previousGames, row, wonIndexes, lostIndexes, wd, opponents, dd, winner, surface) {
    daysdifflist = list()
    daysdifflist$Opponents = opponents
    daysdifflist$DaysDiff = dd
    if (winner == 1) {
      if(surface == 0) {
        row$Winner_COPercentMatchesWon = calculatePercentMatchesWon(wonIndexes, lostIndexes)
        row$Winner_COPercentSetsWon    = calculatePercentSetsWon(previousGames, wonIndexes, lostIndexes)
        row$Winner_COPercentGamesWon   = calculatePercentGamesWon(previousGames, wd, daysdifflist, wonIndexes, lostIndexes)
        row$Winner_COPercentPointsWon  = calculatePercentPointsWon(previousGames, wd, daysdifflist, wonIndexes, lostIndexes)
        row$Winner_COGames             = length(wonIndexes) + length(lostIndexes)
      } else {
        row$Winner_COPercentMatchesThisSurfaceWon = calculatePercentMatchesWon(wonIndexes, lostIndexes)
        row$Winner_COPercentSetsThisSurfaceWon    = calculatePercentSetsWon(previousGames, wonIndexes, lostIndexes)
        row$Winner_COPercentGamesThisSurfaceWon   = calculatePercentGamesWon(previousGames, wd, daysdifflist, wonIndexes, lostIndexes)
        row$Winner_COPercentPointsThisSurfaceWon  = calculatePercentPointsWon(previousGames, wd, daysdifflist, wonIndexes, lostIndexes)
        row$Winner_COThisSurfaceGames             = length(wonIndexes) + length(lostIndexes)
      }
    } else { 
      if (surface == 0) {
        row$Loser_COPercentMatchesWon = calculatePercentMatchesWon(wonIndexes, lostIndexes)
        row$Loser_COPercentSetsWon    = calculatePercentSetsWon(previousGames, wonIndexes, lostIndexes)
        row$Loser_COPercentGamesWon   = calculatePercentGamesWon(previousGames, wd, daysdifflist, wonIndexes, lostIndexes)
        row$Loser_COPercentPointsWon  = calculatePercentPointsWon(previousGames, wd, daysdifflist, wonIndexes, lostIndexes)
        row$Loser_COGames             = length(wonIndexes) + length(lostIndexes)
      } else {
        row$Loser_COPercentMatchesThisSurfaceWon = calculatePercentMatchesWon(wonIndexes, lostIndexes)
        row$Loser_COPercentSetsThisSurfaceWon    = calculatePercentSetsWon(previousGames, wonIndexes, lostIndexes)
        row$Loser_COPercentGamesThisSurfaceWon   = calculatePercentGamesWon(previousGames, wd, daysdifflist, wonIndexes, lostIndexes)
        row$Loser_COPercentPointsThisSurfaceWon  = calculatePercentPointsWon(previousGames, wd, daysdifflist, wonIndexes, lostIndexes)
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

calculatePercentGamesWon = function(Games, wd, daysdifflist, wonIndexes, lostIndexes) {
  #gamesWon  = sum(as.numeric(Games$Winner_Games[wonIndexes])) + sum(as.numeric(Games$Loser_Games[lostIndexes]))
  #gamesLost = sum(as.numeric(Games$Loser_Games[wonIndexes])) + sum(as.numeric(Games$Winner_Games[lostIndexes]))
  #gamesWon / (gamesWon + gamesLost)
  wonDateDiff  = daysdifflist$DaysDiff[match(Games$Loser[wonIndexes], daysdifflist$Opponents)]
  lostDateDiff = daysdifflist$DaysDiff[match(Games$Winner[lostIndexes], daysdifflist$Opponents)]
  weightWon  = wd ^ as.numeric(wonDateDiff)
  weightLost = wd ^ as.numeric(lostDateDiff)
  
  percentGamesInWonGames  = weightWon * as.numeric(Games$Winner_Games[wonIndexes]) / 
    (as.numeric(Games$Winner_Games[wonIndexes]) + as.numeric(Games$Loser_Games[wonIndexes]))
  percentGamesInLostGames = weightLost * as.numeric(Games$Loser_Games[lostIndexes]) / 
    (as.numeric(Games$Loser_Games[lostIndexes]) + as.numeric(Games$Winner_Games[lostIndexes]))
  return(mean(c(percentGamesInWonGames, percentGamesInLostGames)) / 
           sum(c(weightWon, weightLost)))
}

calculatePercentPointsWon = function(Games, wd, daysdifflist, wonIndexes, lostIndexes) {
  df = "%m/%d/%Y"
  
  wonIndexes  = wonIndexes[!is.na(as.numeric(Games$WPts[wonIndexes]))]
  wonIndexes  = wonIndexes[!is.na(as.numeric(Games$LPts[wonIndexes]))]
  lostIndexes = lostIndexes[!is.na(as.numeric(Games$WPts[lostIndexes]))]
  lostIndexes = lostIndexes[!is.na(as.numeric(Games$LPts[lostIndexes]))]  
  
  wonDateDiff  = daysdifflist$DaysDiff[match(Games$Loser[wonIndexes], daysdifflist$Opponents)]
  lostDateDiff = daysdifflist$DaysDiff[match(Games$Winner[lostIndexes], daysdifflist$Opponents)]
  weightWon  = wd ^ as.numeric(wonDateDiff)
  weightLost = wd ^ as.numeric(lostDateDiff)

  percentPointsInWonGames  = weightWon * as.numeric(Games$WPts[wonIndexes]) / 
    (as.numeric(Games$WPts[wonIndexes]) + as.numeric(Games$LPts[wonIndexes]))
  percentPointsInLostGames = weightLost * as.numeric(Games$LPts[lostIndexes]) / 
    (as.numeric(Games$LPts[lostIndexes]) + as.numeric(Games$WPts[lostIndexes]))

  percentPoints = sum(c(percentPointsInWonGames, percentPointsInLostGames)) / 
    sum(c(weightWon, weightLost))
  if (length(percentPoints) == 0) {
    return(NA)
  } else {
    return(percentPoints)
  }
}

getLastYearsGames = function(Games, name, date, maxdays, won) {
  df = "%m/%d/%Y"
  Mindate = date - maxdays
  
  if(won == 1) {
    previousGames = grep(name, Games$Winner)
  } else {
    previousGames = grep(name, Games$Loser)
  }
  
  previousDates = as.Date(as.character(Games$Date[previousGames]), format = df)
  
  previousGames[previousDates >= Mindate]
}