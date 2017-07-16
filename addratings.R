rm(list = ls())
source("formulas.r")
source("addratingsformulas.r")

#K = 20.6

allGames = getAllGamesWithoutRating()

#######Create Ratings for all players and Start initializing them######
rating = InitializeRating(allGames$Winner, allGames$Loser)

allGames = RemoveWalkOvers(allGames)
allGames = InitializeRatingVariablesForGames(allGames)

Nall = nrow(allGames)

for (i in 1: Nall) {
  matchDetails = getMatchDetailsRating(allGames[i, ], rating)
  
  allGames = addUncertaintyAndGames(allGames, rating, i, matchDetails)
  allGames = addRatingVariables(allGames, rating, i, matchDetails)
  allGames = addHomePlayers(allGames, rating, i, matchDetails)
  allGames = addSkillsBoX(allGames, rating, i, matchDetails)
  
  rating = UpdateRating(rating, matchDetails, allGames$Winner_expectationBasedOnRating[i])
}

saveDatasetsWithRating(allGames, rating)
