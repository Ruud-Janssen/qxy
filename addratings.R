rm(list = ls())
source("formulas.r")
source("addratingsformulas.r")

allGames = getAllGamesWithoutRating()

#######Create Ratings for all players and Start initializing them######
rating = InitializeRating(allGames$Winner, allGames$Loser)

allGames = RemoveWalkOvers(allGames)
allGames = InitializeRatingVariables(allGames)

Nall = nrow(allGames)

for(i in 1: Nall) {
  matchDetails = getMatchDetails(allGames[i, ], rating)
  
  allGames = addUncertaintyAndGames(allGames, i, matchDetails)
  allGames = addRatingVariables(allGames, rating, i, matchDetails)
  allGames = addHomePlayers(allGames, rating, i, matchDetails)
  allGames = addSkillsBoX(allGames, rating, i, matchDetails)
  
  rating = UpdateRating(rating, matchDetails, allGames$Winner_expectationBasedOnRating[i])
}

saveDatasetsWithRating(allGames, rating)
