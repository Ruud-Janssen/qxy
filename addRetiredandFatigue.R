rm(list = ls())
source("formulas.r")
source("addRetiredandFatigueformulas.r")

#HyperParameters
maxdays = 5
base = 0.86
gamesBarier = 0

allGames = getAllGamesWithoutRating()
allGames = CreateRetiredWalkoverAndFatigue(allGames)
saveDatasetsWithoutRating(allGames)
