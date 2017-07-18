rm(list = ls())
source("formulas.r")
source("addRetiredandFatigueformulas.r")

#HyperParameters
maxdays = 2
base = 0.63
gamesBarier = 24

allGames = getAllGamesWithoutRating()
player <- getPlayers()
allGames = CreateRetiredWalkoverAndFatigue(allGames, maxdays, base, gamesBarier)
saveDatasetsWithoutRating(allGames)
