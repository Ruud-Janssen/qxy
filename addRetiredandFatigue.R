rm(list = ls())
source("formulas.r")
source("addRetiredandFatigueformulas.r")

allGames = getAllGamesWithoutRating()
allGames = CreateRetiredWalkoverAndFatigue(allGames)
saveDatasetsWithoutRating(allGames)
