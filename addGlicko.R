library(tictoc)
source("formulas.R")
source("addGlickoFormulas.R")

#HyperParameters
startRating         = 1500
startRD             = 350
#periodLength        = 1 #(day)
rdIncreasePerPeriod = 15.49 #got to optimize this I guess, but assuming 500 days and average 50 15.49 is value


allGames = getAllGamesWithRating()
glicko = InitializeGlicko(allGames$Winner, allGames$Loser, startRating, startRD)
allGames = InitializeGlickoVariablesForGames(allGames)
#allGames = sortGamesByDate(allGames)

for (i in 1: nrow(allGames)) {
  matchDetails = getMatchDetailsGlicko(allGames[i, ], glicko)
  glicko = updateRDBeforeGame(glicko, matchDetails, rdIncreasePerPeriod)
  
  #allGames = addGlickoUncertainty(allGames, glicko, i, matchDetails)
  allGames = addGlickoVariables(allGames, glicko, i, matchDetails)
  
  glicko = UpdateGlicko(glicko, matchDetails)
}

saveDatasetsWithRating(allGames, NULL)
