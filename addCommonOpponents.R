source("formulas.r")
source("addCommonOpponentsFormulas.R")

#HyperParameters
maxdays = 365

allGames = getAllGamesWithRating()
allGames = InitializeCommonOpponentsVariables(allGames)

Nstart = nrow(read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = TRUE, fill = TRUE))

for (i in Nstart: nrow(allGames)) {
  matchDetails = getMatchDetails(allGames[i, ])
  allGames = setCommonOpponentVariables(allGames, i, matchDetails, maxdays)
}

saveDatasetsWithRating(allGames, rating)