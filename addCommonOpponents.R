library(tictoc)
source("formulas.r")
source("addCommonOpponentsFormulas.R")

#HyperParameters
maxdays = 365

allGames = getAllGamesWithRating()
allGames = InitializeCommonOpponentsVariables(allGames)

Nstart = nrow(read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = TRUE, fill = TRUE))
Nall = nrow(allGames)

tic()
for (i in Nstart: Nall) {
  matchDetails = getMatchDetails(allGames[i, ])
  allGames = setCommonOpponentVariables(allGames, i, matchDetails, maxdays)
}
toc()
#DONT FORGET TO FOKING SAVE
saveDatasetsWithRating(allGames, NULL)