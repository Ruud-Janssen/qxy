library(tictoc)
source("formulas.R")
source("addCommonOpponentsFormulas.R")
#Parallel
library(parallel)
library(foreach)
library(doParallel)

cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = detectCores() - 3)

#HyperParameters
maxdays = 2 * 365

allGames = getAllGamesWithRating()
allGames = InitializeCommonOpponentsVariables(allGames)

Nstart = nrow(read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = TRUE, fill = TRUE))
Nall   = nrow(allGames)

tic()
allGames[Nstart : Nall, ] = foreach(i = Nstart : Nall, .combine = rbind) %dopar% {
#for(i in Nstart : Nall){
  source("formulas.R")
  source("addCommonOpponentsFormulas.R")                          
  matchDetails = getMatchDetails(allGames[i, ])
  row          = setCommonOpponentVariablesRow(allGames, i, matchDetails, maxdays)
}
toc()
stopCluster(cl)
saveDatasetsWithRating(allGames, NULL)