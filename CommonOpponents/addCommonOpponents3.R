################### add common opponents with weightDecay, taking the average of the matches between the same
################### person for time difference

library(tictoc)
source("formulas.R")
source("addCommonOpponentsFormulas3.R")
#Parallel
library(parallel)
library(doParallel)

cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = detectCores() - 3)

#HyperParameters
maxdays = 2 * 365
weightDecay = 0.998

allGames = getAllGamesWithRating()
allGames = InitializeCommonOpponentsVariables(allGames)

Nstart = nrow(read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = TRUE, fill = TRUE))
Nall   = nrow(allGames)

tic()
allGames[Nstart : Nall, ] = foreach(i = Nstart : Nall, .combine = rbind) %dopar% {
#for(i in Nstart : Nall){
  source("formulas.R")
  source("addCommonOpponentsFormulas3.R")                          
  matchDetails = getMatchDetails(allGames[i, ])
  row          = setCommonOpponentVariablesRow(allGames, i, matchDetails, maxdays, weightDecay)
}
toc()
stopCluster(cl)
saveDatasetsWithRating(allGames, NULL)