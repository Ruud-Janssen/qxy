#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)

rm(list = ls())
source("formulas.r")

allGames <- getAllGamesWithoutRating()
# not relevant, player <- getPlayers()

Nall = nrow(allGames)

allGames$HeadtoHead = rep(0, Nall)
allGames$HeadtoHeadMatches = rep(0, Nall)
allGames$LastHeadtoHead = rep(0, Nall)

result_head_to_head <- as.data.frame(distinct(allGames, Match))
result_head_to_head <- result_head_to_head %>%
    mutate(
      HeadtoHead = 0,
      LastHeadtoHead = 0,
      HeadtoHeadMatches = 0
    )

for(i in 1: Nall) {
    
  r <- which(result_head_to_head$Match == allGames$Match[i]) 
  if (length(r) == 1) {
    # set values from previous results
    allGames$HeadtoHead[i] = result_head_to_head$HeadtoHead[r]
    allGames$HeadtoHeadMatches[i] = result_head_to_head$HeadtoHeadMatches[r]
    allGames$LastHeadtoHead[i] = result_head_to_head$LastHeadtoHead[r]
         
    result_head_to_head[r, "HeadtoHead"] <- result_head_to_head[r, "HeadtoHead"] + allGames$Result[i]
    result_head_to_head[r, "HeadtoHeadMatches"] <- result_head_to_head[r, "HeadtoHeadMatches"] + 1
    result_head_to_head[r, "LastHeadtoHead"] <- allGames$Result[i]
  } else { "error, row cannot be found or more rows exists with same match" }
}

saveDatasetsWithoutRating(allGames)