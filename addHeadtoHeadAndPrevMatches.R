#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)

rm(list = ls())
source("formulas.r")

allGames <- getAllGamesWithoutRating()

if("idWinner" %in% colnames(allGames)) {
  allGames <- subset(allGames, select = -c(idWinner) )
}
if("idLoser" %in% colnames(allGames)) {
  allGames <- subset(allGames, select = -c(idLoser) )
}

allGames <- mutate(allGames, Winner2 = trimws(tolower(str_replace_all(Winner, "[^[:alnum:]]", " "))))
allGames <- mutate(allGames, Loser2 = trimws(tolower(str_replace_all(Loser, "[^[:alnum:]]", " "))))
allGames <- allGames %>% filter(Winner2 != "" | Loser2 != "")

Nall = nrow(allGames)

winner <- allGames %>% distinct(Winner2)
names(winner) <- c("playername")
    
loser <- allGames %>% distinct(Loser2)
names(loser) <- c("playername")

player <- distinct(rbind(winner, loser))


player <- arrange(player, playername)
player$id <- seq.int(nrow(player)) 

allGames <- left_join(allGames, player, by = c("Winner2" = "playername") )
allGames <- rename(allGames, idWinner = id)

allGames <- left_join(allGames, player, by = c("Loser2" = "playername") )
allGames <- rename(allGames, idLoser = id)

allGames <- mutate(allGames, Match = ifelse(idWinner < idLoser, paste(idWinner, idLoser, sep="-"), paste(idLoser, idWinner, sep="-")))
allGames <- mutate(allGames, Result = ifelse(idWinner < idLoser, 1, -1))


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

savePlayers(player)

