rm(list = ls())
source("formulas.r")

allGames = getAllGamesWithoutRating()

Nall = nrow(allGames)

allGames$HeadtoHead = rep(0, Nall)
allGames$HeadtoHeadMatches = rep(0, Nall)
allGames$LastHeadtoHead = rep(0, Nall)

for(i in 1: Nall) {
  winner = allGames$Winner[i]
  loser = allGames$Loser[i]
  
  #Next game will have $Number, indicating the next match
  #Next game will have $Order, signifying same orders(1), 
  #opposite orders(2) or no next game(0)
  nextGame = FindnextGameSamePlayers(winner, loser, allGames$Winner[i + 1 : Nall], 
                                     allGames$Loser[i + 1 : Nall], i)
  
  if(nextGame$Order == 1) {
    allGames$HeadtoHead[nextGame$Number] = allGames$HeadtoHead[i] + 1
    allGames$HeadtoHeadMatches[nextGame$Number] = allGames$HeadtoHeadMatches[i] + 1
    allGames$LastHeadtoHead[nextGame$Number] = 1
    } else if(nextGame$Order == 2){ 
      allGames$HeadtoHead[nextGame$Number] = - (allGames$HeadtoHead[i] + 1)
      allGames$HeadtoHeadMatches[nextGame$Number] = allGames$HeadtoHeadMatches[i] + 1
      allGames$LastHeadtoHead[nextGame$Number] = -1
    }
}

saveDatasetsWithoutRating(allGames)
