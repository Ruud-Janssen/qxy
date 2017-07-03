rm(list = ls())
source("D:/Betting/Tennis/formulas.r")
library(dplyr)

train_rating = read.table("D:/Betting/Tennis/Data/train_rating.csv", header = T, sep = ",", quote = "\"",
                          colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
train_model = read.table("D:/Betting/Tennis/Data/train_model.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
cv = read.table("D:/Betting/Tennis/Data/cv.csv", header = T, sep = ",", quote = "\"", 
                colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
test = read.table("D:/Betting/Tennis/Data/test.csv", header = T, sep = ",", quote = "\"",
                  colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

Nt_r = nrow(train_rating)
Nt_m = nrow(train_model)
Ncv = nrow(cv)
Ntest = nrow(test)

allGames = dplyr::bind_rows(train_rating, train_model, cv, test)

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

firstindextrain_rating = 1
lastindextrain_rating = Nt_r
train_rating = allGames[firstindextrain_rating : lastindextrain_rating, ]

firstindextrain_model = lastindextrain_rating + 1
lastindextrain_model = lastindextrain_rating + Nt_m
train_model = allGames[firstindextrain_model : lastindextrain_model, ]

firstindexcv = lastindextrain_model + 1
lastindexcv = lastindextrain_model + Ncv
cv = allGames[firstindexcv : lastindexcv, ]

firstindextest = lastindexcv + 1
lastindextest = lastindexcv + Ntest
test = allGames[firstindextest: lastindextest, ]

write.csv(file = "D:/Betting/Tennis/Data/train_rating.csv", train_rating, row.names=FALSE)
write.csv(file = "D:/Betting/Tennis/Data/train_model.csv", train_model, row.names=FALSE)
write.csv(file = "D:/Betting/Tennis/Data/cv.csv", cv, row.names=FALSE)
write.csv(file = "D:/Betting/Tennis/Data/test.csv", test, row.names=FALSE)



