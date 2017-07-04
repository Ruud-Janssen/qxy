rm(list = ls())
source("formulas.r")
library(dplyr)

train_rating = read.table("Data/datasets/train_rating.csv", header = T, sep = ",", quote = "\"",
                          colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
train_model = read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
cv = read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
test = read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                  colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

allGames = dplyr::bind_rows(train_rating, train_model, cv, test)

#######Create Ratings for all players and Start initializing them######
rating = InitializeRating(allGames$Winner, allGames$Loser)

allGames = RemoveWalkOvers(allGames)
Nall = nrow(allGames)

allGames$Country = rep(NA, Nall)
allGames$Winner_home = rep(NA, Nall)
allGames$Loser_home = rep(NA, Nall)

#Create extra variables to analyze later
allGames$Winner_expectation = rep(NA, Nall)
allGames$Loser_expectation = rep(NA, Nall)

allGames$Winner_games = rep(NA, Nall)
allGames$Loser_games = rep(NA, Nall)
allGames$Uncertainty = rep(NA, Nall)
allGames$Uncertainty2 = rep(NA, Nall)

allGames$Winner_rating = rep(NA, Nall)
allGames$Winner_ratingClay = rep(NA, Nall)
allGames$Winner_ratingHard = rep(NA, Nall)
allGames$Winner_ratingGrass = rep(NA, Nall)
allGames$Winner_ratingCarpet = rep(NA, Nall)

allGames$Loser_rating = rep(NA, Nall)
allGames$Loser_ratingClay = rep(NA, Nall)
allGames$Loser_ratingHard = rep(NA, Nall)
allGames$Loser_ratingGrass = rep(NA, Nall)
allGames$Loser_ratingCarpet = rep(NA, Nall)

for(i in 1: Nall) {
  winner = allGames$Winner[i]
  loser = allGames$Loser[i]
  surface = allGames$Surface[i]
  
  indexWinner = match(winner, rating$Players)
  indexLoser = match(loser, rating$Players)
  
  allGames$Winner_games[i] = rating$games[indexWinner]
  allGames$Loser_games[i] = rating$games[indexLoser]
  
  if(allGames$Winner_games[i] == 0 | allGames$Loser_games[i] == 0) {
    allGames$Uncertainty[i] = 2
  } else {
    allGames$Uncertainty[i] = 1 / (allGames$Winner_games[i] * allGames$Loser_games[i])
  }
  
  if(allGames$Winner_games[i] == 0 | allGames$Loser_games[i] == 0) {
    allGames$Uncertainty2[i] = 2
  } else {
    allGames$Uncertainty2[i] = 1 / min(allGames$Winner_games[i], allGames$Loser_games[i])
  }
  
  expectationWinner = 1 - 1 / (1 + 10 ^ ((rating$Ratings[indexWinner] 
                                          - rating$Ratings[indexLoser])/ 400))
  expectationLoser = 1 - expectationWinner
  
  allGames$Winner_expectation[i] = expectationWinner
  allGames$Loser_expectation[i] = expectationLoser
  
  allGames$Winner_rating[i] = rating$Ratings[indexWinner]
  allGames$Winner_ratingClay[i] = rating$Clay_Ratings[indexWinner]
  allGames$Winner_ratingHard[i] = rating$Hard_Ratings[indexWinner]
  allGames$Winner_ratingGrass[i] = rating$Grass_Ratings[indexWinner]
  allGames$Winner_ratingCarpet[i] = rating$Carpet_Ratings[indexWinner]
  
  allGames$Loser_rating[i] = rating$Ratings[indexLoser]
  allGames$Loser_ratingClay[i] = rating$Clay_Ratings[indexLoser]
  allGames$Loser_ratingHard[i] = rating$Hard_Ratings[indexLoser]
  allGames$Loser_ratingGrass[i] = rating$Grass_Ratings[indexLoser]
  allGames$Loser_ratingCarpet[i] = rating$Carpet_Ratings[indexLoser]
  
  if(expectationWinner >= 1 | expectationWinner <= 0 ) {
    print(i)
  }
  
  #allGames = addHomePlayers(allGames, indexWinner, indexLoser, rating)
  
  #update rating
  rating = UpdateRating(rating, winner, loser, surface)
}

Nt_r = nrow(train_rating)
Nt_m = nrow(train_model)
Ncv = nrow(cv)
Ntest = nrow(test)

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

write.csv(file = "Data/datasets/train_ratingWithRatings.csv", train_rating, row.names=FALSE)
write.csv(file = "Data/datasets/train_modelWithRatings.csv", train_model, row.names=FALSE)
write.csv(file = "Data/datasets/cvWithRatings.csv", cv, row.names=FALSE)
write.csv(file = "Data/datasets/testWithRatings.csv", test, row.names=FALSE)

write.csv(file = "Data/datasets/ratingafterTest.csv", 
          rating, row.names=FALSE)
