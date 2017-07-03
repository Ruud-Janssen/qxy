#####The idea of this file is to add the current ratings for players for each match,
#####Also the Walkovers will be deleted from the data, this is about 0.7% of the data


rm(list = ls())
source("D:/Betting/Tennis/formulas.r")

allData = read.table("D:/Betting/Tennis/Data/all_unaltered.csv", header = T, sep = ",", 
                        quote = "\"", fill = TRUE)


#######Create Ratings for all players and Start initializing them######
rating = InitializeRating(allData$Winner, allData$Loser)

train_rating = read.table("D:/Betting/Tennis/Data/train_rating.csv", header = T, sep = ",", 
                          quote = "\"", fill = TRUE)
train_rating = RemoveWalkOvers(train_rating)



Nt_r = nrow(train_rating)

#Update ratings with train_rating
for (i in 1: Nt_r) {
  winner = train_rating$Winner[i]
  loser = train_rating$Loser[i]
  surface = train_rating$Surface[i]
  
  rating = UpdateRating(rating, winner, loser, surface)
}

#store ratings in a file
write.csv(file = "D:/Betting/Tennis/Data/ratingaftertrain.csv", 
          rating, row.names=FALSE)

rm(list = setdiff(ls(), lsf.str()))


######Train Model, store ratings in train_model and create ratings to use for Cross Validation ######
ratingTrainModel = read.table("D:/Betting/Tennis/Data/ratingaftertrain.csv", header = T, sep = ",", 
           quote = "\"", fill = TRUE)


#Store ratings for train_model and after update the rating
train_model = read.table("D:/Betting/Tennis/Data/train_model.csv", header = T, sep = ",", 
                         quote = "\"", fill = TRUE)
train_model = RemoveWalkOvers(train_model)

Nt = nrow(train_model)

#Create extra variables to analyze later
train_model$Winner_expectation = rep(NA, Nt)
train_model$Loser_expectation = rep(NA, Nt)

train_model$Winner_games = rep(NA, Nt)
train_model$Loser_games = rep(NA, Nt)
train_model$Uncertainty = rep(NA, Nt)
train_model$Uncertainty2 = rep(NA, Nt)

train_model$Winner_rating = rep(NA, Nt)
train_model$Winner_ratingClay = rep(NA, Nt)
train_model$Winner_ratingHard = rep(NA, Nt)
train_model$Winner_ratingGrass = rep(NA, Nt)
train_model$Winner_ratingCarpet = rep(NA, Nt)

train_model$Loser_rating = rep(NA, Nt)
train_model$Loser_ratingClay = rep(NA, Nt)
train_model$Loser_ratingHard = rep(NA, Nt)
train_model$Loser_ratingGrass = rep(NA, Nt)
train_model$Loser_ratingCarpet = rep(NA, Nt)

for(i in 1: Nt) {
  winner = train_model$Winner[i]
  loser = train_model$Loser[i]
  surface = train_model$Surface[i]
  
  indexWinner = match(winner, ratingTrainModel$Players)
  indexLoser = match(loser, ratingTrainModel$Players)
  
  train_model$Winner_games[i] = ratingTrainModel$games[indexWinner]
  train_model$Loser_games[i] = ratingTrainModel$games[indexLoser]
  
  if(train_model$Winner_games[i] == 0 | train_model$Loser_games[i] == 0) {
    train_model$Uncertainty[i] = 2
  } else {
    train_model$Uncertainty[i] = 1 / (train_model$Winner_games[i] * train_model$Loser_games[i])
  }
  
  if(train_model$Winner_games[i] == 0 | train_model$Loser_games[i] == 0) {
    train_model$Uncertainty2[i] = 2
  } else {
    train_model$Uncertainty2[i] = 1 / min(train_model$Winner_games[i], train_model$Loser_games[i])
  }
  
  expectationWinner = 1 - 1 / (1 + 10 ^ ((ratingTrainModel$Ratings[indexWinner] 
                                          - ratingTrainModel$Ratings[indexLoser])/ 400))
  expectationLoser = 1 - expectationWinner
  
  train_model$Winner_expectation[i] = expectationWinner
  train_model$Loser_expectation[i] = expectationLoser
  
  train_model$Winner_rating[i] = ratingTrainModel$Ratings[indexWinner]
  train_model$Winner_ratingClay[i] = ratingTrainModel$Clay_Ratings[indexWinner]
  train_model$Winner_ratingHard[i] = ratingTrainModel$Hard_Ratings[indexWinner]
  train_model$Winner_ratingGrass[i] = ratingTrainModel$Grass_Ratings[indexWinner]
  train_model$Winner_ratingCarpet[i] = ratingTrainModel$Carpet_Ratings[indexWinner]
  
  train_model$Loser_rating[i] = ratingTrainModel$Ratings[indexLoser]
  train_model$Loser_ratingClay[i] = ratingTrainModel$Clay_Ratings[indexLoser]
  train_model$Loser_ratingHard[i] = ratingTrainModel$Hard_Ratings[indexLoser]
  train_model$Loser_ratingGrass[i] = ratingTrainModel$Grass_Ratings[indexLoser]
  train_model$Loser_ratingCarpet[i] = ratingTrainModel$Carpet_Ratings[indexLoser]
  
  if(expectationWinner >= 1 | expectationWinner <= 0 ) {
    print(i)
  }
  
  #update rating
  ratingTrainModel = UpdateRating(ratingTrainModel, winner, loser, surface)
}

#store ratings in a file for a while here
write.csv(file = "D:/Betting/Tennis/Data/ratingaftertrain_model.csv", 
          ratingTrainModel, row.names=FALSE)

write.csv(file = "D:/Betting/Tennis/Data/train_modelWithRatings.csv", 
         train_model, row.names=FALSE)

rm(list = setdiff(ls(), lsf.str()))

######cv, store ratings in CV and create ratings to use for test ######
ratingcv = read.table("D:/Betting/Tennis/Data/ratingaftertrain_model.csv", header = T, sep = ",", 
                    quote = "\"", fill = TRUE)

#Store ratings for train_model and after update the rating
cv = read.table("D:/Betting/Tennis/Data/cv.csv", header = T, sep = ",", 
                quote = "\"", fill = TRUE)
cv = RemoveWalkOvers(cv)


Nt = nrow(cv)

#Create extra variables to analyze later
cv$Winner_expectation = rep(NA, Nt)
cv$Loser_expectation = rep(NA, Nt)

cv$Winner_games = rep(NA, Nt)
cv$Loser_games = rep(NA, Nt)
cv$Uncertainty = rep(NA, Nt)
cv$Uncertainty2 = rep(NA, Nt)

cv$Winner_rating = rep(NA, Nt)
cv$Winner_ratingClay = rep(NA, Nt)
cv$Winner_ratingHard = rep(NA, Nt)
cv$Winner_ratingGrass = rep(NA, Nt)
cv$Winner_ratingCarpet = rep(NA, Nt)

cv$Loser_rating = rep(NA, Nt)
cv$Loser_ratingClay = rep(NA, Nt)
cv$Loser_ratingHard = rep(NA, Nt)
cv$Loser_ratingGrass = rep(NA, Nt)
cv$Loser_ratingCarpet = rep(NA, Nt)

for(i in 1: Nt) {
  winner = cv$Winner[i]
  loser = cv$Loser[i]
  surface = cv$Surface[i]
  
  indexWinner = match(winner, ratingcv$Players)
  indexLoser = match(loser, ratingcv$Players)
  
  cv$Winner_games[i] = ratingcv$games[indexWinner]
  cv$Loser_games[i] = ratingcv$games[indexLoser]
  
  if(cv$Winner_games[i] == 0 | cv$Loser_games[i] == 0) {
    cv$Uncertainty[i] = 2
  } else {
    cv$Uncertainty[i] = 1 / (cv$Winner_games[i] * cv$Loser_games[i])
  }
  
  if(cv$Winner_games[i] == 0 | cv$Loser_games[i] == 0) {
    cv$Uncertainty2[i] = 2
  } else {
    cv$Uncertainty2[i] = 1 / min(cv$Winner_games[i], cv$Loser_games[i])
  }
  
  expectationWinner = 1 - 1 / (1 + 10 ^ ((ratingcv$Ratings[indexWinner] 
                                          - ratingcv$Ratings[indexLoser])/ 400))
  expectationLoser = 1 - expectationWinner
  
  cv$Winner_expectation[i] = expectationWinner
  cv$Loser_expectation[i] = expectationLoser
  
  cv$Winner_rating[i] = ratingcv$Ratings[indexWinner]
  cv$Winner_ratingClay[i] = ratingcv$Clay_Ratings[indexWinner]
  cv$Winner_ratingHard[i] = ratingcv$Hard_Ratings[indexWinner]
  cv$Winner_ratingGrass[i] = ratingcv$Grass_Ratings[indexWinner]
  cv$Winner_ratingCarpet[i] = ratingcv$Carpet_Ratings[indexWinner]
  
  cv$Loser_rating[i] = ratingcv$Ratings[indexLoser]
  cv$Loser_ratingClay[i] = ratingcv$Clay_Ratings[indexLoser]
  cv$Loser_ratingHard[i] = ratingcv$Hard_Ratings[indexLoser]
  cv$Loser_ratingGrass[i] = ratingcv$Grass_Ratings[indexLoser]
  cv$Loser_ratingCarpet[i] = ratingcv$Carpet_Ratings[indexLoser]
  
  if(expectationWinner >= 1 | expectationWinner <= 0 ) {
    print(i)
  }
  
  #update rating
  ratingcv = UpdateRating(ratingcv, winner, loser, surface)
}

#store ratings in a file for a while here
write.csv(file = "D:/Betting/Tennis/Data/ratingaftercv.csv", 
          ratingcv, row.names=FALSE)

write.csv(file = "D:/Betting/Tennis/Data/cvWithRatings.csv", 
          cv, row.names=FALSE)

rm(list = setdiff(ls(), lsf.str()))

######Test, store ratings in Test and create finalratings ######
ratingTest = read.table("D:/Betting/Tennis/Data/ratingaftercv.csv", header = T, sep = ",", 
                      quote = "\"", fill = TRUE)

#Store ratings for train_model and after update the rating
Test = read.table("D:/Betting/Tennis/Data/test.csv", header = T, sep = ",", 
                quote = "\"", fill = TRUE)
Test = RemoveWalkOvers(Test)


Nt = nrow(Test)

#Create extra variables to analyze later
Test$Winner_expectation = rep(NA, Nt)
Test$Loser_expectation = rep(NA, Nt)

Test$Winner_games = rep(NA, Nt)
Test$Loser_games = rep(NA, Nt)
Test$Uncertainty = rep(NA, Nt)
Test$Uncertainty2 = rep(NA, Nt)

Test$Winner_rating = rep(NA, Nt)
Test$Winner_ratingClay = rep(NA, Nt)
Test$Winner_ratingHard = rep(NA, Nt)
Test$Winner_ratingGrass = rep(NA, Nt)
Test$Winner_ratingCarpet = rep(NA, Nt)

Test$Loser_rating = rep(NA, Nt)
Test$Loser_ratingClay = rep(NA, Nt)
Test$Loser_ratingHard = rep(NA, Nt)
Test$Loser_ratingGrass = rep(NA, Nt)
Test$Loser_ratingCarpet = rep(NA, Nt)

for(i in 1: Nt) {
  winner = Test$Winner[i]
  loser = Test$Loser[i]
  surface = Test$Surface[i]
  
  indexWinner = match(winner, ratingTest$Players)
  indexLoser = match(loser, ratingTest$Players)
  
  Test$Winner_games[i] = ratingTest$games[indexWinner]
  Test$Loser_games[i] = ratingTest$games[indexLoser]
  
  if(Test$Winner_games[i] == 0 | Test$Loser_games[i] == 0) {
    Test$Uncertainty[i] = 2
  } else {
    Test$Uncertainty[i] = 1 / (Test$Winner_games[i] * Test$Loser_games[i])
  }
  
  if(Test$Winner_games[i] == 0 | Test$Loser_games[i] == 0) {
    Test$Uncertainty2[i] = 2
  } else {
    Test$Uncertainty2[i] = 1 / min(Test$Winner_games[i], Test$Loser_games[i])
  }
  
  expectationWinner = 1 - 1 / (1 + 10 ^ ((ratingTest$Ratings[indexWinner] 
                                          - ratingTest$Ratings[indexLoser])/ 400))
  expectationLoser = 1 - expectationWinner
  
  Test$Winner_expectation[i] = expectationWinner
  Test$Loser_expectation[i] = expectationLoser
  
  Test$Winner_rating[i] = ratingTest$Ratings[indexWinner]
  Test$Winner_ratingClay[i] = ratingTest$Clay_Ratings[indexWinner]
  Test$Winner_ratingHard[i] = ratingTest$Hard_Ratings[indexWinner]
  Test$Winner_ratingGrass[i] = ratingTest$Grass_Ratings[indexWinner]
  Test$Winner_ratingCarpet[i] = ratingTest$Carpet_Ratings[indexWinner]
  
  Test$Loser_rating[i] = ratingTest$Ratings[indexLoser]
  Test$Loser_ratingClay[i] = ratingTest$Clay_Ratings[indexLoser]
  Test$Loser_ratingHard[i] = ratingTest$Hard_Ratings[indexLoser]
  Test$Loser_ratingGrass[i] = ratingTest$Grass_Ratings[indexLoser]
  Test$Loser_ratingCarpet[i] = ratingTest$Carpet_Ratings[indexLoser]
  
  if(expectationWinner >= 1 | expectationWinner <= 0 ) {
    print(i)
  }
  
  #update rating
  ratingTest = UpdateRating(ratingTest, winner, loser, surface)
}

#store ratings in a file for a while here
write.csv(file = "D:/Betting/Tennis/Data/ratingafterTest.csv", 
          ratingTest, row.names=FALSE)

write.csv(file = "D:/Betting/Tennis/Data/testWithRatings.csv", 
          Test, row.names=FALSE)

