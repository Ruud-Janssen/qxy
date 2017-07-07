#This functions creates empty lists for locations, ratings and uncertainty
InitializeRatingVariables = function(dataset){
  
  rows = nrow(dataset)
  
  dataset$Country = rep(NA, rows)
  dataset$Winner_home = rep(NA, rows)
  dataset$Loser_home = rep(NA, rows)
  
  dataset$Winner_games = rep(NA, rows)
  dataset$Loser_games = rep(NA, rows)
  dataset$Uncertainty = rep(NA, rows)
  dataset$Uncertainty2 = rep(NA, rows)
  
  dataset$Winner_rating = rep(NA, rows)
  dataset$Winner_ratingClay = rep(NA, rows)
  dataset$Winner_ratingHard = rep(NA, rows)
  dataset$Winner_ratingGrass = rep(NA, rows)
  
  dataset$Loser_rating = rep(NA, rows)
  dataset$Loser_ratingClay = rep(NA, rows)
  dataset$Loser_ratingHard = rep(NA, rows)
  dataset$Loser_ratingGrass = rep(NA, rows)
  
  return(dataset)
}

saveDatasetsWithRating = function(allGames, rating){
  
  Nt_r = nrow(RemoveWalkOvers(read.table("Data/datasets/train_rating.csv",  header = T, sep = ",", quote = "\"",
                                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)))
  Nt_m = nrow(RemoveWalkOvers(read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)))
  Ncv = nrow(RemoveWalkOvers(read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)))
  Ntest = nrow(RemoveWalkOvers(read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                                          colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)))
  
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
}

addRatingVariables = function(Games, rating, i, matchDetails){
  Games$Winner_rating[i] = rating$Ratings[matchDetails$IndexWinner]
  Games$Winner_ratingClay[i] = rating$Clay_Ratings[matchDetails$IndexWinner]
  Games$Winner_ratingHard[i] = rating$Hard_Ratings[matchDetails$IndexWinner]
  Games$Winner_ratingGrass[i] = rating$Grass_Ratings[matchDetails$IndexWinner]
  
  Games$Loser_rating[i] = rating$Ratings[matchDetails$IndexLoser]
  Games$Loser_ratingClay[i] = rating$Clay_Ratings[matchDetails$IndexLoser]
  Games$Loser_ratingHard[i] = rating$Hard_Ratings[matchDetails$IndexLoser]
  Games$Loser_ratingGrass[i] = rating$Grass_Ratings[matchDetails$IndexLoser]
  
  return(Games)
}

getMatchDetails = function(game, rating){
  matchDetails = list()
  
  matchDetails$Winner = game$Winner
  matchDetails$Loser = game$Loser
  matchDetails$Surface = game$Surface
  
  matchDetails$IndexWinner = match(matchDetails$Winner, rating$Players)
  matchDetails$IndexLoser = match(matchDetails$Loser, rating$Players)
  
  matchDetails$Winner_games = rating$games[matchDetails$IndexWinner]
  matchDetails$Loser_games = rating$games[matchDetails$IndexLoser]
  
  return(matchDetails)
}

addUncertaintyAndGames = function(Games, i, matchDetails){
  Games$Winner_games[i] = matchDetails$Winner_games
  Games$Loser_games[i] = matchDetails$Loser_games
  
  if(Games$Winner_games[i] == 0 | Games$Loser_games[i] == 0) {
    Games$Uncertainty[i] = 2
  } else {
    Games$Uncertainty[i] = 1 / (Games$Winner_games[i] * Games$Loser_games[i])
  }
  
  if(Games$Winner_games[i] == 0 | Games$Loser_games[i] == 0) {
    Games$Uncertainty2[i] = 2
  } else {
    Games$Uncertainty2[i] = 1 / min(Games$Winner_games[i], Games$Loser_games[i])
  }
  
  return(Games)
}