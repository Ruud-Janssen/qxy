library(dplyr)

UpdateRating <- function(rating, matchDetails, expectationWinner) {
  expectationLoser = 1 - expectationWinner

  #Normal ratings
  rating$Ratings = UpdateThisRatingType(rating$Ratings, rating$games, 
                                        matchDetails$IndexWinner, matchDetails$IndexLoser)
  rating$games = addAGame(rating$games, matchDetails$IndexWinner, matchDetails$IndexLoser)
  
  if(is.na(matchDetails$Surface)) {
    surface = "Missing"
  }
                                         
  #Surface Ratings
  if(matchDetails$Surface == "Hard") {
    rating$Hard_Ratings = UpdateThisRatingType(rating$Hard_Ratings, rating$Hard_games
                                               , matchDetails$IndexWinner, matchDetails$IndexLoser)
    rating$Hard_games = addAGame(rating$Hard_games, 
                                 matchDetails$IndexWinner, matchDetails$IndexLoser)
  } else{
    rating$NotHard_Ratings = UpdateThisRatingType(rating$NotHard_Ratings, rating$NotHard_games
                                               , matchDetails$IndexWinner, matchDetails$IndexLoser)
    rating$NotHard_games = addAGame(rating$NotHard_games, 
                                 matchDetails$IndexWinner, matchDetails$IndexLoser)
  }
  if(matchDetails$Surface == "Grass") {
      rating$Grass_Ratings = UpdateThisRatingType(rating$Grass_Ratings, rating$Grass_games,
                                                  matchDetails$IndexWinner, matchDetails$IndexLoser)
      rating$Grass_games = addAGame(rating$Grass_games, matchDetails$IndexWinner, matchDetails$IndexLoser)
      
    } else if(matchDetails$Surface == "Clay") {
      rating$Clay_Ratings = UpdateThisRatingType(rating$Clay_Ratings, rating$Clay_games, 
                                                 matchDetails$IndexWinner, matchDetails$IndexLoser)
      rating$Clay_games = addAGame(rating$Clay_games, matchDetails$IndexWinner, matchDetails$IndexLoser)
    
    } else if(matchDetails$Surface == "Carpet") {
      rating$Carpet_Ratings = UpdateThisRatingType(rating$Carpet_Ratings, rating$Carpet_games, 
                                                   matchDetails$IndexWinner, matchDetails$IndexLoser)
      rating$Carpet_games = addAGame(rating$Carpet_games, matchDetails$IndexWinner, matchDetails$IndexLoser)
    }
  
  if(is.na(matchDetails$Best.of)) {
    matchDetails$Best.of = 0
  }
  
  if(matchDetails$Best.of == 3){
    rating$Bo3_Ratings = UpdateThisRatingType(rating$Bo3_Ratings, rating$Bo3_games
                                               , matchDetails$IndexWinner, matchDetails$IndexLoser)
    rating$Bo3_games = addAGame(rating$Bo3_games, 
                                 matchDetails$IndexWinner, matchDetails$IndexLoser)
    
    rating$Bo3Played[matchDetails$IndexWinner] = rating$Bo3Played[matchDetails$IndexWinner] + 1
    rating$Bo3Played[matchDetails$IndexLoser] = rating$Bo3Played[matchDetails$IndexLoser] + 1
    
    rating$Bo3Won[matchDetails$IndexWinner] = rating$Bo3Won[matchDetails$IndexWinner] + 1
    
    rating$Bo3PlusScore[matchDetails$IndexWinner] = rating$Bo3PlusScore[matchDetails$IndexWinner] +
      (1 - expectationWinner)
    
    rating$Bo3PlusScore[matchDetails$IndexLoser] = rating$Bo3PlusScore[matchDetails$IndexLoser] -
        expectationLoser
    
  } else if(matchDetails$Best.of == 5) {
    rating$Bo5_Ratings = UpdateThisRatingType(rating$Bo5_Ratings, rating$Bo5_games
                                              , matchDetails$IndexWinner, matchDetails$IndexLoser)
    rating$Bo5_games = addAGame(rating$Bo5_games, 
                                matchDetails$IndexWinner, matchDetails$IndexLoser)
    
    rating$Bo5Played[matchDetails$IndexWinner] = rating$Bo5Played[matchDetails$IndexWinner] + 1
    rating$Bo5Played[matchDetails$IndexLoser] = rating$Bo5Played[matchDetails$IndexLoser] + 1
    
    rating$Bo5Won[matchDetails$IndexWinner] = rating$Bo5Won[matchDetails$IndexWinner] + 1
    
    rating$Bo5PlusScore[matchDetails$IndexWinner] = rating$Bo5PlusScore[matchDetails$IndexWinner] +
      (1 - expectationWinner)
    
    rating$Bo5PlusScore[matchDetails$IndexLoser] = rating$Bo5PlusScore[matchDetails$IndexLoser] -
        expectationLoser
  }

  return(rating)
}

UpdateThisRatingType <- function(Ratings, games, indexWinner, indexLoser) {
  Kwinner = K(games[indexWinner])
  Kloser = K(games[indexLoser])
  
  ratingWinner = Ratings[indexWinner]
  ratingLoser = Ratings[indexLoser]
  
  expectationWinner = 1 - 1 / (1 + 10 ^ ((ratingWinner - ratingLoser)/ 400))
  expectationLoser = 1 - expectationWinner
  
  Ratings[indexWinner] = NewRating(ratingWinner, Kwinner, expectationWinner, 1)
  Ratings[indexLoser] = NewRating(ratingLoser, Kloser, expectationLoser, 0)

  return(Ratings)
}

addAGame <- function(games, indexWinner, indexLoser) {
  games[indexWinner] = games[indexWinner] + 1
  games[indexLoser] = games[indexLoser] + 1
  
  return(games)
}

NewRating <- function(ratingPlayer, KPlayer, expectationPlayer, result) {
  ratingPlayer + KPlayer * (result - expectationPlayer)
}  

K <- function(numberOfGames) {
  #Got changed after found out that constant 20.6 is better when you remove a lot of the games
  #return (250 / (numberOfGames + 12) ^ 0.44)
  25
}

Expectation <- function(diff) {
  1 - 1 / (1 + 10 ^ (diff / 400))
}


#Just ctrl-c ctrl-ved this one, need to check it maybe
LogLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

RemoveWalkOvers = function(Data){
  Data = Data[Data$Comment != "Walkover", ]
  Data = Data[Data$Comment != "Walover", ]
}

getMatchDetails = function(game){
  matchDetails = list()
  
  matchDetails$Winner = game$Winner
  matchDetails$Loser = game$Loser
  matchDetails$Surface = game$Surface
  matchDetails$Best.of = game$Best.of
  matchDetails$Date = as.Date(as.character(game$Date), format = "%m/%d/%Y")  
  
  return(matchDetails)
}

addUncertaintyAndGames = function(Games, i, matchDetails){
  Games$Winner_games[i] = matchDetails$Winner_games
  Games$Loser_games[i] = matchDetails$Loser_games
  
  if (Games$Winner_games[i] == 0 | Games$Loser_games[i] == 0) {
    Games$Uncertainty[i] = 2
  } else {
    Games$Uncertainty[i] = 1 / (Games$Winner_games[i] * Games$Loser_games[i])
  }
  
  if (Games$Winner_games[i] == 0 | Games$Loser_games[i] == 0) {
    Games$Uncertainty2[i] = 2
  } else {
    Games$Uncertainty2[i] = 1 / min(Games$Winner_games[i], Games$Loser_games[i])
  }
  return(Games)
}

#returns a vector containing the next game if available and 
#an index indicating whether it's the winner (1), loser (2)
#or no next game (0)
FindNextGame = function(name, winners, losers, nrPreviousGames){
  nextWin = match(name, winners)
  nextLoss = match(name, losers)
  
  nextGame = list()
  
  if(is.na(nextWin) & is.na(nextLoss)){
    nextGame$number = NA
    nextGame$player = 0
    return(nextGame)
  }
  
  if(is.na(nextWin) & !is.na(nextLoss)) {
    nextGame$number = nextLoss + nrPreviousGames
    nextGame$player = 2
    return(nextGame)
  } else if(!is.na(nextWin) & is.na(nextLoss)){
    nextGame$number = nextWin + nrPreviousGames
    nextGame$player = 1
    return(nextGame)
  }
  
  if(nextWin < nextLoss) {
    nextGame$number = nextWin + nrPreviousGames
    nextGame$player = 1
  } else if (nextLoss < nextWin){
    nextGame$number = nextLoss + nrPreviousGames
    nextGame$player = 2
  }
  return(nextGame)
}

FindDaysDiff = function(dates){
  date1 = as.Date(as.character(dates[1]), format = "%m/%d/%Y" )
  date2 = as.Date(as.character(dates[2]), format = "%m/%d/%Y" )
  
  abs(as.numeric(date2-date1))
}
FindnextGameSamePlayers = function(winner, loser, winners, losers, previousMatches) {
  winner_innext_winner = previousMatches + which(winners %in% winner) 
  winner_innext_loser = previousMatches + which(losers %in% winner) 
  
  loser_innext_winner = previousMatches + which(winners %in% loser) 
  loser_innext_loser = previousMatches + which(losers %in% loser) 
  
  sameOrders = intersect(winner_innext_winner, loser_innext_loser)
  oppOrders = intersect(winner_innext_loser, loser_innext_winner)
  
  sameOrders = sameOrders[1]
  oppOrders = oppOrders[1]
  
  nextGame = list()
  
  if(!is.na(sameOrders) & !is.na(oppOrders)) {
    if(sameOrders < oppOrders) {
      nextGame$Number = sameOrders
      nextGame$Order = 1
    } else {
      nextGame$Number = oppOrders
      nextGame$Order = 2
    }
  } else if(!is.na(sameOrders)) {
    nextGame$Number = sameOrders
    nextGame$Order = 1
  } else if(!is.na(oppOrders)) {
    nextGame$Number = oppOrders
    nextGame$Order = 2
  } else {
    nextGame$Order = 0
  }  
  
  return(nextGame)
}

getAllGamesWithoutRating = function() {
  train_rating = read.table("Data/datasets/train_rating.csv", header = T, sep = ",", quote = "\"",
                            colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  train_model = read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                           colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  cv = read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                  colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  test = read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                    colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

  allGames = dplyr::bind_rows(train_rating, train_model, cv, test)
  
  #everything is now character, however some columns need to be numeric
  if ("Result" %in% colnames(allGames)) {
    allGames <- mutate(allGames, Result = as.numeric(Result)) 
  }
  
  if ("HeadtoHead" %in% colnames(allGames)) {
    allGames <- mutate(allGames, 
                    HeadtoHead = as.numeric(HeadtoHead),
                    HeadtoHeadMatches = as.numeric(HeadtoHeadMatches),
                    LastHeadtoHead = as.numeric(LastHeadtoHead)
                ) 
  }
  return(allGames)
  
}

getAllGamesWithRating = function() {
  train_rating = read.table("Data/datasets/train_ratingWithRatings.csv", header = T, sep = ",", quote = "\"",
                            colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  train_model = read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                           colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  cv = read.table("Data/datasets/cvWithRatings.csv", header = T, sep = ",", quote = "\"", 
                  colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  test = read.table("Data/datasets/testWithRatings.csv", header = T, sep = ",", quote = "\"",
                    colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  
  dplyr::bind_rows(train_rating, train_model, cv, test)
}

getPlayers = function() {
  read.table("Data/datasets/players.csv", header = T, sep = ",", quote = "\"",
                            colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
}

saveDatasetsWithoutRating = function(allGames){
  
  Nt_r = nrow(read.table("Data/datasets/train_rating.csv",  header = T, sep = ",", quote = "\"",
                                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE))
  Nt_m = nrow(read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE))
  Ncv = nrow(read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                                        colClasses = "character", stringsAsFactors = FALSE, fill = TRUE))
  Ntest = nrow(read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                                          colClasses = "character", stringsAsFactors = FALSE, fill = TRUE))
  
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
  
  write.csv(file = "Data/datasets/train_rating.csv", train_rating, row.names=FALSE)
  write.csv(file = "Data/datasets/train_model.csv", train_model, row.names=FALSE)
  write.csv(file = "Data/datasets/cv.csv", cv, row.names=FALSE)
  write.csv(file = "Data/datasets/test.csv", test, row.names=FALSE)
}

#Note that we assume here that the walkovers are removed
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
  
  if(!is.null(rating)) {
    write.csv(file = "Data/datasets/ratingafterTest.csv", 
              rating, row.names=FALSE)
  }
}

savePlayers = function(player) {
  write.csv(file = "Data/datasets/players.csv", player, row.names=FALSE)
}
