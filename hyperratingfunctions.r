source("formulas.r")

GetRatings = function(offset, power, constant) {
  train_rating = read.table("Data/datasets/train_rating.csv", header = T, sep = ",", quote = "\"",
                            stringsAsFactors = FALSE, fill = TRUE)
  train_model = read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"", 
                           stringsAsFactors = FALSE, fill = TRUE)
  cv = read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                  stringsAsFactors = FALSE, fill = TRUE)
  
  train_rating = RemoveWalkOvers(train_rating)
  train_model = RemoveWalkOvers(train_model)
  cv = RemoveWalkOvers(cv)
  
  Nt_r = nrow(train_rating)
  Nt_m = nrow(train_model)
  Ncv = nrow(cv)
  
  Ntot = Nt_r + Nt_m + Ncv
  
  allData = dplyr::bind_rows(train_rating, train_model, cv)
  
  #######Create Ratings for all players and Start initializing them######
  rating = InitializeRating(allData$Winner, allData$Loser)
  
  #Update ratings with train_rating
  for (i in 1: Nt_r) {
    winner = allData$Winner[i]
    loser = allData$Loser[i]
    surface = allData$Surface[i]
    
    rating = UpdateRating(rating, winner, loser, surface, offset, power, constant)
  }
  
  #Create extra variables to analyze later
  allData$Winner_expectation = rep(NA, Ntot)
  allData$Loser_expectation = rep(NA, Ntot)
  
  allData$Winner_games = rep(NA, Ntot)
  allData$Loser_games = rep(NA, Ntot)
  allData$Uncertainty = rep(NA, Ntot)
  
  allData$Winner_rating = rep(NA, Ntot)
  allData$Winner_ratingClay = rep(NA, Ntot)
  allData$Winner_ratingHard = rep(NA, Ntot)
  allData$Winner_ratingGrass = rep(NA, Ntot)
  
  allData$Loser_rating = rep(NA, Ntot)
  allData$Loser_ratingClay = rep(NA, Ntot)
  allData$Loser_ratingHard = rep(NA, Ntot)
  allData$Loser_ratingGrass = rep(NA, Ntot)
  
  for(i in (Nt_r + 1): Ntot) {
    winner = allData$Winner[i]
    loser = allData$Loser[i]
    surface = allData$Surface[i]
    
    indexWinner = match(winner, rating$Players)
    indexLoser = match(loser, rating$Players)
    
    allData$Winner_games[i] = rating$games[indexWinner]
    allData$Loser_games[i] = rating$games[indexLoser]
    
    if(allData$Winner_games[i] == 0 | allData$Loser_games[i] == 0) {
      allData$Uncertainty[i] = 2
    } else {
      allData$Uncertainty[i] = 1 / (allData$Winner_games[i] * allData$Loser_games[i])
    }
    
    expectationWinner = 1 - 1 / (1 + 10 ^ ((rating$Ratings[indexWinner] 
                                            - rating$Ratings[indexLoser])/ 400))
    expectationLoser = 1 - expectationWinner
    
    allData$Winner_expectation[i] = expectationWinner
    allData$Loser_expectation[i] = expectationLoser
    
    allData$Winner_rating[i] = rating$Ratings[indexWinner]
    allData$Winner_ratingClay[i] = rating$Clay_Ratings[indexWinner]
    allData$Winner_ratingHard[i] = rating$Hard_Ratings[indexWinner]
    allData$Winner_ratingGrass[i] = rating$Grass_Ratings[indexWinner]

    
    allData$Loser_rating[i] = rating$Ratings[indexLoser]
    allData$Loser_ratingClay[i] = rating$Clay_Ratings[indexLoser]
    allData$Loser_ratingHard[i] = rating$Hard_Ratings[indexLoser]
    allData$Loser_ratingGrass[i] = rating$Grass_Ratings[indexLoser]
    
    #update rating
    rating = UpdateRating(rating, winner, loser, surface, offset, power, constant)
  }
  
  return(allData[(Nt_r + 1):Ntot, ])
}


CalculateExpectation <- function(rating, surface, winner, loser) {
  
  indexWinner = match(winner, rating$Players)
  indexLoser = match(loser, rating$Players)
  
  ratingWinner = rating$Ratings[indexWinner]
  ratingLoser = rating$Ratings[indexLoser]
  
  weight_rating = 0.71
  weight_surfacerating = 0.29
  
  ##All surfaces if more than 10 games
  #if(rating$ [indexWinner] > 10 & ratingSurface$games[indexLoser]){
  
  if(surface == "Clay" & rating$Clay_games[indexWinner] > 10 & rating$Clay_games[indexLoser]){
    avgRatingWinner = weight_rating * ratingWinner + weight_surfacerating * rating$Clay_Ratings[indexWinner]
    avgRatingLoser = weight_rating * ratingLoser + weight_surfacerating * rating$Clay_Ratings[indexLoser]
    return (Expectation(avgRatingWinner - avgRatingLoser))
  } else if(surface == "Hard"& rating$Hard_games[indexWinner] > 10 & rating$Hard_games[indexLoser]){
    avgRatingWinner = weight_rating * ratingWinner + weight_surfacerating * rating$Hard_Ratings[indexWinner]
    avgRatingLoser = weight_rating * ratingLoser + weight_surfacerating * rating$Hard_Ratings[indexLoser]
    return (Expectation(avgRatingWinner - avgRatingLoser))
  } else if(surface == "Grass"& rating$Grass_games[indexWinner] > 10 & rating$Grass_games[indexLoser]){
    avgRatingWinner = weight_rating * ratingWinner + weight_surfacerating * rating$Grass_Ratings[indexWinner]
    avgRatingLoser = weight_rating * ratingLoser + weight_surfacerating * rating$Grass_Ratings[indexLoser]
    return (Expectation(avgRatingWinner - avgRatingLoser))
  }
  #} else {
  #  avgRatingWinner = ratingWinner
  #  avgRatingLoser = ratingLoser
  #}
  
  
  
  return (Expectation(ratingWinner - ratingLoser))
}

UpdateRating <- function(rating, winner, loser, surface, offset, power, constant) {
  
  indexWinner = match(winner, rating$Players)
  indexLoser = match(loser, rating$Players)
  
  #Normal ratings
  rating$Ratings = UpdateThisRatingType(rating$Ratings, rating$games, indexWinner, indexLoser, 
                                        offset, power, constant)
  rating$games = AddAGame(rating$games, indexWinner, indexLoser)
  
  if(is.na(surface)) {
    surface = "Missing"
  }
  
  #Surface Ratings
  if(surface == "Hard") {
    rating$Hard_Ratings = UpdateThisRatingType(rating$Hard_Ratings, rating$Hard_games, indexWinner, indexLoser,
                                               offset, power, constant)
    rating$Hard_games = AddAGame(rating$Hard_games, indexWinner, indexLoser)
    
  }else if(surface == "Grass") {
    rating$Grass_Ratings = UpdateThisRatingType(rating$Grass_Ratings, rating$Grass_games, indexWinner, indexLoser,
                                                offset, power, constant)
    rating$Grass_games = AddAGame(rating$Grass_games, indexWinner, indexLoser)
    
  } else if(surface == "Clay") {
    rating$Clay_Ratings = UpdateThisRatingType(rating$Clay_Ratings, rating$Clay_games, indexWinner, indexLoser,
                                               offset, power, constant)
    rating$Clay_games = AddAGame(rating$Clay_games, indexWinner, indexLoser)
    
  } 
  
  return(rating)
}

UpdateThisRatingType <- function(Ratings, games, indexWinner, indexLoser, offset, power, constant) {
  Kwinner = K(games[indexWinner], offset, power, constant)
  Kloser = K(games[indexLoser], offset, power, constant)

  ratingWinner = Ratings[indexWinner]
  ratingLoser = Ratings[indexLoser]
  
  expectationWinner = 1 - 1 / (1 + 10 ^ ((ratingWinner - ratingLoser)/ 400))
  expectationLoser = 1 - expectationWinner
  
  Ratings[indexWinner] = NewRating(ratingWinner, Kwinner, expectationWinner, 1)
  Ratings[indexLoser] = NewRating(ratingLoser, Kloser, expectationLoser, 0)
  
  return(Ratings)
}

AddAGame <- function(games, indexWinner, indexLoser) {
  games[indexWinner] = games[indexWinner] + 1
  games[indexLoser] = games[indexLoser] + 1
  
  return(games)
}

NewRating <- function(ratingPlayer, KPlayer, expectationPlayer, result) {
  return(ratingPlayer + KPlayer * (result - expectationPlayer))
}  

#K <- function(numberOfGames, offset, power, constant) {
#  return (constant / (numberOfGames + offset) ^ power)
#}

K <- function(numberOfGames, offset, power, constant) {
    return (constant)
  }

Expectation <- function(diff) {
  return (1 - 1 / (1 + 10 ^ (diff / 400)))
}


#Just ctrl-c ctrl-ved this one, need to check it maybe
LogLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

RemoveWalkOvers = function(Data){
  Data = Data[Data$Comment != "Walkover", ]
  Data = Data[Data$Comment != "Walover", ]
  
  return(Data)
}

InitializeRating = function(winners, losers){
  winners_all = as.data.frame(matrix(nrow = length(winners), ncol = 0))
  winners_all$Players = winners
  
  losers_all = as.data.frame(matrix(nrow = length(losers), ncol = 0))
  losers_all$Players = losers
  
  rating = rbind(winners_all, losers_all)
  rating = unique(rating)
  
  numberOfPlayers = length(rating[, ])
  
  #Add the moment I do not see the value of IDs, maybe if I care later I can add it again
  #Add ID numbers
  #rating$id = rep(0, numberOfPlayers)
  
  #for(i in 1 : numberOfPlayers) {
  #Search in winners
  #indexPlayer = match(rating$Players[i], allData$winner_name)
  #if(!is.na(indexPlayer)) {
  #  rating$id[i] = allData$winner_id[indexPlayer]
  #  next()
  #}
  
  #Search in losers
  #indexPlayer = match(rating$Players[i], allData$loser_name)
  #if(!is.na(indexPlayer)) {
  #  rating$id[i] = allData$loser_id[indexPlayer]
  #}
  #}
  
  #Add start rating and number of games
  rating$Ratings = rep(1500, numberOfPlayers)
  rating$games = rep(0, numberOfPlayers)
  
  #Create Surface Ratings
  rating$Hard_Ratings = rating$Ratings
  rating$Hard_games = rating$games
  
  rating$Grass_Ratings = rating$Ratings
  rating$Grass_games = rating$games
  
  rating$Clay_Ratings = rating$Ratings
  rating$Clay_games = rating$games
  
  return(rating)
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
  
  return(abs(as.numeric(date2-date1)))
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