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

UpdateRating <- function(rating, winner, loser, surface) {
  
  indexWinner = match(winner, rating$Players)
  indexLoser = match(loser, rating$Players)
  
  #Normal ratings
  rating$Ratings = UpdateThisRatingType(rating$Ratings, rating$games, indexWinner, indexLoser)
  rating$games = AddAGame(rating$games, indexWinner, indexLoser)
  
  if(is.na(surface)) {
    surface = "Missing"
  }
                                         
  #Surface Ratings
  if(surface == "Hard") {
    rating$Hard_Ratings = UpdateThisRatingType(rating$Hard_Ratings, rating$Hard_games, indexWinner, indexLoser)
    rating$Hard_games = AddAGame(rating$Hard_games, indexWinner, indexLoser)
       
    }else if(surface == "Grass") {
      rating$Grass_Ratings = UpdateThisRatingType(rating$Grass_Ratings, rating$Grass_games, indexWinner, indexLoser)
      rating$Grass_games = AddAGame(rating$Grass_games, indexWinner, indexLoser)
      
    } else if(surface == "Clay") {
      rating$Clay_Ratings = UpdateThisRatingType(rating$Clay_Ratings, rating$Clay_games, indexWinner, indexLoser)
      rating$Clay_games = AddAGame(rating$Clay_games, indexWinner, indexLoser)
    
    } else if(surface == "Carpet") {
      rating$Carpet_Ratings = UpdateThisRatingType(rating$Carpet_Ratings, rating$Carpet_games, indexWinner, indexLoser)
      rating$Carpet_games = AddAGame(rating$Carpet_games, indexWinner, indexLoser)
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

AddAGame <- function(games, indexWinner, indexLoser) {
  games[indexWinner] = games[indexWinner] + 1
  games[indexLoser] = games[indexLoser] + 1
  
  return(games)
}

NewRating <- function(ratingPlayer, KPlayer, expectationPlayer, result) {
  return(ratingPlayer + KPlayer * (result - expectationPlayer))
}  

K <- function(numberOfGames) {
  #Got changed after found out that constant 20.6 is better when you remove a lot of the games
  #return (250 / (numberOfGames + 12) ^ 0.44)
  return(20.6)
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
  
  rating$Carpet_Ratings = rating$Ratings
  rating$Carpet_games = rating$games
  
  atp_players = read.table("Data/datasets/atp_players.csv", header = T, sep = ",", 
                           quote = "\"", fill = TRUE)
  
  rating$Nationality = rep(NA, numberOfPlayers)
  
  for(i in 1 : length(rating$Players)) {
    name = rating$Players[i]
    if(name == ""){
      next()
    }
    name = unlist(strsplit(as.character(name), ' (?=[^ ]+$)', perl=TRUE))
    lastName = name[1]
    firstName = name[2]
    firstName = gsub("Jr.", "", firstName)

    indexLastName = grep(lastName, atp_players$lastName ,ignore.case=TRUE)
    if(sum(!is.na(indexLastName)) > 0) {
      for(j in 1:length(indexLastName)){
        playerNumberAtp = indexLastName[j]
        if(startsWith(as.character(atp_players$firstName[playerNumberAtp]), substring(firstName, 1, 1))) {
          rating$Nationality[i] = as.character(atp_players$Nationality[playerNumberAtp])
        }
      }
    } else {
      lastName = gsub("-", " ", lastName)
      lastName = gsub("\'", "", lastName)
      if(lastName == "Nadal Parera"){
        lastName = "Nadal"
      }
      if(lastName == "Hantschek") {
        lastName = "Hantschk"
      }
      
      indexLastName = grep(lastName, atp_players$lastName ,ignore.case=TRUE)
      if(sum(!is.na(indexLastName)) > 0) {
        for(j in 1:length(indexLastName)){
          playerNumberAtp = indexLastName[j]
          if(startsWith(as.character(atp_players$firstName[playerNumberAtp]), substring(firstName, 1, 1))) {
            rating$Nationality[i] = as.character(atp_players$Nationality[playerNumberAtp])
          }
        }
      }
    }
  }
  
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