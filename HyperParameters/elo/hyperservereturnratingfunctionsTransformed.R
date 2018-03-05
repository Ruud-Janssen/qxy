source("formulas.r")
source("addratingsformulas.r")

GetServeReturnRatings = function(offset, power, constant, winPlusScore = 0) {
  powerHard    <- power
  constantHard <- constant
  
  
  train_rating = read.table("Data/datasets/train_ratingWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)
  train_model = read.table("Data/datasets/train_modelWithRatings.csv"
                           , header = T, sep = ",", quote = "\"", fill = TRUE)
  
  Nt_r = nrow(train_rating)
  Nt_m = nrow(train_model)
  
  Ntot = Nt_r + Nt_m
  
  allGames = rbind(train_rating, train_model)
  
  
  #######Create Ratings for all players and Start initializing them######
  
  player   <- getPlayers()
  rating   <- InitializeRatingServeReturn(player)
  
  #Update ratings with train_rating
  allGames$w_svpt   <- as.numeric(allGames$w_svpt)
  allGames$w_1stWon <- as.numeric(allGames$w_1stWon)
  allGames$w_2ndWon <- as.numeric(allGames$w_2ndWon)
  
  allGames$l_svpt   <- as.numeric(allGames$l_svpt)
  allGames$l_1stWon <- as.numeric(allGames$l_1stWon)
  allGames$l_2ndWon <- as.numeric(allGames$l_2ndWon)
  
  allGames$Winner_serverpercentwon <- NA
  allGames$Winner_returnpercentwon  <- NA
  
  allGames$Winner_serve_expectation   <- NA 
  allGames$Winner_return_expectation  <- NA
  allGames$Winner_serve_expectation_surface  <- NA
  allGames$Winner_return_expectation_surface <- NA
  
  Nall <- nrow(allGames)
  
  for (i in 1: Nall) {
    # get matching winner and loserplayer in rating and save the rownr
    # get matching winner and loserplayer in rating and save the rownr
    row_nr_winner <- which(rating$id == allGames$idWinner[i])
    row_nr_loser  <- which(rating$id == allGames$idLoser[i])
    
    if (row_nr_winner > 0 & row_nr_loser > 0) {
      # NOTE: allGames = addHomePlayers(allGames, rating, i, matchDetails) NOT YET CONVERTED!!!!  
      
      # calculate surface independent variables
      allGames$Winner_serverating[i]                       <- rating$ServeRatings[row_nr_winner]
      allGames$Winner_returnrating[i]                      <- rating$ReturnRatings[row_nr_winner]
      allGames$Winner_serveratingHard[i]                   <- rating$Hard_ServeRatings[row_nr_winner]
      allGames$Winner_returnratingHard[i]                  <- rating$Hard_ReturnRatings[row_nr_winner]
      
      allGames$Loser_serverating[i]                        <- rating$ServeRatings[row_nr_loser]
      allGames$Loser_returnrating[i]                       <- rating$ReturnRatings[row_nr_loser]
      allGames$Loser_serveratingHard[i]                    <- rating$Hard_ServeRatings[row_nr_loser]
      allGames$Loser_returnratingHard[i]                   <- rating$Hard_ReturnRatings[row_nr_loser]
      
      #Add Country and Dummy Home
      #allGames$Country[i]        <- as.character(cityToCountry$country[match(allGames$Location[i], 
      #                                                                       cityToCountry$city)])
      #allGames$Winner_country[i] <- rating$Country[row_nr_winner]
      #allGames$Loser_country[i]  <- rating$Country[row_nr_loser]
      #allGames$WinnerisHome[i]   <- as.numeric(allGames$Country[i] == allGames$Winner_country[i])
      #allGames$LoserisHome[i]    <- as.numeric(allGames$Country[i] == allGames$Loser_country[i])
      
      #Handed
      #allGames$Winner_hand[i] <- rating$Handed[row_nr_winner]
      #allGames$Loser_hand[i]  <- rating$Handed[row_nr_loser] 
      
      #Unfortunately some NAs, because of 4 players whose country is not identified
      #if(is.na(allGames$WinnerisHome[i])) {
      #  allGames$WinnerisHome[i] = 0
      #}
      #if(is.na(allGames$LoserisHome[i])) {
      #  allGames$LoserisHome[i] = 0
      #}
      
      numberOfGames      <- calculateGames(allGames[i, ])
      if(numberOfGames <= 10) {
        next()
      }
      
      if (is.na(allGames$w_svpt[i]) | is.na(allGames$w_1stWon[i]) | is.na(allGames$w_2ndWon[i]) |
          is.na(allGames$l_svpt[i]) | is.na(allGames$l_1stWon[i]) | is.na(allGames$l_2ndWon[i])) {
        
        winner_serve_expectation                            <- getWinExpectationBasedOnRating(rating$ServeRatings[row_nr_winner], rating$ReturnRatings[row_nr_loser])
        winner_return_expectation                           <- getWinExpectationBasedOnRating(rating$ReturnRatings[row_nr_winner], rating$ServeRatings[row_nr_loser])
        
        ResultServeWinner <- winner_serve_expectation + winPlusScore
        ResultReturnLoser <- 1 - ResultServeWinner 
        
        ResultsReturnWinner <- winner_return_expectation + winPlusScore
        ResultsReturnLoser  <- 1 - ResultsReturnWinner
        
        rating$ServeRatings[row_nr_winner]                  <- calculateNewRating(rating$ServeRatings[row_nr_winner], rating$games[row_nr_winner], winner_serve_expectation, ResultServeWinner, offset, power, constant)
        rating$ReturnRatings[row_nr_loser]                  <- calculateNewRating(rating$ReturnRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_serve_expectation , ResultReturnLoser, offset, power, constant)
        
        rating$ReturnRatings[row_nr_winner]                 <- calculateNewRating(rating$ReturnRatings[row_nr_winner], rating$games[row_nr_winner], winner_return_expectation, ResultsReturnWinner, offset, power, constant)
        rating$ServeRatings[row_nr_loser]                   <- calculateNewRating(rating$ServeRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_return_expectation , ResultsServeLoser, offset, power, constant)
        
        allGames$Winner_serve_expectation_surface[i]                <- winner_serve_expectation 
        allGames$Winner_return_expectation_surface[i]               <- winner_return_expectation
        
        # Update games         
        rating$games[row_nr_winner]                         <- rating$games[row_nr_winner] + 1
        rating$games[row_nr_loser]                          <- rating$games[row_nr_loser]  + 1
        rating$games_won[row_nr_winner]                     <- rating$games_won[row_nr_winner] + 1
        
        # surface dependent variables
        if (allGames$Surface[i] == "Hard") {
          
          winner_serve_expectation                            <- getWinExpectationBasedOnRating(rating$Hard_ServeRatings[row_nr_winner], rating$Hard_ReturnRatings[row_nr_loser])
          winner_return_expectation                           <- getWinExpectationBasedOnRating(rating$Hard_ReturnRatings[row_nr_winner], rating$Hard_ServeRatings[row_nr_loser])
          
          ResultServeWinner <- winner_serve_expectation + winPlusScore
          ResultReturnLoser <- 1 - ResultServeWinner 
          
          ResultsReturnWinner <- winner_return_expectation + winPlusScore
          ResultsReturnLoser  <- 1 - ResultsReturnWinner
          
          rating$Hard_ServeRatings[row_nr_winner]                  <- calculateNewRating(rating$Hard_ServeRatings[row_nr_winner], rating$games[row_nr_winner], winner_serve_expectation, ResultServeWinner, offset, power, constant)
          rating$Hard_ReturnRatings[row_nr_loser]                  <- calculateNewRating(rating$Hard_ReturnRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_serve_expectation , ResultReturnLoser, offset, power, constant)
          
          rating$Hard_ReturnRatings[row_nr_winner]                 <- calculateNewRating(rating$Hard_ReturnRatings[row_nr_winner], rating$games[row_nr_winner], winner_return_expectation, ResultsReturnWinner, offset, power, constant)
          rating$Hard_ServeRatings[row_nr_loser]                   <- calculateNewRating(rating$Hard_ServeRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_return_expectation , ResultsServeLoser, offset, power, constant)
          
          rating$Hard_games[row_nr_winner]                   <- rating$Hard_games[row_nr_winner] + 1
          rating$Hard_games[row_nr_loser]                    <- rating$Hard_games[row_nr_loser]  + 1
          rating$Hard_games_won[row_nr_winner]               <- rating$Hard_games_won[row_nr_winner] + 1
          
          allGames$Winner_serve_expectation_surface[i]                <- winner_serve_expectation 
          allGames$Winner_return_expectation_surface[i]               <- winner_return_expectation
        } 
        next()  
      }
      
      if(allGames$w_svpt[i] == 0 | allGames$l_svpt[i] == 0) {
        next()
      }
      
      
      
      WinnerPercentServeWon  <- (allGames$w_1stWon[i] + allGames$w_2ndWon[i]) / allGames$w_svpt[i]
      LoserPercentServeWon   <- (allGames$l_1stWon[i] + allGames$l_2ndWon[i]) / allGames$l_svpt[i]
      
      allGames$Winner_serverpercentwon[i] <- WinnerPercentServeWon
      allGames$Winner_returnpercentwon[i]  <- 1 - LoserPercentServeWon
      
      ResultServeWinner   <- WinnerPercentServeWon
      ResultReturnLoser   <- 1 - ResultServeWinner
      
      ResultsServeLoser   <- LoserPercentServeWon
      ResultsReturnWinner <-  1 - ResultsServeLoser
      
      allGames$ResultServeWinner[i]  <- ResultServeWinner
      allGames$ResultReturnWinner[i] <- ResultsReturnWinner
      
      
      # Update rating

      
      winner_serve_expectation                            <- getWinExpectationBasedOnRating(rating$ServeRatings[row_nr_winner], rating$ReturnRatings[row_nr_loser])
      winner_return_expectation                           <- getWinExpectationBasedOnRating(rating$ReturnRatings[row_nr_winner], rating$ServeRatings[row_nr_loser])
      
      rating$ServeRatings[row_nr_winner]                  <- calculateNewRating(rating$ServeRatings[row_nr_winner], rating$games[row_nr_winner], winner_serve_expectation, ResultServeWinner, offset, power, constant)
      rating$ReturnRatings[row_nr_loser]                  <- calculateNewRating(rating$ReturnRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_serve_expectation , ResultReturnLoser, offset, power, constant)
      
      rating$ReturnRatings[row_nr_winner]                 <- calculateNewRating(rating$ReturnRatings[row_nr_winner], rating$games[row_nr_winner], winner_return_expectation, ResultsReturnWinner, offset, power, constant)
      rating$ServeRatings[row_nr_loser]                   <- calculateNewRating(rating$ServeRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_return_expectation , ResultsServeLoser, offset, power, constant)
      
      allGames$Winner_serve_expectation[i]                <- winner_serve_expectation 
      allGames$Winner_return_expectation[i]               <- winner_return_expectation
      
      # Update games         
      rating$games[row_nr_winner]                         <- rating$games[row_nr_winner] + 1
      rating$games[row_nr_loser]                          <- rating$games[row_nr_loser]  + 1
      rating$games_won[row_nr_winner]                     <- rating$games_won[row_nr_winner] + 1
      
      # surface dependent variables
      if (allGames$Surface[i] == "Hard") {
        
        winner_serve_expectation                            <- getWinExpectationBasedOnRating(rating$Hard_ServeRatings[row_nr_winner], rating$Hard_ReturnRatings[row_nr_loser])
        winner_return_expectation                           <- getWinExpectationBasedOnRating(rating$Hard_ReturnRatings[row_nr_winner], rating$Hard_ServeRatings[row_nr_loser])
        
        rating$Hard_ServeRatings[row_nr_winner]                  <- calculateNewRating(rating$Hard_ServeRatings[row_nr_winner], rating$games[row_nr_winner], winner_serve_expectation, ResultServeWinner, offset, power, constant)
        rating$Hard_ReturnRatings[row_nr_loser]                  <- calculateNewRating(rating$Hard_ReturnRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_serve_expectation , ResultReturnLoser, offset, power, constant)
        
        rating$Hard_ReturnRatings[row_nr_winner]                 <- calculateNewRating(rating$Hard_ReturnRatings[row_nr_winner], rating$games[row_nr_winner], winner_return_expectation, ResultsReturnWinner, offset, power, constant)
        rating$Hard_ServeRatings[row_nr_loser]                   <- calculateNewRating(rating$Hard_ServeRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_return_expectation , ResultsServeLoser, offset, power, constant)
        
        rating$Hard_games[row_nr_winner]                   <- rating$Hard_games[row_nr_winner] + 1
        rating$Hard_games[row_nr_loser]                    <- rating$Hard_games[row_nr_loser]  + 1
        rating$Hard_games_won[row_nr_winner]               <- rating$Hard_games_won[row_nr_winner] + 1
        
        allGames$Winner_serve_expectation_surface[i]                <- winner_serve_expectation 
        allGames$Winner_return_expectation_surface[i]               <- winner_return_expectation
      } 
      
    } else {
      print("ERROR: Player cannot be matched with Rating")
    }
  }
  
  return(allGames[(Nt_r + 1):Ntot, ])
}

InitializeRating = function(player){
  #player = SetContinentsAndNationalities(player)
  rating <- InitializeRatingVariables(player)
  
}

InitializeRatingVariables = function(rating){
  numberOfPlayers = nrow(rating)
  
  #Add start rating and number of games
  rating$Ratings = rep(1500, numberOfPlayers)
  rating$games = rep(0, numberOfPlayers)
  rating$games_won = rep(0, numberOfPlayers)
  
  #Create Speciality Ratings
  rating$Hard_Ratings = rating$Ratings
  rating$Hard_games = rating$games
  rating$Hard_games = rating$games
  rating$Hard_games_won = rating$games
  
  rating$Grass_Ratings = rating$Ratings
  rating$Grass_games = rating$games
  rating$Grass_games_won = rating$games
  
  rating$Clay_Ratings = rating$Ratings
  rating$Clay_games = rating$games
  rating$Clay_games_won = rating$games
  
  rating$NotHard_Ratings = rating$Ratings
  rating$NotHard_games =  rating$games
  rating$NotHard_games_won =  rating$games
  
  rating$Bo3_Ratings =  rating$Ratings
  rating$Bo3_games = rating$games
  rating$Bo3_games_won = rating$games
  
  rating$Bo5_Ratings = rating$Ratings
  rating$Bo5_games = rating$games
  rating$Bo5_games_won = rating$games
  
  return(rating)
}

calculateNewRating <- function(current_rating, total_matches, expectation_based_on_rating, result, offset, power, constant) {
  Kfactor <- K(total_matches, offset, power, constant)
  plusScoreMultiplier  <- ifelse((result - expectation_based_on_rating) > 0, 1, -1) 
  plusScoreTransformed <- tanh(abs(result - expectation_based_on_rating) * 25 - 2.3) + 1
  
  new_rating <- current_rating + Kfactor * plusScoreMultiplier * plusScoreTransformed 
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
  plusScore <- result - expectationPlayer
  
  return(ratingPlayer + KPlayer * (plusScore))
}  

K <- function(numberOfGames, offset, power, constant) {
  constant / (offset + numberOfGames) ^ power
  constant
}



Expectation <- function(diff) {
  return (1 - 1 / (1 + 10 ^ (diff / 400)))
}


#Just ctrl-c ctrl-ved this one, need to check it maybe
LogLoss = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps) 
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

RemoveWalkOvers = function(Data){
  Data = Data[Data$Comment != "Walkover", ]
  Data = Data[Data$Comment != "Walover", ]
  
  return(Data)
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

getWinExpectationBasedOnRating = function(rating_winner, rating_loser, perspective = "winner"){
  ratingdiff = (rating_winner - rating_loser) 
  
  Winner_expectationBasedOnRating = 1 - 1 / (1 + 10 ^ ((ratingdiff) / 400))
  if (perspective == "loser") { 1 - Winner_expectationBasedOnRating } else { Winner_expectationBasedOnRating }
}


calculateFractionNetBreakGamesWinnerWon <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  percentWonBreakGames <- 0.5 + (wonGames - lostGames) / (wonGames + lostGames)
}

calculateFractionGamesWinnerWon <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  percentWonGames <- wonGames / (wonGames + lostGames)
}


calculateGames <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  Games <- wonGames + lostGames
}
