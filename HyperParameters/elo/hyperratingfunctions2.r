source("formulas.r")
source("addratingsformulas.r")

GetRatings = function(K, winBonus = 0) {
 
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
  rating = InitializeRating(player)
  
  #Update ratings with train_rating
  allGames <- InitializeRatingVariablesForGames(allGames)
  
  for (i in 1: Ntot) {
    # get matching winner and loserplayer in rating and save the rownr
    row_nr_winner <- which(rating$id == allGames$idWinner[i])
    row_nr_loser  <- which(rating$id == allGames$idLoser[i])
    
    if (row_nr_winner > 0 & row_nr_loser > 0) {
      # NOTE: allGames = addHomePlayers(allGames, rating, i, matchDetails) NOT YET CONVERTED!!!!  
      
      # calculate surface independent variables
      allGames$Uncertainty[i]                              <- getUncertainty(rating$games[row_nr_winner], rating$games[row_nr_loser])
      allGames$Uncertainty2[i]                             <- getUncertainty(rating$games[row_nr_winner], rating$games[row_nr_loser], type_uncertainty = 2)
      
      allGames$Winner_rating[i]                            <- rating$Ratings[row_nr_winner]
      allGames$Winner_ratingClay[i]                        <- rating$Clay_Ratings[row_nr_winner]
      allGames$Winner_ratingHard[i]                        <- rating$Hard_Ratings[row_nr_winner]
      allGames$Winner_ratingGrass[i]                       <- rating$Grass_Ratings[row_nr_winner]
      allGames$Winner_ratingNotHard[i]                     <- rating$NotHard_Ratings[row_nr_winner]
      allGames$Winner_ratingBo3[i]                         <- rating$Bo3_Ratings[row_nr_winner]
      allGames$Winner_ratingBo5[i]                         <- rating$Bo5_Ratings[row_nr_winner]
      allGames$Winner_skillBo5[i]                          <- getBo5vsBo3Skill(rating$Bo5_games_won[row_nr_winner], rating$Bo3_games_won[row_nr_winner], rating$Bo5_games[row_nr_winner], rating$Bo3_games[row_nr_winner])
      allGames$Winner_skillBo3[i]                          <-  - allGames$Winner_skillBo5[i]
      allGames$Winner_ratingNewHard[i]                    <- rating$NewHard_Ratings[row_nr_winner]
      
      
      allGames$Loser_rating[i]                             <- rating$Ratings[row_nr_loser]
      allGames$Loser_ratingClay[i]                         <- rating$Clay_Ratings[row_nr_loser]
      allGames$Loser_ratingHard[i]                         <- rating$Hard_Ratings[row_nr_loser]
      allGames$Loser_ratingGrass[i]                        <- rating$Grass_Ratings[row_nr_loser]
      allGames$Loser_ratingNotHard[i]                      <- rating$NotHard_Ratings[row_nr_loser]
      allGames$Loser_ratingBo3[i]                          <- rating$Bo3_Ratings[row_nr_loser]
      allGames$Loser_ratingBo5[i]                          <- rating$Bo5_Ratings[row_nr_loser]
      allGames$Loser_skillBo5[i]                           <- getBo5vsBo3Skill(rating$Bo5_games_won[row_nr_loser], rating$Bo3_games_won[row_nr_loser], rating$Bo5_games[row_nr_loser], rating$Bo3_games[row_nr_loser])
      allGames$Loser_skillBo3[i]                           <-  - allGames$Loser_skillBo5[i]
      allGames$Loser_ratingNewHard[i]                    <- rating$NewHard_Ratings[row_nr_loser]
      
      
      # numberOfGames      <- calculateGames(allGames[i, ])
      # if(numberOfGames <= 10) {
      #   next()
      # }
      # 
      # FractionNetBreakGamesWinner  <- calculateFractionNetBreakGamesWinnerWon(allGames[i, ])
      # 
      ResultWinner  <- 1
      ResultLoser   <- 0
    
      
      
      
      # Update rating        
      allGames$Winner_expectationBasedOnRating[i]          <- getWinExpectationBasedOnRating(rating$Ratings[row_nr_winner], rating$Ratings[row_nr_loser])
      allGames$Loser_expectationBasedOnRating[i]           <- getWinExpectationBasedOnRating(rating$Ratings[row_nr_winner], rating$Ratings[row_nr_loser], perspective = "loser")
      
      rating$Ratings[row_nr_winner]                        <- calculateNewRating(rating$Ratings[row_nr_winner], rating$games[row_nr_winner], allGames$Winner_expectationBasedOnRating[i], ResultWinner)
      rating$Ratings[row_nr_loser]                         <- calculateNewRating(rating$Ratings[row_nr_loser] , rating$games[row_nr_loser] , allGames$Loser_expectationBasedOnRating[i] , ResultLoser)
      
      # Update games         
      rating$games[row_nr_winner]                          <- rating$games[row_nr_winner] + 1
      rating$games[row_nr_loser]                           <- rating$games[row_nr_loser]  + 1
      rating$games_won[row_nr_winner]                      <- rating$games_won[row_nr_winner] + 1
      
      ##
      expW                                                 <- getWinExpectationBasedOnRating(rating$NewHard_Ratings[row_nr_winner], rating$NewHard_Ratings[row_nr_loser])
      expL                                                 <- getWinExpectationBasedOnRating(rating$NewHard_Ratings[row_nr_winner], rating$NewHard_Ratings[row_nr_loser], perspective = "loser")
      if (allGames$Surface[i] == "Hard") {
        
        
        rating$NewHard_Ratings[row_nr_winner]                        <- calculateNewRating(rating$NewHard_Ratings[row_nr_winner], rating$NewHard_games[row_nr_winner], expW, ResultWinner)
        rating$NewHard_Ratings[row_nr_loser]                         <- calculateNewRating(rating$NewHard_Ratings[row_nr_loser] , rating$NewHard_games[row_nr_loser] , expL, ResultLoser)
        
        # Update games         
        rating$NewHard_games[row_nr_winner]                          <- rating$NewHard_games[row_nr_winner] + 1
        rating$NewHard_games[row_nr_loser]                           <- rating$NewHard_games[row_nr_loser]  + 1
      } else {
        
        rating$NewHard_Ratings[row_nr_winner]                        <- calculateNewRating(rating$NewHard_Ratings[row_nr_winner], rating$NewHard_games[row_nr_winner], expW, ResultWinner)
        rating$NewHard_Ratings[row_nr_loser]                         <- calculateNewRating(rating$NewHard_Ratings[row_nr_loser] , rating$NewHard_games[row_nr_loser] , expL, ResultLoser)
        
        # Update games         
        rating$NewHard_games[row_nr_winner]                          <- rating$NewHard_games[row_nr_winner] + 1
        rating$NewHard_games[row_nr_loser]                           <- rating$NewHard_games[row_nr_loser]  + 1
        
      }
        
        
        
      # bo3 and bo5 ratings  
      if (allGames$Best.of[i] == 3) {  
        allGames$UncertaintyBestOf[i]                      <- getUncertainty(rating$Bo3_games[row_nr_winner], rating$Bo3_games[row_nr_loser])
        
        allGames$Winner_expectationBestOfBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Bo3_Ratings[row_nr_winner], rating$Bo3_Ratings[row_nr_loser])
        allGames$Loser_expectationBestOfBasedOnRating[i]   <- getWinExpectationBasedOnRating(rating$Bo3_Ratings[row_nr_winner], rating$Bo3_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Bo3_Ratings[row_nr_winner]                  <- calculateNewRating(rating$Bo3_Ratings[row_nr_winner], rating$Bo3_games[row_nr_winner], allGames$Winner_expectationBestOfBasedOnRating[i], 1)
        rating$Bo3_Ratings[row_nr_loser]                   <- calculateNewRating(rating$Bo3_Ratings[row_nr_loser] , rating$Bo3_games[row_nr_loser] , allGames$Loser_expectationBestOfBasedOnRating[i] , 0)
        
        rating$Bo3_games[row_nr_winner]                    <- rating$Bo3_games[row_nr_winner] + 1
        rating$Bo3_games[row_nr_loser]                     <- rating$Bo3_games[row_nr_loser]  + 1
        rating$Bo3_games_won[row_nr_winner]                <- rating$Bo3_games_won[row_nr_winner] + 1
      } else {        
        allGames$UncertaintyBestOf[i]                      <- getUncertainty(rating$Bo5_games[row_nr_winner], rating$Bo5_games[row_nr_loser])
        
        allGames$Winner_expectationBestOfBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Bo5_Ratings[row_nr_winner], rating$Bo5_Ratings[row_nr_loser])
        allGames$Loser_expectationBestOfBasedOnRating[i]   <- getWinExpectationBasedOnRating(rating$Bo5_Ratings[row_nr_winner], rating$Bo5_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Bo5_Ratings[row_nr_winner]                  <- calculateNewRating(rating$Bo5_Ratings[row_nr_winner], rating$Bo5_games[row_nr_winner], allGames$Winner_expectationBestOfBasedOnRating[i], 1)
        rating$Bo5_Ratings[row_nr_loser]                   <- calculateNewRating(rating$Bo5_Ratings[row_nr_loser] , rating$Bo5_games[row_nr_loser] , allGames$Loser_expectationBestOfBasedOnRating[i] , 0)
        
        rating$Bo5_games[row_nr_winner]                    <- rating$Bo5_games[row_nr_winner] + 1
        rating$Bo5_games[row_nr_loser]                     <- rating$Bo5_games[row_nr_loser]  + 1
        rating$Bo5_games_won[row_nr_winner]                <- rating$Bo5_games_won[row_nr_winner] + 1
      }  
      
      # surface dependent variables
      if (allGames$Surface[i] == "Hard") {
        allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Hard_games[row_nr_winner], rating$Hard_games[row_nr_loser])
        
        allGames$Winner_expectationSurfaceBasedOnRating[i] <- getWinExpectationBasedOnRating(rating$Hard_Ratings[row_nr_winner], rating$Hard_Ratings[row_nr_loser])
        allGames$Loser_expectationSurfaceBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Hard_Ratings[row_nr_winner], rating$Hard_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Hard_Ratings[row_nr_winner]                 <- calculateNewRating(rating$Hard_Ratings[row_nr_winner], rating$Hard_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[i], ResultWinner)
        rating$Hard_Ratings[row_nr_loser]                  <- calculateNewRating(rating$Hard_Ratings[row_nr_loser], rating$Hard_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[i], ResultLoser)
        
        rating$Hard_games[row_nr_winner]                   <- rating$Hard_games[row_nr_winner] + 1
        rating$Hard_games[row_nr_loser]                    <- rating$Hard_games[row_nr_loser]  + 1
        rating$Hard_games_won[row_nr_winner]               <- rating$Hard_games_won[row_nr_winner] + 1
      } else { # Not Hard
        Winner_expectationNotHardSurfaceBasedOnRating      <- getWinExpectationBasedOnRating(rating$NotHard_Ratings[row_nr_winner], rating$NotHard_Ratings[row_nr_loser])
        Loser_expectationNotHardSurfaceBasedOnRating       <- getWinExpectationBasedOnRating(rating$NotHard_Ratings[row_nr_winner], rating$NotHard_Ratings[row_nr_loser], perspective = "loser")
        
        rating$NotHard_Ratings[row_nr_winner]              <- calculateNewRating(rating$NotHard_Ratings[row_nr_winner], rating$NotHard_games[row_nr_winner], Winner_expectationNotHardSurfaceBasedOnRating, 1)
        rating$NotHard_Ratings[row_nr_loser]               <- calculateNewRating(rating$NotHard_Ratings[row_nr_loser], rating$NotHard_games[row_nr_loser], Loser_expectationNotHardSurfaceBasedOnRating, 0)
        rm(Winner_expectationNotHardSurfaceBasedOnRating, Loser_expectationNotHardSurfaceBasedOnRating)
        
        rating$NotHard_games[row_nr_winner]                <- rating$NotHard_games[row_nr_winner] + 1
        rating$NotHard_games[row_nr_loser]                 <- rating$NotHard_games[row_nr_loser]  + 1
        rating$NotHard_games_won[row_nr_winner]            <- rating$NotHard_games_won[row_nr_winner] + 1
        
      }
      if(allGames$Surface[i] == "Grass") {
        allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Grass_games[row_nr_winner], rating$Grass_games[row_nr_loser])
        
        allGames$Winner_expectationSurfaceBasedOnRating[i] <- getWinExpectationBasedOnRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_Ratings[row_nr_loser])
        allGames$Loser_expectationSurfaceBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Grass_Ratings[row_nr_winner]                <- calculateNewRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[i], 1)
        rating$Grass_Ratings[row_nr_loser]                 <- calculateNewRating(rating$Grass_Ratings[row_nr_loser], rating$Grass_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[i], 0)
        
        rating$Grass_games[row_nr_winner]                  <- rating$Grass_games[row_nr_winner] + 1
        rating$Grass_games[row_nr_loser]                   <- rating$Grass_games[row_nr_loser]  + 1
        rating$Grass_games_won[row_nr_winner]              <- rating$Grass_games_won[row_nr_winner] + 1
        
      } else if(allGames$Surface[i] == "Clay") {
        allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Clay_games[row_nr_winner], rating$Clay_games[row_nr_loser])
        
        allGames$Winner_expectationSurfaceBasedOnRating[i] <- getWinExpectationBasedOnRating(rating$Clay_Ratings[row_nr_winner], rating$Clay_Ratings[row_nr_loser])
        allGames$Loser_expectationSurfaceBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Clay_Ratings[row_nr_winner], rating$Clay_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Clay_Ratings[row_nr_winner]                 <- calculateNewRating(rating$Clay_Ratings[row_nr_winner], rating$Clay_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[i], 1)
        rating$Clay_Ratings[row_nr_loser]                  <- calculateNewRating(rating$Clay_Ratings[row_nr_loser], rating$Clay_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[i], 0)
        
        rating$Clay_games[row_nr_winner]                   <- rating$Clay_games[row_nr_winner] + 1
        rating$Clay_games[row_nr_loser]                    <- rating$Clay_games[row_nr_loser]  + 1
        rating$Clay_games_won[row_nr_winner]               <- rating$Clay_games_won[row_nr_winner] + 1
        
      } 
      else if(allGames$Surface[i] == "Carpet") {
        #Since carpet is not in use since 2009 no carpet variables will be saved
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
  
  rating$NewHard_Ratings = rating$Ratings
  rating$NewHard_games = rating$games
  
  return(rating)
}

calculateNewRating <- function(current_rating, total_matches, expectation_based_on_rating, result) {
  Kfactor <- K(total_matches)
  
  #OLD CODE IN COMMENT but already defined elsewhere, expectationWinner = 1 - 1 / (1 + 10 ^ ((ratingWinner - ratingLoser)/ 400))
  new_rating <- current_rating + Kfactor * (result - expectation_based_on_rating)
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
  plusScore <- result - expectationPlayer
  
  return(ratingPlayer + KPlayer * (plusScore))
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
