source("formulas.r")
source("addratingsformulas.r")

GetRatings = function(offset, power, constant) {
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
      #    allGames$Winner_skillBo5PlusScores[i]                <- getBo5SkillBasedOnRating(rating$Bo5PlusScore[row_nr_winner], rating$Bo3PlusScore[row_nr_winner], rating$Bo5_games[row_nr_winner], rating$Bo3_games[row_nr_winner])
      #    allGames$Winner_skillBo3PlusScores[i]                <-  - Games$Winner_skillBo5PlusScores[i]
      
      allGames$Loser_rating[i]                             <- rating$Ratings[row_nr_loser]
      allGames$Loser_ratingClay[i]                         <- rating$Clay_Ratings[row_nr_loser]
      allGames$Loser_ratingHard[i]                         <- rating$Hard_Ratings[row_nr_loser]
      allGames$Loser_ratingGrass[i]                        <- rating$Grass_Ratings[row_nr_loser]
      allGames$Loser_ratingNotHard[i]                      <- rating$NotHard_Ratings[row_nr_loser]
      allGames$Loser_ratingBo3[i]                          <- rating$Bo3_Ratings[row_nr_loser]
      allGames$Loser_ratingBo5[i]                          <- rating$Bo5_Ratings[row_nr_loser]
      allGames$Loser_skillBo5[i]                           <- getBo5vsBo3Skill(rating$Bo5_games_won[row_nr_loser], rating$Bo3_games_won[row_nr_loser], rating$Bo5_games[row_nr_loser], rating$Bo3_games[row_nr_loser])
      allGames$Loser_skillBo3[i]                           <-  - allGames$Loser_skillBo5[i]
      #   allGames$Loser_skillBo5PlusScores[i]                 <- getBo5SkillBasedOnRating(rating$Bo5PlusScore[row_nr_loser], rating$Bo3PlusScore[row_nr_loser], rating$Bo5_games[row_nr_loser], rating$Bo3_games[row_nr_loser])
      #   allGames$Loser_skillBo3PlusScores[i]                 <-  - Games$Loser_skillBo5PlusScores[i]
      
      
      # Update rating        
      allGames$Winner_expectationBasedOnRating[i]          <- getWinExpectationBasedOnRating(rating$Ratings[row_nr_winner], rating$Ratings[row_nr_loser])
      allGames$Loser_expectationBasedOnRating[i]           <- getWinExpectationBasedOnRating(rating$Ratings[row_nr_winner], rating$Ratings[row_nr_loser], perspective = "loser")
      
      rating$Ratings[row_nr_winner]                        <- calculateNewRating(rating$Ratings[row_nr_winner], rating$games[row_nr_winner], allGames$Winner_expectationBasedOnRating[i], 1, offset, power, constant)
      rating$Ratings[row_nr_loser]                         <- calculateNewRating(rating$Ratings[row_nr_loser] , rating$games[row_nr_loser] , allGames$Loser_expectationBasedOnRating[i] , 0, offset, power, constant)
      
      # Update games         
      rating$games[row_nr_winner]                          <- rating$games[row_nr_winner] + 1
      rating$games[row_nr_loser]                           <- rating$games[row_nr_loser]  + 1
      rating$games_won[row_nr_winner]                      <- rating$games_won[row_nr_winner] + 1
      
      # bo3 and bo5 ratings  
      if (allGames$Best.of[i] == 3) {  
        allGames$UncertaintyBestOf[i]                      <- getUncertainty(rating$Bo3_games[row_nr_winner], rating$Bo3_games[row_nr_loser])
        
        allGames$Winner_expectationBestOfBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Bo3_Ratings[row_nr_winner], rating$Bo3_Ratings[row_nr_loser])
        allGames$Loser_expectationBestOfBasedOnRating[i]   <- getWinExpectationBasedOnRating(rating$Bo3_Ratings[row_nr_winner], rating$Bo3_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Bo3_Ratings[row_nr_winner]                  <- calculateNewRating(rating$Bo3_Ratings[row_nr_winner], rating$Bo3_games[row_nr_winner], allGames$Winner_expectationBestOfBasedOnRating[i], 1, offset, power, constant)
        rating$Bo3_Ratings[row_nr_loser]                   <- calculateNewRating(rating$Bo3_Ratings[row_nr_loser] , rating$Bo3_games[row_nr_loser] , allGames$Loser_expectationBestOfBasedOnRating[i] , 0, offset, power, constant)
        
        rating$Bo3_games[row_nr_winner]                    <- rating$Bo3_games[row_nr_winner] + 1
        rating$Bo3_games[row_nr_loser]                     <- rating$Bo3_games[row_nr_loser]  + 1
        rating$Bo3_games_won[row_nr_winner]                <- rating$Bo3_games_won[row_nr_winner] + 1
      } else {        
        allGames$UncertaintyBestOf[i]                      <- getUncertainty(rating$Bo5_games[row_nr_winner], rating$Bo5_games[row_nr_loser])
        
        allGames$Winner_expectationBestOfBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Bo5_Ratings[row_nr_winner], rating$Bo5_Ratings[row_nr_loser])
        allGames$Loser_expectationBestOfBasedOnRating[i]   <- getWinExpectationBasedOnRating(rating$Bo5_Ratings[row_nr_winner], rating$Bo5_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Bo5_Ratings[row_nr_winner]                  <- calculateNewRating(rating$Bo5_Ratings[row_nr_winner], rating$Bo5_games[row_nr_winner], allGames$Winner_expectationBestOfBasedOnRating[i], 1, offset, power, constant)
        rating$Bo5_Ratings[row_nr_loser]                   <- calculateNewRating(rating$Bo5_Ratings[row_nr_loser] , rating$Bo5_games[row_nr_loser] , allGames$Loser_expectationBestOfBasedOnRating[i] , 0, offset, power, constant)
        
        rating$Bo5_games[row_nr_winner]                    <- rating$Bo5_games[row_nr_winner] + 1
        rating$Bo5_games[row_nr_loser]                     <- rating$Bo5_games[row_nr_loser]  + 1
        rating$Bo5_games_won[row_nr_winner]                <- rating$Bo5_games_won[row_nr_winner] + 1
      }  
      
      # surface dependent variables
      if (allGames$Surface[i] == "Hard") {
        allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Hard_games[row_nr_winner], rating$Hard_games[row_nr_loser])
        
        allGames$Winner_expectationSurfaceBasedOnRating[i] <- getWinExpectationBasedOnRating(rating$Hard_Ratings[row_nr_winner], rating$Hard_Ratings[row_nr_loser])
        allGames$Loser_expectationSurfaceBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Hard_Ratings[row_nr_winner], rating$Hard_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Hard_Ratings[row_nr_winner]                 <- calculateNewRating(rating$Hard_Ratings[row_nr_winner], rating$Hard_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[i], 1, offset, power, constant)
        rating$Hard_Ratings[row_nr_loser]                  <- calculateNewRating(rating$Hard_Ratings[row_nr_loser], rating$Hard_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[i], 0, offset, power, constant)
        
        rating$Hard_games[row_nr_winner]                   <- rating$Hard_games[row_nr_winner] + 1
        rating$Hard_games[row_nr_loser]                    <- rating$Hard_games[row_nr_loser]  + 1
        rating$Hard_games_won[row_nr_winner]               <- rating$Hard_games_won[row_nr_winner] + 1
      } else { # Not Hard
        Winner_expectationNotHardSurfaceBasedOnRating      <- getWinExpectationBasedOnRating(rating$NotHard_Ratings[row_nr_winner], rating$NotHard_Ratings[row_nr_loser])
        Loser_expectationNotHardSurfaceBasedOnRating       <- getWinExpectationBasedOnRating(rating$NotHard_Ratings[row_nr_winner], rating$NotHard_Ratings[row_nr_loser], perspective = "loser")
        
        rating$NotHard_Ratings[row_nr_winner]              <- calculateNewRating(rating$NotHard_Ratings[row_nr_winner], rating$NotHard_games[row_nr_winner], Winner_expectationNotHardSurfaceBasedOnRating, 1, offset, power, constant)
        rating$NotHard_Ratings[row_nr_loser]               <- calculateNewRating(rating$NotHard_Ratings[row_nr_loser], rating$NotHard_games[row_nr_loser], Loser_expectationNotHardSurfaceBasedOnRating, 0, offset, power, constant)
        rm(Winner_expectationNotHardSurfaceBasedOnRating, Loser_expectationNotHardSurfaceBasedOnRating)
        
        rating$NotHard_games[row_nr_winner]                <- rating$NotHard_games[row_nr_winner] + 1
        rating$NotHard_games[row_nr_loser]                 <- rating$NotHard_games[row_nr_loser]  + 1
        rating$NotHard_games_won[row_nr_winner]            <- rating$NotHard_games_won[row_nr_winner] + 1
        
      }
      if(allGames$Surface[i] == "Grass") {
        allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Grass_games[row_nr_winner], rating$Grass_games[row_nr_loser])
        
        allGames$Winner_expectationSurfaceBasedOnRating[i] <- getWinExpectationBasedOnRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_Ratings[row_nr_loser])
        allGames$Loser_expectationSurfaceBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Grass_Ratings[row_nr_winner]                <- calculateNewRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[i], 1, offset, power, constant)
        rating$Grass_Ratings[row_nr_loser]                 <- calculateNewRating(rating$Grass_Ratings[row_nr_loser], rating$Grass_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[i], 0, offset, power, constant)
        
        rating$Grass_games[row_nr_winner]                  <- rating$Grass_games[row_nr_winner] + 1
        rating$Grass_games[row_nr_loser]                   <- rating$Grass_games[row_nr_loser]  + 1
        rating$Grass_games_won[row_nr_winner]              <- rating$Grass_games_won[row_nr_winner] + 1
        
      } else if(allGames$Surface[i] == "Clay") {
        allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Clay_games[row_nr_winner], rating$Clay_games[row_nr_loser])
        
        allGames$Winner_expectationSurfaceBasedOnRating[i] <- getWinExpectationBasedOnRating(rating$Clay_Ratings[row_nr_winner], rating$Clay_Ratings[row_nr_loser])
        allGames$Loser_expectationSurfaceBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Clay_Ratings[row_nr_winner], rating$Clay_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Clay_Ratings[row_nr_winner]                 <- calculateNewRating(rating$Clay_Ratings[row_nr_winner], rating$Clay_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[i], 1, offset, power, constant)
        rating$Clay_Ratings[row_nr_loser]                  <- calculateNewRating(rating$Clay_Ratings[row_nr_loser], rating$Clay_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[i], 0, offset, power, constant)
        
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
  
  #OLD CODE IN COMMENT but already defined elsewhere, expectationWinner = 1 - 1 / (1 + 10 ^ ((ratingWinner - ratingLoser)/ 400))
  new_rating <- current_rating + Kfactor * (result - expectation_based_on_rating)
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

K <- function(numberOfGames, offset, power, constant) {
  #constant
  constant / (offset + numberOfGames) ^ power
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