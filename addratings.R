rm(list = ls())
source("formulas.r")
source("addratingsformulas.r")

startTime <- Sys.time ()

allGames      <- getAllGamesWithoutRating()
player        <- getPlayers()
cityToCountry <- read.table("Data/datasets/citycountry.csv", header = T, sep = ",", quote = "\"", fill = TRUE)

# Create Ratings for all players, ratings are adapted after each match
rating <- InitializeRating(player)
rating <- SetContinentsAndCountries(rating)

allGames <- RemoveWalkOvers(allGames)
allGames <- InitializeRatingVariablesForGames(allGames)

allGames <- allGames %>% filter(atp_match == 1)
allGames <- allGames %>% arrange(Date)

Nall <- nrow(allGames)

for (i in 1: Nall) {
  
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
                   
    allGames$Loser_rating[i]                             <- rating$Ratings[row_nr_loser]
    allGames$Loser_ratingClay[i]                         <- rating$Clay_Ratings[row_nr_loser]
    allGames$Loser_ratingHard[i]                         <- rating$Hard_Ratings[row_nr_loser]
    allGames$Loser_ratingGrass[i]                        <- rating$Grass_Ratings[row_nr_loser]
    allGames$Loser_ratingNotHard[i]                      <- rating$NotHard_Ratings[row_nr_loser]

    
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
    
    # surface dependent variables
    if (allGames$Surface[i] == "Hard") {
      allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Hard_games[row_nr_winner], rating$Hard_games[row_nr_loser])
      } else if(allGames$Surface[i] == "Grass") {
      allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Grass_games[row_nr_winner], rating$Grass_games[row_nr_loser])
    } else if(allGames$Surface[i] == "Clay") {
      allGames$UncertaintySurface[i]                     <- getUncertainty(rating$Clay_games[row_nr_winner], rating$Clay_games[row_nr_loser])
    } else if(allGames$Surface[i] == "Carpet") {
      #Since carpet is not in use since 2009 no carpet variables will be saved
    }
    
    
    #numberOfGames      <- calculateGames(allGames[i, ])
    #if(numberOfGames <= 10) {
    #  next()
    #}
    
    ResultWinner  <- 1
    ResultLoser   <- 1 - ResultWinner
    
    # Update rating        
    allGames$Winner_expectationBasedOnRating[i]          <- getWinExpectationBasedOnRating(rating$Ratings[row_nr_winner], rating$Ratings[row_nr_loser])
    allGames$Loser_expectationBasedOnRating[i]           <- getWinExpectationBasedOnRating(rating$Ratings[row_nr_winner], rating$Ratings[row_nr_loser], perspective = "loser")
    
    rating$Ratings[row_nr_winner]                        <- calculateNewRating(rating$Ratings[row_nr_winner], rating$games[row_nr_winner], allGames$Winner_expectationBasedOnRating[i], ResultWinner)
    rating$Ratings[row_nr_loser]                         <- calculateNewRating(rating$Ratings[row_nr_loser] , rating$games[row_nr_loser] , allGames$Loser_expectationBasedOnRating[i] , ResultLoser)
    
    # Update games         
    rating$games[row_nr_winner]                          <- rating$games[row_nr_winner] + 1
    rating$games[row_nr_loser]                           <- rating$games[row_nr_loser]  + 1
    rating$games_won[row_nr_winner]                      <- rating$games_won[row_nr_winner] + 1

    # surface dependent variables
    if (allGames$Surface[i] == "Hard") {
   
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
 
      allGames$Winner_expectationSurfaceBasedOnRating[i] <- getWinExpectationBasedOnRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_Ratings[row_nr_loser])
      allGames$Loser_expectationSurfaceBasedOnRating[i]  <- getWinExpectationBasedOnRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_Ratings[row_nr_loser], perspective = "loser")
      
      rating$Grass_Ratings[row_nr_winner]                <- calculateNewRating(rating$Grass_Ratings[row_nr_winner], rating$Grass_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[i], 1)
      rating$Grass_Ratings[row_nr_loser]                 <- calculateNewRating(rating$Grass_Ratings[row_nr_loser], rating$Grass_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[i], 0)
      
      rating$Grass_games[row_nr_winner]                  <- rating$Grass_games[row_nr_winner] + 1
      rating$Grass_games[row_nr_loser]                   <- rating$Grass_games[row_nr_loser]  + 1
      rating$Grass_games_won[row_nr_winner]              <- rating$Grass_games_won[row_nr_winner] + 1
    
    } else if(allGames$Surface[i] == "Clay") {
   
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
saveDatasetsWithRating(allGames, rating)

endTime <- Sys.time()

loadtime <- endTime - startTime
print(loadtime)