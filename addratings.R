rm(list = ls())
source("formulas.r")
source("addratingsformulas.r")

startTime <- Sys.time ()

allGames = getAllGamesWithoutRating()
player = getPlayers()

# Create Ratings for all players, ratings are adapted after each match
rating = InitializeRating(player)

allGames = RemoveWalkOvers(allGames)
allGames = InitializeRatingVariablesForGames(allGames)

Nall = nrow(allGames)

for (i in 1: Nall) {
    # get matching winner and loserplayer in rating and save the rownr
  row_nr_winner <- which(rating$id == allGames$idWinner[i])
  row_nr_loser <- which(rating$id == allGames$idLoser[i])

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
         
    rating$Ratings[row_nr_winner]                        <- calculateNewRating(rating$Ratings[row_nr_winner], rating$games[row_nr_winner], allGames$Winner_expectationBasedOnRating[i], 1)
    rating$Ratings[row_nr_loser]                         <- calculateNewRating(rating$Ratings[row_nr_loser] , rating$games[row_nr_loser] , allGames$Loser_expectationBasedOnRating[i] , 0)
         
    # Update games         
    rating$games[row_nr_winner]                          <- rating$games[row_nr_winner] + 1
    rating$games[row_nr_loser]                           <- rating$games[row_nr_loser]  + 1
    rating$games_won[row_nr_winner]                      <- rating$games_won[row_nr_winner] + 1
    
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
      
      rating$Hard_Ratings[row_nr_winner]                 <- calculateNewRating(rating$Hard_Ratings[row_nr_winner], rating$Hard_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[i], 1)
      rating$Hard_Ratings[row_nr_loser]                  <- calculateNewRating(rating$Hard_Ratings[row_nr_loser], rating$Hard_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[i], 0)
  
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
    #Since carpet is not in use since 2009 no carpet variables will be saved
    else if(allGames$Surface[i] == "Carpet") {
    }
  } else {
    print("ERROR: Player cannot be matched with Rating")
  }
}
saveDatasetsWithRating(allGames, rating)

endTime <- Sys.time()

loadtime <- endTime - startTime
print(loadtime)