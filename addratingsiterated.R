rm(list = ls())
source("formulas.r")
source("addratingsformulas.r")
library(dplyr)

startTime <- Sys.time ()

allGames <- getAllGamesWithRating()
df <- "%m/%d/%Y"
#df <- "%Y/%m/%d"
allGames$D <- as.Date(allGames$Date, df)
allGames <- allGames %>%  mutate(YearMonth = format.Date(allGames$D, '%Y/%m'))
allGames <- allGames %>%  mutate(date_id = group_indices_(allGames, .dots = c("YearMonth")))


player   <- getPlayers()
cityToCountry <- read.table("Data/datasets/citycountry.csv", header = T, sep = ",", quote = "\"", fill = TRUE)

# Create Ratings for all players, ratings are adapted after each match
rating <- InitializeRating(player)

#allGames <- RemoveWalkOvers(allGames)

Nall <- nrow(allGames)

allGames$Winner_expectationBasedOnRating <- NULL
allGames$Loser_expectationBasedOnRating  <- NULL

allGames$Winner_expectationSurfaceBasedOnRating <- NULL
allGames$Loser_expectationSurfaceBasedOnRating  <- NULL

for(i in unique(allGames$date_id)) {
  
  rating3 <- rating
  rating2 <- rating
  
  for(j in which(allGames$date_id == i)){

    # get matching winner and loserplayer in rating and save the rownr
    row_nr_winner <- which(rating2$id == allGames$idWinner[j])
    row_nr_loser  <- which(rating2$id == allGames$idLoser[j])
    
    #Day to day updates
    if (row_nr_winner > 0 & row_nr_loser > 0) {
      
      
      allGames$Winner_expectationBasedOnRating[j]          <- getWinExpectationBasedOnRating(rating2$Ratings[row_nr_winner], rating2$Ratings[row_nr_loser])
      allGames$Loser_expectationBasedOnRating[j]           <- getWinExpectationBasedOnRating(rating2$Ratings[row_nr_winner], rating2$Ratings[row_nr_loser], perspective = "loser")
      
      
      allGames$Winner_rating[j]                            <- rating2$Ratings[row_nr_winner]
      allGames$Winner_ratingHard[j]                        <- rating2$Hard_Ratings[row_nr_winner]
      
      allGames$Loser_rating[j]                             <- rating2$Ratings[row_nr_loser]
      allGames$Loser_ratingHard[j]                         <- rating2$Hard_Ratings[row_nr_loser]
   
      
      #numberOfGames      <- calculateGames(allGames[j, ])
      #if(numberOfGames <= 10) {
      #  next()
      #}
      
      #FractionNetBreakGamesWinner  <- calculateFractionNetBreakGamesWinnerWon(allGames[j, ])
      
      ResultWinner  <- 1
      ResultLoser   <- 1 - ResultWinner
      
      # Update rating        
      rating2$Ratings[row_nr_winner]                        <- calculateNewRating(rating2$Ratings[row_nr_winner], rating2$games[row_nr_winner], allGames$Winner_expectationBasedOnRating[j], ResultWinner)
      rating2$Ratings[row_nr_loser]                         <- calculateNewRating(rating2$Ratings[row_nr_loser] , rating2$games[row_nr_loser] , allGames$Loser_expectationBasedOnRating[j] , ResultLoser)
      
      # Update games         
      rating2$games[row_nr_winner]                          <- rating2$games[row_nr_winner] + 1
      rating2$games[row_nr_loser]                           <- rating2$games[row_nr_loser]  + 1
      rating2$games_won[row_nr_winner]                      <- rating2$games_won[row_nr_winner] + 1
      
    
      
      # surface dependent variables
      if (allGames$Surface[j] == "Hard") {
        
        allGames$Winner_expectationSurfaceBasedOnRating[j] <- getWinExpectationBasedOnRating(rating2$Hard_Ratings[row_nr_winner], rating2$Hard_Ratings[row_nr_loser])
        allGames$Loser_expectationSurfaceBasedOnRating[j]  <- getWinExpectationBasedOnRating(rating2$Hard_Ratings[row_nr_winner], rating2$Hard_Ratings[row_nr_loser], perspective = "loser")
        
        rating2$Hard_Ratings[row_nr_winner]                 <- calculateNewRating(rating2$Hard_Ratings[row_nr_winner], rating2$Hard_games[row_nr_winner], allGames$Winner_expectationSurfaceBasedOnRating[j], ResultWinner)
        rating2$Hard_Ratings[row_nr_loser]                  <- calculateNewRating(rating2$Hard_Ratings[row_nr_loser], rating2$Hard_games[row_nr_loser], allGames$Loser_expectationSurfaceBasedOnRating[j], ResultLoser)
        
        rating2$Hard_games[row_nr_winner]                   <- rating2$Hard_games[row_nr_winner] + 1
        rating2$Hard_games[row_nr_loser]                    <- rating2$Hard_games[row_nr_loser]  + 1
        rating2$Hard_games_won[row_nr_winner]               <- rating2$Hard_games_won[row_nr_winner] + 1
      } 
    } else {
      print("ERROR: Player cannot be matched with Rating")
    }
  }
  
  
  for(j in which(allGames$date_id  == i)){
    
    # get matching winner and loserplayer in rating and save the rownr
    row_nr_winner <- which(rating$id == allGames$idWinner[j])
    row_nr_loser  <- which(rating$id == allGames$idLoser[j])
    
    if (row_nr_winner > 0 & row_nr_loser > 0) {
      
      winner_exp          <- getWinExpectationBasedOnRating(rating3$Ratings[row_nr_winner], rating2$Ratings[row_nr_loser])
      loser_exp           <- getWinExpectationBasedOnRating(rating2$Ratings[row_nr_winner], rating3$Ratings[row_nr_loser], perspective = "loser")
      
      #numberOfGames      <- calculateGames(allGames[j, ])
      #if(numberOfGames <= 10) {
      #  next()
      #}
      
      #FractionNetBreakGamesWinner  <- calculateFractionNetBreakGamesWinnerWon(allGames[j, ])
      
      ResultWinner  <- 1
      ResultLoser   <- 1 - ResultWinner
      
      # Update rating        
      rating$Ratings[row_nr_winner]                        <- calculateNewRating(rating$Ratings[row_nr_winner], rating$games[row_nr_winner], winner_exp, ResultWinner)
      rating$Ratings[row_nr_loser]                         <- calculateNewRating(rating$Ratings[row_nr_loser] , rating$games[row_nr_loser] , loser_exp , ResultLoser)
      
      # Update games         
      rating$games[row_nr_winner]                          <- rating$games[row_nr_winner] + 1
      rating$games[row_nr_loser]                           <- rating$games[row_nr_loser]  + 1
      rating$games_won[row_nr_winner]                      <- rating$games_won[row_nr_winner] + 1
      
      
      
      # surface dependent variables
      if (allGames$Surface[j] == "Hard") {
        
        winner_surf_exp <- getWinExpectationBasedOnRating(rating3$Hard_Ratings[row_nr_winner], rating2$Hard_Ratings[row_nr_loser])
        loser_surf_exp  <- getWinExpectationBasedOnRating(rating2$Hard_Ratings[row_nr_winner], rating3$Hard_Ratings[row_nr_loser], perspective = "loser")
        
        rating$Hard_Ratings[row_nr_winner]                 <- calculateNewRating(rating$Hard_Ratings[row_nr_winner], rating$Hard_games[row_nr_winner],winner_surf_exp, ResultWinner)
        rating$Hard_Ratings[row_nr_loser]                  <- calculateNewRating(rating$Hard_Ratings[row_nr_loser], rating$Hard_games[row_nr_loser], loser_surf_exp, ResultLoser)
        
        rating$Hard_games[row_nr_winner]                   <- rating$Hard_games[row_nr_winner] + 1
        rating$Hard_games[row_nr_loser]                    <- rating$Hard_games[row_nr_loser]  + 1
        rating$Hard_games_won[row_nr_winner]               <- rating$Hard_games_won[row_nr_winner] + 1
      } 
    } else {
      print("ERROR: Player cannot be matched with Rating")
    }
  }
}
saveDatasetsWithRating(allGames, rating)

endTime <- Sys.time()

loadtime <- endTime - startTime
print(loadtime)