rm(list = ls())
source("formulas.r")
source("addratingsformulas.r")

startTime <- Sys.time ()

allGames <- getAllGamesWithRating()
player   <- getPlayers()
cityToCountry <- read.table("Data/datasets/citycountry.csv", header = T, sep = ",", quote = "\"", fill = TRUE)

# Create Ratings for all players, ratings are adapted after each match
rating <- InitializeRatingServeReturn(player)
rating <- SetContinentsAndCountries(rating)

allGames <- allGames[allGames$atp_match == 1, ]
allGames <- allGames[order(allGames$Date), ]

allGames$w_svpt   <- as.numeric(allGames$w_svpt)
allGames$w_1stWon <- as.numeric(allGames$w_1stWon)
allGames$w_2ndWon <- as.numeric(allGames$w_2ndWon)

allGames$l_svpt   <- as.numeric(allGames$l_svpt)
allGames$l_1stWon <- as.numeric(allGames$l_1stWon)
allGames$l_2ndWon <- as.numeric(allGames$l_2ndWon)

Nall <- nrow(allGames)

for (i in 1: Nall) {
  
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
      next()  
    }
    
    if(allGames$w_svpt[i] == 0 | allGames$l_svpt[i] == 0) {
      print(i)
      print("IS 0 dfhsjkdhfjkdsfhjksfdhkdsfjdfkhdsfkdshkdkhdsf")
      next()
    }
      
    
    
    WinnerPercentServeWon  <- (allGames$w_1stWon[i] + allGames$w_2ndWon[i]) / allGames$w_svpt[i]
    LoserPercentServeWon   <- (allGames$l_1stWon[i] + allGames$l_2ndWon[i]) / allGames$l_svpt[i]
    
    ResultServeWinner   <- WinnerPercentServeWon
    ResultReturnLoser   <- 1 - ResultServeWinner
    
    ResultsServeLoser   <- LoserPercentServeWon
    ResultsReturnWinner <-  1 - ResultsServeLoser
    
    
    
    # Update rating        
    winner_serve_expectation                            <- getWinExpectationBasedOnRating(rating$ServeRatings[row_nr_winner], rating$ReturnRatings[row_nr_loser])
    winner_return_expectation                           <- getWinExpectationBasedOnRating(rating$ReturnRatings[row_nr_winner], rating$ServeRatings[row_nr_loser])
    
    rating$ServeRatings[row_nr_winner]                  <- calculateNewRating(rating$ServeRatings[row_nr_winner], rating$games[row_nr_winner], winner_serve_expectation, ResultServeWinner)
    rating$ReturnRatings[row_nr_loser]                  <- calculateNewRating(rating$ReturnRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_serve_expectation , ResultReturnLoser)
    
    rating$ReturnRatings[row_nr_winner]                 <- calculateNewRating(rating$ReturnRatings[row_nr_winner], rating$games[row_nr_winner], winner_return_expectation, ResultsReturnWinner)
    rating$ServeRatings[row_nr_loser]                   <- calculateNewRating(rating$ServeRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_return_expectation , ResultsServeLoser)
    
    
    # Update games         
    rating$games[row_nr_winner]                         <- rating$games[row_nr_winner] + 1
    rating$games[row_nr_loser]                          <- rating$games[row_nr_loser]  + 1
    rating$games_won[row_nr_winner]                     <- rating$games_won[row_nr_winner] + 1
    
    # surface dependent variables
    if (allGames$Surface[i] == "Hard") {
      
      winner_serve_expectation                            <- getWinExpectationBasedOnRating(rating$Hard_ServeRatings[row_nr_winner], rating$Hard_ReturnRatings[row_nr_loser])
      winner_return_expectation                           <- getWinExpectationBasedOnRating(rating$Hard_ReturnRatings[row_nr_winner], rating$Hard_ServeRatings[row_nr_loser])
      
      rating$Hard_ServeRatings[row_nr_winner]                  <- calculateNewRating(rating$Hard_ServeRatings[row_nr_winner], rating$games[row_nr_winner], winner_serve_expectation, ResultServeWinner)
      rating$Hard_ReturnRatings[row_nr_loser]                  <- calculateNewRating(rating$Hard_ReturnRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_serve_expectation , ResultReturnLoser)
      
      rating$Hard_ReturnRatings[row_nr_winner]                 <- calculateNewRating(rating$Hard_ReturnRatings[row_nr_winner], rating$games[row_nr_winner], winner_return_expectation, ResultsReturnWinner)
      rating$Hard_ServeRatings[row_nr_loser]                   <- calculateNewRating(rating$Hard_ServeRatings[row_nr_loser] , rating$games[row_nr_loser] , 1 - winner_return_expectation , ResultsServeLoser)
      
      rating$Hard_games[row_nr_winner]                   <- rating$Hard_games[row_nr_winner] + 1
      rating$Hard_games[row_nr_loser]                    <- rating$Hard_games[row_nr_loser]  + 1
      rating$Hard_games_won[row_nr_winner]               <- rating$Hard_games_won[row_nr_winner] + 1
    } 
   
  } else {
    print("ERROR: Player cannot be matched with Rating")
  }
}
saveDatasetsWithRating(allGames, rating)

endTime <- Sys.time()

loadtime <- endTime - startTime
print(loadtime)