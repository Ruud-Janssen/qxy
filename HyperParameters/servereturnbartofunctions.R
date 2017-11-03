source("formulas.r")
source("addratingsformulas.r")
require(Rmpfr)

GetServeReturnBarto = function(Bp, Bf, sigma, ratingGainForWin) {
  rg        <- ratingGainForWin 
    
  BpHard    <- Bp
  BfHard    <- Bf
  sigmaHard <- sigma
  rgHard    <- rg

  train_rating = read.table("Data/datasets/train_ratingWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)
  train_model = read.table("Data/datasets/train_modelWithRatings.csv"
                           , header = T, sep = ",", quote = "\"", fill = TRUE)
  
  Nt_r = nrow(train_rating)
  Nt_m = nrow(train_model)
  
  Ntot = Nt_r + Nt_m
  
  allGames = rbind(train_rating, train_model)
  
  #######Create Ratings for all players and Start initializing them######
  
  player  <- getPlayers()
  barto   <- InitializeBartoServeReturn(player)
  
  #Update ratings with train_rating
  allGames$w_svpt   <- as.numeric(allGames$w_svpt)
  allGames$w_1stWon <- as.numeric(allGames$w_1stWon)
  allGames$w_2ndWon <- as.numeric(allGames$w_2ndWon)
  
  allGames$l_svpt   <- as.numeric(allGames$l_svpt)
  allGames$l_1stWon <- as.numeric(allGames$l_1stWon)
  allGames$l_2ndWon <- as.numeric(allGames$l_2ndWon)
  
  Nall <- nrow(allGames)
  
  for (i in 1: Nall) {
    # get matching winner and loserplayer in rating and save the rownr
    # get matching winner and loserplayer in rating and save the rownr
    row_nr_winner <- which(barto$id == allGames$idWinner[i])
    row_nr_loser  <- which(barto$id == allGames$idLoser[i])
    
    if (row_nr_winner > 0 & row_nr_loser > 0) {
      # NOTE: allGames = addHomePlayers(allGames, rating, i, matchDetails) NOT YET CONVERTED!!!!  
      
      # calculate surface independent variables
      allGames$Winner_servebarto[i]                       <- barto$ServeBarto[row_nr_winner]
      allGames$Winner_returnbarto[i]                      <- barto$ReturnBarto[row_nr_winner]
      allGames$Winner_servebartoHard[i]                   <- barto$Hard_ServeBarto[row_nr_winner]
      allGames$Winner_returnbartoHard[i]                  <- barto$Hard_ReturnBarto[row_nr_winner]
      
      allGames$Loser_servebarto[i]                        <- barto$ServeBarto[row_nr_loser]
      allGames$Loser_returnbarto[i]                       <- barto$ReturnBarto[row_nr_loser]
      allGames$Loser_servebartoHard[i]                    <- barto$Hard_ServeBarto[row_nr_loser]
      allGames$Loser_returnbartoHard[i]                   <- barto$Hard_ReturnBarto[row_nr_loser]
      
      
      numberOfGames      <- calculateGames(allGames[i, ])
      if(numberOfGames <= 10) {
        next()
      }
      
      if (is.na(allGames$w_svpt[i]) | is.na(allGames$w_1stWon[i]) | is.na(allGames$w_2ndWon[i]) |
          is.na(allGames$l_svpt[i]) | is.na(allGames$l_1stWon[i]) | is.na(allGames$l_2ndWon[i])) {
      
        #Serve
        barto$ServeBarto[row_nr_winner]  <- barto$ServeBarto[row_nr_winner] + rg
        barto$ReturnBarto[row_nr_loser]  <- barto$ReturnBarto[row_nr_loser] - rg
        
        #Return
        barto$ReturnBarto[row_nr_winner] <- barto$ReturnBarto[row_nr_winner] + rg
        barto$ServeBarto[row_nr_loser]   <- barto$ServeBarto[row_nr_loser] - rg
        
        # Update games         
        barto$games[row_nr_winner]       <- barto$games[row_nr_winner] + 1
        barto$games[row_nr_loser]        <- barto$games[row_nr_loser]  + 1
        
        # surface dependent variables
        if (allGames$Surface[i] == "Hard") {
          
          #Serve
          barto$Hard_ServeBarto[row_nr_winner]  <- barto$Hard_ServeBarto[row_nr_winner] + rgHard
          barto$Hard_ReturnBarto[row_nr_loser]  <- barto$Hard_ReturnBarto[row_nr_loser] - rgHard
          
          #Return
          barto$Hard_ReturnBarto[row_nr_winner] <- barto$Hard_ReturnBarto[row_nr_winner] + rgHard
          barto$Hard_ServeBarto[row_nr_loser]   <- barto$Hard_ServeBarto[row_nr_loser] - rgHard
          
          # Update games         
          barto$Hard_games[row_nr_winner]       <- barto$games[row_nr_winner] + 1
          barto$Hard_games[row_nr_loser]        <- barto$games[row_nr_loser]  + 1  
        } 
        next()  
      }
      
      if(i == 29311 | i == 3871 | i == 29312 | i == 5452 | i == 27083 | i == 35329 | i == 37398) {
        next()
      }
      
      if(allGames$w_svpt[i] == 0 | allGames$l_svpt[i] == 0) {
        next()
      }
      
      rating_serve_winner_diff         <- barto$ServeBarto[row_nr_winner] - barto$ReturnBarto[row_nr_loser]
      posterior_serve_diff             <- posteriorRatingDiff(Bp, Bf, rating_serve_winner_diff, sigma, 
                                                              allGames$w_svpt[i], allGames$w_1stWon[i] + allGames$w_2ndWon[i])
      rating_change_serve_winner       <- -(rating_serve_winner_diff - posterior_serve_diff) / 2
      
      barto$ServeBarto[row_nr_winner]  <- barto$ServeBarto[row_nr_winner] + rating_change_serve_winner
      barto$ReturnBarto[row_nr_loser]  <- barto$ReturnBarto[row_nr_loser] - rating_change_serve_winner
      
      #Return
      rating_return_winner_diff        <- barto$ReturnBarto[row_nr_winner] - barto$ServeBarto[row_nr_loser]
      posterior_return_diff            <- posteriorRatingDiff(Bp, Bf, rating_return_winner_diff, sigma, 
                                                              allGames$l_svpt[i], allGames$l_svpt[i] - allGames$l_1stWon[i] - allGames$l_2ndWon[i])
      rating_change_return_winner      <- -(rating_return_winner_diff - posterior_return_diff) / 2
      
      barto$ReturnBarto[row_nr_winner] <- barto$ReturnBarto[row_nr_winner] + rating_change_return_winner
      barto$ServeBarto[row_nr_loser]   <- barto$ServeBarto[row_nr_loser] - rating_change_return_winner
      
      # Update games         
      barto$games[row_nr_winner]       <- barto$games[row_nr_winner] + 1
      barto$games[row_nr_loser]        <- barto$games[row_nr_loser]  + 1
      
      # surface dependent variables
      if (allGames$Surface[i] == "Hard") {
        
        #Serve
        rating_serve_winner_diff         <- barto$Hard_ServeBarto[row_nr_winner] - barto$Hard_ReturnBarto[row_nr_loser]
        posterior_serve_diff             <- posteriorRatingDiff(Bp, Bf, rating_serve_winner_diff, sigma, 
                                                                allGames$w_svpt[i], allGames$w_1stWon[i] + allGames$w_2ndWon[i])
        rating_change_serve_winner       <- -(rating_serve_winner_diff - posterior_serve_diff) / 2
        
        barto$Hard_ServeBarto[row_nr_winner]  <- barto$Hard_ServeBarto[row_nr_winner] + rating_change_serve_winner
        barto$Hard_ReturnBarto[row_nr_loser]  <- barto$Hard_ReturnBarto[row_nr_loser] - rating_change_serve_winner
        
        #Return
        rating_return_winner_diff        <- barto$Hard_ReturnBarto[row_nr_winner] - barto$Hard_ServeBarto[row_nr_loser]
        posterior_return_diff            <- posteriorRatingDiff(Bp, Bf, rating_return_winner_diff, sigma, 
                                                                allGames$l_svpt[i], allGames$l_svpt[i] - allGames$l_1stWon[i] - allGames$l_2ndWon[i])
        rating_change_return_winner      <- -(rating_return_winner_diff - posterior_return_diff) / 2
        
        barto$Hard_ReturnBarto[row_nr_winner] <- barto$Hard_ReturnBarto[row_nr_winner] + rating_change_return_winner
        barto$Hard_ServeBarto[row_nr_loser]   <- barto$Hard_ServeBarto[row_nr_loser] - rating_change_return_winner
        
        # Update games         
        barto$Hard_games[row_nr_winner]       <- barto$games[row_nr_winner] + 1
        barto$Hard_games[row_nr_loser]        <- barto$games[row_nr_loser]  + 1  
      } 
      
    } else {
      print("ERROR: Player cannot be matched with Rating")
    }
  }
  
  return(allGames[(Nt_r + 1):Ntot, ])
}

AddAGame <- function(games, indexWinner, indexLoser) {
  games[indexWinner] = games[indexWinner] + 1
  games[indexLoser] = games[indexLoser] + 1
  
  return(games)
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

InitializeBartoServeReturn = function(barto){
  barto <- barto %>% mutate(ServeBarto = 1650,
                            ReturnBarto = 1350,
                            games = 0,
                              
                            Hard_ServeBarto = ServeBarto,
                            Hard_ReturnBarto = ReturnBarto,
                            Hard_games = 0
  )
}

barto_likelihood <- function(Pd, Sd, Bp, Bf, Ud, sigma, n, w) {
  loga <- log(1 + exp(Pd * Bp))
  logb <- Pd * Bp
  logc <- -1 / 2 * ((Pd - Sd) / sqrt(2 * Bf ^ 2)) ^ 2
 
  return(exp(-n * loga + (n - w) * logb + logc))
}

negProportionalPosterior <- function(Sd, Bp, Bf, Ud, sigma, n, w) {
  #l     <- integrateR(barto_likelihood, lower = -10000, upper = 10000, Sd = Sd, 
  #                  Bp = Bp, Bf = Bf, Ud = Ud, sigma = sigma, n = n, w = w) 
  l     <- integrateR(barto_likelihood, lower = Sd - 300, upper = Sd + 300, Sd = Sd, 
                      Bp = Bp, Bf = Bf, Ud = Ud, sigma = sigma, n = n, w = w) 
  prior <- dnorm(Sd, mean = Ud, sd = sqrt(2 * sigma ^ 2))
  -prior * l$value * 10 ^ 12
}

posteriorRatingDiff <- function(Bp, Bf, Ud, sigma, n, w) {
  optim(Ud, negProportionalPosterior, Bp = Bp, Bf = Bf, Ud = Ud, sigma = sigma, n = n, w = w, 
        lower = Ud - 50, upper = Ud + 50, method = "Brent")$par
}
