source("formulas.r")
source("addratingsformulas.r")
require(Rmpfr)
library(cubature)

GetServeReturnBarto = function(allGames, Bp, Bf, sigma, ratingGainForWin) {
  rg        <- ratingGainForWin 
    
  BpHard    <- Bp
  BfHard    <- Bf
  sigmaHard <- sigma
  rgHard    <- rg

  train_rating <- read.table("Data/datasets/train_ratingWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)
  train_model  <- read.table("Data/datasets/train_modelWithRatings.csv"
                           , header = T, sep = ",", quote = "\"", fill = TRUE)
  
  Nt_r <- nrow(train_rating)
  Nt_m <- nrow(train_model)
  
  Ntot <- Nt_r + Nt_m
  
  #######Create Ratings for all players and Start initializing them######
  
  player  <- getPlayers()
  barto   <- InitializeBartoServeReturn(player)
  
  #Update ratings with train_rating
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
      
      if (is.na(allGames$w_svpt[i]) | is.na(allGames$w_svpt_won[i]) |
          is.na(allGames$l_svpt[i]) | is.na(allGames$w_rtpt_won[i])) {
      
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
      
      ##
      
      #rating_serve_winner_diff         <- barto$ServeBarto[row_nr_winner] - barto$ReturnBarto[row_nr_loser]
      #rating_change_serve_winner       <- ratingChanceWinner(Bp, Bf, rating_serve_winner_diff, sigma, 
      #                                                       allGames$w_svpt[i], allGames$w_svpt_won[i])
      
      #barto$ServeBarto[row_nr_winner]  <- barto$ServeBarto[row_nr_winner] + rating_change_serve_winner
      #barto$ReturnBarto[row_nr_loser]  <- barto$ReturnBarto[row_nr_loser] - rating_change_serve_winner
      
      rating_chances                   <- ratingChances(Bp, Bf, Bf, barto$ServeBarto[row_nr_winner], 
                                                        barto$ReturnBarto[row_nr_loser], sigma, n, w) 
      
      barto$ServeBarto[row_nr_winner]  <- barto$ServeBarto[row_nr_winner] + rating_chances[1]
      barto$ReturnBarto[row_nr_loser]  <- barto$ReturnBarto[row_nr_loser] + rating_chances[2]
      
      #Return
      #rating_return_winner_diff        <- barto$ReturnBarto[row_nr_winner] - barto$ServeBarto[row_nr_loser]
      #rating_change_return_winner      <- ratingChanceWinner(Bp, Bf, rating_return_winner_diff, sigma, 
      #                                                        allGames$l_svpt[i], allGames$w_rtpt_won[i])
      
      #barto$ReturnBarto[row_nr_winner] <- barto$ReturnBarto[row_nr_winner] + rating_change_return_winner
      #barto$ServeBarto[row_nr_loser]   <- barto$ServeBarto[row_nr_loser] - rating_change_return_winner
      
      rating_chances                   <- ratingChances(Bp, Bf, Bf, barto$ReturnBarto[row_nr_winner], 
                                                        barto$ServeBarto[row_nr_loser], sigma, n, w) 
      
      barto$ReturnBarto[row_nr_winner] <- barto$ReturnBarto[row_nr_winner] + rating_chances[1]
      barto$ServeBarto[row_nr_loser]   <- barto$ServeBarto[row_nr_loser] + rating_chances[2]
      
      # Update games         
      barto$games[row_nr_winner]       <- barto$games[row_nr_winner] + 1
      barto$games[row_nr_loser]        <- barto$games[row_nr_loser]  + 1
      
      # surface dependent variables
      if (allGames$Surface[i] == "Hard") {
        
        #Serve
        #rating_serve_winner_diff         <- barto$Hard_ServeBarto[row_nr_winner] - barto$Hard_ReturnBarto[row_nr_loser]
        #rating_change_serve_winner       <- ratingChanceWinner(Bp, Bf, rating_serve_winner_diff, sigma, 
        #                                                       allGames$w_svpt[i], allGames$w_svpt_won[i])
            
        #barto$Hard_ServeBarto[row_nr_winner]  <- barto$Hard_ServeBarto[row_nr_winner] + rating_change_serve_winner
        #barto$Hard_ReturnBarto[row_nr_loser]  <- barto$Hard_ReturnBarto[row_nr_loser] - rating_change_serve_winner
        
        rating_chances                   <- ratingChances(Bp, Bf, Bf, barto$Hard_ServeBarto[row_nr_winner], 
                                                          barto$Hard_ReturnBarto[row_nr_loser], sigma, n, w) 
        
        barto$Hard_ServeBarto[row_nr_winner]  <- barto$Hard_ServeBarto[row_nr_winner] + rating_chances[1]
        barto$Hard_ReturnBarto[row_nr_loser]  <- barto$Hard_ReturnBarto[row_nr_loser] + rating_chances[2]
        
        #Return
        #rating_return_winner_diff        <- barto$Hard_ReturnBarto[row_nr_winner] - barto$Hard_ServeBarto[row_nr_loser]
        #rating_change_return_winner      <- ratingChanceWinner(Bp, Bf, rating_return_winner_diff, sigma, 
        #                                                        allGames$l_svpt[i], allGames$w_rtpt_won[i])
            
        #barto$Hard_ReturnBarto[row_nr_winner] <- barto$Hard_ReturnBarto[row_nr_winner] + rating_change_return_winner
        #barto$Hard_ServeBarto[row_nr_loser]   <- barto$Hard_ServeBarto[row_nr_loser] - rating_change_return_winner
        
        rating_chances                   <- ratingChances(Bp, Bf, Bf, barto$Hard_ServeBarto[row_nr_winner], 
                                                          barto$Hard_ReturnBarto[row_nr_loser], sigma, n, w)
        
        barto$Hard_ServeBarto[row_nr_winner]  <- barto$Hard_ServeBarto[row_nr_winner] + rating_chances[1]
        barto$Hard_ReturnBarto[row_nr_loser]  <- barto$Hard_ReturnBarto[row_nr_loser] + rating_chances[2]
        
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

setEstimatedScores <- function() {
  train_rating <- read.table("Data/datasets/train_ratingWithRatings.csv"
                             , header = T, sep = ",", quote = "\"", fill = TRUE)
  train_model <- read.table("Data/datasets/train_modelWithRatings.csv"
                            , header = T, sep = ",", quote = "\"", fill = TRUE)
  
  allGames <- rbind(train_rating, train_model)
  
  allGames <- allGames %>% mutate(pointsMissing  = is.na(w_svpt),
                                  w_svpt_won     = w_1stWon + w_2ndWon,
                                  w_rtpt_won     = l_svpt - l_1stWon - l_2ndWon,
                                  w_svptpercent  = (w_1stWon + w_2ndWon) / w_svpt,
                                  w_rtptpercent  = 1 - (l_1stWon + l_2ndWon) / allGames$l_svpt,
                                  GamesWonWinner = sum(c(W1, W2, W3, W4, W5), na.rm = T),
                                  GamesWonLoser  = sum(c(L1, L2, L3, L4, L5), na.rm = T),
                                  orderedScore   = NA)
  
  
  for(i in 1 : nrow(allGames)){
    
    totalScore <- data.frame(wins = c(allGames$W1[i], allGames$W2[i], allGames$W3[i], allGames$W4[i], allGames$W5[i]), 
                             lost = c(allGames$L1[i], allGames$L2[i], allGames$L3[i], allGames$L4[i], allGames$L5[i]))
    totalScore <- na.omit(totalScore)
    totalScore <- totalScore %>% arrange(desc(wins), desc(lost))
    
    stringOrderedScore <- ""
    for(j in 1:  nrow(totalScore)) {
      stringOrderedScore <- paste(stringOrderedScore, as.character(totalScore$wins[j]), "-", as.character(totalScore$lost[j])," ", sep = "")
    }
    allGames$orderedScore[i] = stringOrderedScore
  }
  
  resultsDataBase <-     tibble(Score                        = NA,
                                GamesWonWinner               = NA,
                                GamesWonLoser                = NA,
                                Matches                      = NA,
                                PercentServePointsWonWinner  = NA,
                                ServePointsWinner            = NA,
                                PercentReturnPointsWonWinner = NA,
                                ReturnPointsWinner           = NA
  )
  
  indexesHavingPoints <- which(allGames$pointsMissing == 0)
  
  for(i in indexesHavingPoints) {
    index <- match(allGames$orderedScore[i], resultsDataBase$Score)
    
    if(!is.na(index)) {
      resultsDataBase$PercentServePointsWonWinner[index] <- 
        (resultsDataBase$PercentServePointsWonWinner[index] * resultsDataBase$Matches[index] + 
           allGames$w_svptpercent[i]) / (resultsDataBase$Matches[index] + 1)
      resultsDataBase$ServePointsWinner[index] <- 
        (resultsDataBase$ServePointsWinner[index] * resultsDataBase$Matches[index] + 
           allGames$w_svpt[i]) / (resultsDataBase$Matches[index] + 1)
      
      resultsDataBase$PercentReturnPointsWonWinner[index] <- 
        (resultsDataBase$PercentReturnPointsWonWinner[index] * resultsDataBase$Matches[index] + 
           allGames$w_rtptpercent[i]) / (resultsDataBase$Matches[index] + 1)
      resultsDataBase$ReturnPointsWinner[index] <- 
        (resultsDataBase$ReturnPointsWinner[index] * resultsDataBase$Matches[index] + 
           allGames$l_svpt[i]) / (resultsDataBase$Matches[index] + 1)
      resultsDataBase$Matches[index] <- resultsDataBase$Matches[index] + 1
    } else {
      Score                        <- allGames$orderedScore[i]
      GamesWonWinner               <- allGames$GamesWonWinner[i]
      GamesWonLoser                <- allGames$GamesWonLoser[i]
      Matches                      <- 1
      PercentServePointsWonWinner  <- allGames$w_svptpercent[i]
      ServePointsWinner            <- allGames$w_svpt[i]
      PercentReturnPointsWonWinner <- allGames$w_rtptpercent[i]
      ReturnPointsWinner           <- allGames$l_svpt[i]
      
      resultsDataBase              <- resultsDataBase %>% add_row(Score, GamesWonWinner, GamesWonLoser, Matches, 
                                                                  PercentServePointsWonWinner, ServePointsWinner, 
                                                                  PercentReturnPointsWonWinner, ReturnPointsWinner)
    }
  }
  
  resultsDataBase <- resultsDataBase %>% arrange(desc(Matches))
  
  indexesMissingPoints <- which(allGames$pointsMissing == 1)
  
  for(i in indexesMissingPoints) {
    if((allGames$orderedScore[i] %in% resultsDataBase$Score)) {
      iDB <- match(allGames$orderedScore[i], resultsDataBase$Score)
      
      allGames$w_svpt[i]     <- resultsDataBase %>% slice(iDB) %>% select(ServePointsWinner) %>% round() %>% as.numeric()
      allGames$w_svpt_won[i] <- 
        resultsDataBase %>% slice(iDB) %>% mutate(w_1stIn = PercentServePointsWonWinner * ServePointsWinner) %>% 
        select(w_1stIn) %>% round() %>% as.numeric()
      
      allGames$l_svpt[i]     <- resultsDataBase %>% slice(iDB) %>% select(ReturnPointsWinner) %>% round() %>% as.numeric()
      allGames$w_rtpt_won[i] <- 
        resultsDataBase %>% slice(iDB) %>% mutate(w_1stIn = PercentReturnPointsWonWinner * ReturnPointsWinner) %>% 
        select(w_1stIn) %>% round() %>% as.numeric()
    } else {
      iDB <- which(resultsDataBase$GamesWonWinner == allGames$GamesWonWinner[i] & 
                     resultsDataBase$GamesWonLoser == allGames$GamesWonLoser[i])
      if(!is_empty(iDB)) {
        allGames$w_svpt[i]     <- 
          resultsDataBase %>% slice(iDB) %>% 
          summarise(AvgServePointsWinner = sum(ServePointsWinner * Matches) / sum(Matches)) %>% 
          round() %>% as.numeric()  
        allGames$w_svpt_won[i] <- 
          resultsDataBase %>% slice(iDB) %>% 
          summarise(AvgPercentServePointsWinner = sum(PercentServePointsWonWinner * Matches) / sum(Matches) * allGames$w_svpt[i]) %>% 
          round() %>% as.numeric()
        
        allGames$l_svpt[i]     <- 
          resultsDataBase %>% slice(iDB) %>% summarise(AvgReturnPointsWinner = sum(ReturnPointsWinner * Matches) / sum(Matches)) %>% 
          round() %>% as.numeric()  
        allGames$w_rtpt_won[i] <- 
          resultsDataBase %>% slice(iDB) %>%  
          summarise(AvgPercentReturnPointsWinner = sum(PercentReturnPointsWonWinner * Matches) / sum(Matches) * allGames$l_svpt[i]) %>% 
          round() %>% as.numeric() 
      }
    }
  } 
  
  allGames %>% select(-w_svptpercent, -w_rtptpercent) 
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
  barto <- barto %>% mutate(ServeBarto = 1600,
                            ReturnBarto = 1400,
                            games = 0,
                              
                            Hard_ServeBarto = ServeBarto,
                            Hard_ReturnBarto = ReturnBarto,
                            Hard_games = 0
  )
}

barto_likelihood <- function(Pd, Sd, Bp, Bf, n, w) {
  loga <- log(1 + exp(Pd * Bp))
  logb <- Pd * Bp
  logc <- -1 / 2 * ((Pd - Sd) / sqrt(2 * Bf ^ 2)) ^ 2
 
  return(exp(-n * loga + (n - w) * logb + logc))
}

negProportionalPosterior <- function(Sd, Bp, Bf, Ud, sigma, n, w) {
  l     <- integrateR(barto_likelihood, lower = Sd - 300, upper = Sd + 300, 
                      Sd = Sd, Bp = Bp, Bf = Bf, n = n, w = w) 
  prior <- dnorm(Sd, mean = Ud, sd = sqrt(2 * sigma ^ 2))
  -prior * l$value * 10 ^ 12
}

posteriorRatingDiff <- function(Bp, Bf, Ud, sigma, n, w) {
  -optim(Ud, negProportionalPosterior, Bp = Bp, Bf = Bf, Ud = Ud, sigma = sigma, n = n, w = w, 
        lower = Ud - 50, upper = Ud + 50, method = "Brent")$par
}

ratingChanceWinner <- function(Bp, Bf, Rd, sigma, n, w) {
  Ud <- -Rd
  posteriorRd <- posteriorRatingDiff(Bp, Bf, Ud, sigma, n, w)
  (posteriorRd - Rd) / 2
}

####New Try
ratingChances <- function(Bp, Bserve, Breturn, U1, U2, sigma, n, w) {
  c(U1, U2) - posteriorRatingDiff2(Bp, Bserve, Breturn, U1, U2, sigma, n, w)
}

posteriorRatingDiff2 <- function(Bp, Bserve, Breturn, U1, U2, sigma, n, w) {
  U_12 <- c(U1, U2)
  optim(U_12, negProportionalPosterior2, Bp = Bp, Bserve = Bserve, Breturn = Breturn, U1 = U1, U2 = U2, sigma = sigma, n = n, w = w, 
        method = "Nelder-Mead")$par
}

negProportionalPosterior2 <- function(S_12, Bp, Bserve, Breturn, U1, U2, sigma, n, w) {
  l <- adaptIntegrate(barto_likelihood2, lowerLimit = S_12 - 300, upperLimit =  S_12 + 300,
                      S1 = S_12[1], S2 = S_12[2], Bp = Bp, Bserve = Bserve, Breturn = Breturn, n = n, w = w)
  prior <- dnorm(S_12[1], mean = U1, sd = sigma) * dnorm(S_12[2], mean = U2, sd = sigma)
  -prior * l$integral * 10 ^ 30
}

barto_likelihood2 <- function(x, S1, S2, Bp, Bserve, Breturn, n, w) {
  P1 <- x[1]
  P2 <- x[2]
  loga <- log(1 + exp((P2 - P1) * Bp))  
  logb <- (P2 - P1) * Bp
  logc <- -1 / 2 * ((P1 - S1) / Bserve) ^ 2
  logd <- -1 / 2 * ((P2 - S2) / Breturn) ^ 2
  exp(-n * loga + (n - w) * logb + logc + logd)
}