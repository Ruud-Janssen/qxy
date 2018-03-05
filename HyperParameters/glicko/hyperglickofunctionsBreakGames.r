source("formulas.R")
source("addratingsformulas.R")
library(dplyr)

getGlickos = function(rdIntNow, cNow, winBonus = 0) {
  
  cHard        <- 2.8
  cRDMaxHard   <- 130
  winBonusHard <- 0.26
  
  train_rating <- read.table("Data/datasets/train_ratingWithRatings.csv", 
                             header = T, sep = ",", quote = "\"", fill = TRUE)
  train_model  <- read.table("Data/datasets/train_modelWithRatings.csv", 
                             header = T, sep = ",", quote = "\"", fill = TRUE)
  
  train_rating <- RemoveWalkOvers(train_rating)
  train_model  <- RemoveWalkOvers(train_model)
  
  Nt_r <- nrow(train_rating)
  Nt_m <- nrow(train_model)
  
  Ntot <- Nt_r + Nt_m
  
  games <- rbind(train_rating, train_model)
  games$Date <- as.character(games$Date, format = "%m/%d/%Y")
  
  player   <- getPlayers()
  
  # Create Ratings for all players, ratings are adapted after each match
  glicko <- InitializeGlicko(player, rdIntNow)
  
  games <- RemoveWalkOvers(games)
  games <- InitializeGlickoVariablesForGames(games)
  
  Nall <- nrow(games)
  
  for (i in 1 : Nall) {

    # get matching winner and loserplayer in glicko and save the rownr
    row_nr_winner <- which(glicko$id == games$idWinner[i])
    row_nr_loser  <- which(glicko$id == games$idLoser[i])
    
    
    
    if (row_nr_winner > 0 & row_nr_loser > 0) {
      gameDate <- games$Date[i]
      #update RDs before game
      glicko$rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$rdGames[row_nr_winner], glicko$last_game[row_nr_winner], gameDate, rdIntNow, cNow)
      glicko$rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$rdGames[row_nr_loser], glicko$last_game[row_nr_loser], gameDate, rdIntNow, cNow)
      #Update date last game
      glicko$last_game[row_nr_winner] <- gameDate 
      glicko$last_game[row_nr_loser]  <- gameDate
      
      if (games$Best.of[i] == 3) {   
        glicko$Bo3_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Bo3_rdGames[row_nr_winner], glicko$Bo3_last_game[row_nr_winner], gameDate, rdIntNow, cNow)
        glicko$Bo3_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Bo3_rdGames[row_nr_loser], glicko$Bo3_last_game[row_nr_loser], gameDate, rdIntNow, cNow)
        #Update date last game
        glicko$Bo3_last_game[row_nr_winner] <- gameDate 
        glicko$Bo3_last_game[row_nr_loser]  <- gameDate
      } else {
        glicko$Bo5_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Bo5_rdGames[row_nr_winner], glicko$Bo5_last_game[row_nr_winner], gameDate, rdIntNow, cNow)
        glicko$Bo5_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Bo5_rdGames[row_nr_loser], glicko$Bo5_last_game[row_nr_loser], gameDate, rdIntNow, cNow)
        #Update date last game
        glicko$Bo5_last_game[row_nr_winner] <- gameDate 
        glicko$Bo5_last_game[row_nr_loser]  <- gameDate
      }  
      
      # surface dependent variables
      if (games$Surface[i] == "Hard") {
        glicko$Hard_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Hard_rdGames[row_nr_winner], glicko$Hard_last_game[row_nr_winner], gameDate, cRDMaxHard, cHard)
        glicko$Hard_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Hard_rdGames[row_nr_loser], glicko$Hard_last_game[row_nr_loser], gameDate, cRDMaxHard, cHard)
        #Update date last game
        glicko$Hard_last_game[row_nr_winner] <- gameDate 
        glicko$Hard_last_game[row_nr_loser]  <- gameDate
      } else { # Not Hard
        glicko$NotHard_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$NotHard_rdGames[row_nr_winner], glicko$NotHard_last_game[row_nr_winner], gameDate, rdIntNow, cNow)
        glicko$NotHard_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$NotHard_rdGames[row_nr_loser], glicko$NotHard_last_game[row_nr_loser], gameDate, rdIntNow, cNow)
        
        #Update date last game
        glicko$NotHard_last_game[row_nr_winner] <- gameDate 
        glicko$NotHard_last_game[row_nr_loser]  <- gameDate
      }
      if(games$Surface[i] == "Grass") {
        glicko$Grass_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Grass_rdGames[row_nr_winner], glicko$Grass_last_game[row_nr_winner], gameDate, rdIntNow, cNow)
        glicko$Grass_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Grass_rdGames[row_nr_loser], glicko$Grass_last_game[row_nr_loser], gameDate, rdIntNow, cNow)
        
        #Update date last game
        glicko$Grass_last_game[row_nr_winner] <- gameDate 
        glicko$Grass_last_game[row_nr_loser]  <- gameDate
      } else if(games$Surface[i] == "Clay") {
        glicko$Clay_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Clay_rdGames[row_nr_winner], glicko$Clay_last_game[row_nr_winner], gameDate, rdIntNow, cNow)
        glicko$Clay_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Clay_rdGames[row_nr_loser], glicko$Clay_last_game[row_nr_loser], gameDate, rdIntNow, cNow)
        
        #Update date last game
        glicko$Clay_last_game[row_nr_winner] <- gameDate 
        glicko$Clay_last_game[row_nr_loser]  <- gameDate
      } 
      else if(games$Surface[i] == "Carpet") {
        #Since carpet is not in use since 2009 no carpet variables will be saved
      }
      
      games$Winner_glickoGames[i]                            <- calculateGlickoVariable(glicko$RatingsGames[row_nr_winner], glicko$rdGames[row_nr_winner], glicko$rdGames[row_nr_loser])
      games$Winner_glickoClayGames[i]                        <- calculateGlickoVariable(glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_rdGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser])
      games$Winner_glickoHardGames[i]                        <- calculateGlickoVariable(glicko$Hard_RatingsGames[row_nr_winner], glicko$Hard_rdGames[row_nr_winner], glicko$Hard_rdGames[row_nr_loser])
      games$Winner_glickoGrassGames[i]                       <- calculateGlickoVariable(glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_rdGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser])
      games$Winner_glickoNotHardGames[i]                     <- calculateGlickoVariable(glicko$NotHard_RatingsGames[row_nr_winner], glicko$NotHard_rdGames[row_nr_winner], glicko$NotHard_rdGames[row_nr_loser])
      games$Winner_glickoBo3Games[i]                         <- calculateGlickoVariable(glicko$Bo3_RatingsGames[row_nr_winner], glicko$Bo3_rdGames[row_nr_winner], glicko$Bo3_rdGames[row_nr_loser])
      games$Winner_glickoBo5Games[i]                         <- calculateGlickoVariable(glicko$Bo5_RatingsGames[row_nr_winner], glicko$Bo5_rdGames[row_nr_winner], glicko$Bo5_rdGames[row_nr_loser])
      
      games$Loser_glickoGames[i]                            <- calculateGlickoVariable(glicko$RatingsGames[row_nr_loser], glicko$rdGames[row_nr_loser], glicko$rdGames[row_nr_loser])
      games$Loser_glickoClayGames[i]                        <- calculateGlickoVariable(glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_rdGames[row_nr_loser], glicko$Clay_rdGames[row_nr_loser])
      games$Loser_glickoHardGames[i]                        <- calculateGlickoVariable(glicko$Hard_RatingsGames[row_nr_loser], glicko$Hard_rdGames[row_nr_loser], glicko$Hard_rdGames[row_nr_loser])
      games$Loser_glickoGrassGames[i]                       <- calculateGlickoVariable(glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_rdGames[row_nr_loser], glicko$Grass_rdGames[row_nr_loser])
      games$Loser_glickoNotHardGames[i]                     <- calculateGlickoVariable(glicko$NotHard_RatingsGames[row_nr_loser], glicko$NotHard_rdGames[row_nr_loser], glicko$NotHard_rdGames[row_nr_loser])
      games$Loser_glickoBo3Games[i]                         <- calculateGlickoVariable(glicko$Bo3_RatingsGames[row_nr_loser], glicko$Bo3_rdGames[row_nr_loser], glicko$Bo3_rdGames[row_nr_loser])
      games$Loser_glickoBo5Games[i]                         <- calculateGlickoVariable(glicko$Bo5_RatingsGames[row_nr_loser], glicko$Bo5_rdGames[row_nr_loser], glicko$Bo5_rdGames[row_nr_loser])
      
      
      # Update glicko  
      if(games$Comment[i] == 'Retired' || games$Comment[i] == 'retired' || games$Comment[i] == 'Retied') {
        next()
      }
      
      numberOfGames      <- calculateGames(games[i, ])
      if(numberOfGames <= 10) {
        next()
      }
      
      FractionNetBreaksWinner  <- calculateFractionNetBreakGamesWinnerWon(games[i, ])
      
      GamesResultWinner  <- FractionNetBreaksWinner + winBonus
      GamesResultLoser   <- 1 - GamesResultWinner
      
      GamesResultWinnerHard  <- FractionNetBreaksWinner + winBonusHard
      GamesResultLoserHard   <- 1 - GamesResultWinnerHard
      
      
      if(is.nan(GamesResultWinner)) {
        next()
      }

      
      glicko$RatingsGames[row_nr_winner]                        <- calculateNewGlicko(glicko$RatingsGames[row_nr_winner], glicko$RatingsGames[row_nr_loser], glicko$rdGames[row_nr_winner], glicko$rdGames[row_nr_loser], GamesResultWinner)
      glicko$RatingsGames[row_nr_loser]                         <- calculateNewGlicko(glicko$RatingsGames[row_nr_loser], glicko$RatingsGames[row_nr_winner], glicko$rdGames[row_nr_loser], glicko$rdGames[row_nr_winner], GamesResultLoser)
      
      glicko$rdGames[row_nr_winner] <- calculateRDAfterGame(glicko$RatingsGames[row_nr_winner], glicko$RatingsGames[row_nr_loser], glicko$rdGames[row_nr_winner], glicko$rdGames[row_nr_loser], GamesResultWinner)
      glicko$rdGames[row_nr_loser]  <- calculateRDAfterGame(glicko$RatingsGames[row_nr_loser], glicko$RatingsGames[row_nr_winner], glicko$rdGames[row_nr_loser], glicko$rdGames[row_nr_winner], GamesResultLoser)
      
      # bo3 and bo5 glickos  
      if (games$Best.of[i] == 3) {  
        glicko$Bo3_RatingsGames[row_nr_winner]                   <- calculateNewGlicko(glicko$Bo3_RatingsGames[row_nr_winner], glicko$Bo3_RatingsGames[row_nr_loser], glicko$Bo3_rdGames[row_nr_winner], glicko$Bo3_rdGames[row_nr_loser], GamesResultWinner)
        glicko$Bo3_RatingsGames[row_nr_loser]                    <- calculateNewGlicko(glicko$Bo3_RatingsGames[row_nr_loser], glicko$Bo3_RatingsGames[row_nr_winner], glicko$Bo3_rdGames[row_nr_loser], glicko$Bo3_rdGames[row_nr_winner], GamesResultLoser)
        
        glicko$Bo3_rdGames[row_nr_winner] <- calculateRDAfterGame(glicko$Bo3_RatingsGames[row_nr_winner], glicko$Bo3_RatingsGames[row_nr_loser], glicko$Bo3_rdGames[row_nr_winner], glicko$Bo3_rdGames[row_nr_loser], GamesResultWinner)
        glicko$Bo3_rdGames[row_nr_loser]  <- calculateRDAfterGame(glicko$Bo3_RatingsGames[row_nr_loser], glicko$Bo3_RatingsGames[row_nr_winner], glicko$Bo3_rdGames[row_nr_loser], glicko$Bo3_rdGames[row_nr_winner], GamesResultLoser)
        
      } else {        
        glicko$Bo5_RatingsGames[row_nr_winner]                   <- calculateNewGlicko(glicko$Bo5_RatingsGames[row_nr_winner], glicko$Bo5_RatingsGames[row_nr_loser], glicko$Bo5_rdGames[row_nr_winner], glicko$Bo5_rdGames[row_nr_loser], GamesResultWinner)
        glicko$Bo5_RatingsGames[row_nr_loser]                    <- calculateNewGlicko(glicko$Bo5_RatingsGames[row_nr_loser], glicko$Bo5_RatingsGames[row_nr_winner], glicko$Bo5_rdGames[row_nr_loser], glicko$Bo5_rdGames[row_nr_winner], GamesResultLoser)
        
        glicko$Bo5_rdGames[row_nr_winner] <- calculateRDAfterGame(glicko$Bo5_RatingsGames[row_nr_winner], glicko$Bo5_RatingsGames[row_nr_loser], glicko$Bo5_rdGames[row_nr_winner], glicko$Bo5_rdGames[row_nr_loser], GamesResultWinner)
        glicko$Bo5_rdGames[row_nr_loser]  <- calculateRDAfterGame(glicko$Bo5_RatingsGames[row_nr_loser], glicko$Bo5_RatingsGames[row_nr_winner], glicko$Bo5_rdGames[row_nr_loser], glicko$Bo5_rdGames[row_nr_winner], GamesResultLoser)
      }
      
      # surface dependent variables
      if (games$Surface[i] == "Hard") {
        glicko$Hard_RatingsGames[row_nr_winner]                   <- calculateNewGlicko(glicko$Hard_RatingsGames[row_nr_winner], glicko$Hard_RatingsGames[row_nr_loser], glicko$Hard_rdGames[row_nr_winner], glicko$Hard_rdGames[row_nr_loser], GamesResultWinnerHard)
        glicko$Hard_RatingsGames[row_nr_loser]                    <- calculateNewGlicko(glicko$Hard_RatingsGames[row_nr_loser], glicko$Hard_RatingsGames[row_nr_winner], glicko$Hard_rdGames[row_nr_loser], glicko$Hard_rdGames[row_nr_winner], GamesResultLoserHard)
        
        glicko$Hard_rdGames[row_nr_winner] <- calculateRDAfterGame(glicko$Hard_RatingsGames[row_nr_winner], glicko$Hard_RatingsGames[row_nr_loser], glicko$Hard_rdGames[row_nr_winner], glicko$Hard_rdGames[row_nr_loser], GamesResultWinnerHard)
        glicko$Hard_rdGames[row_nr_loser]  <- calculateRDAfterGame(glicko$Hard_RatingsGames[row_nr_loser], glicko$Hard_RatingsGames[row_nr_winner], glicko$Hard_rdGames[row_nr_loser], glicko$Hard_rdGames[row_nr_winner], GamesResultLoserHard)
        
      } else { # Not Hard
        glicko$NotHard_RatingsGames[row_nr_winner]                   <- calculateNewGlicko(glicko$NotHard_RatingsGames[row_nr_winner], glicko$NotHard_RatingsGames[row_nr_loser], glicko$NotHard_rdGames[row_nr_winner], glicko$NotHard_rdGames[row_nr_loser], GamesResultWinner)
        glicko$NotHard_RatingsGames[row_nr_loser]                    <- calculateNewGlicko(glicko$NotHard_RatingsGames[row_nr_loser], glicko$NotHard_RatingsGames[row_nr_winner], glicko$NotHard_rdGames[row_nr_loser], glicko$NotHard_rdGames[row_nr_winner], GamesResultLoser)
        
        glicko$NotHard_rdGames[row_nr_winner] <- calculateRDAfterGame(glicko$NotHard_RatingsGames[row_nr_winner], glicko$NotHard_RatingsGames[row_nr_loser], glicko$NotHard_rdGames[row_nr_winner], glicko$NotHard_rdGames[row_nr_loser], GamesResultWinner)
        glicko$NotHard_rdGames[row_nr_loser]  <- calculateRDAfterGame(glicko$NotHard_RatingsGames[row_nr_loser], glicko$NotHard_RatingsGames[row_nr_winner], glicko$NotHard_rdGames[row_nr_loser], glicko$NotHard_rdGames[row_nr_winner], GamesResultLoser)
        
      }
      if(games$Surface[i] == "Grass") {
        glicko$Grass_RatingsGames[row_nr_winner]                   <- calculateNewGlicko(glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_rdGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser], GamesResultWinner)
        glicko$Grass_RatingsGames[row_nr_loser]                    <- calculateNewGlicko(glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser], glicko$Grass_rdGames[row_nr_winner], GamesResultLoser)
        
        glicko$Grass_rdGames[row_nr_winner]               <- calculateRDAfterGame(glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_rdGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser], GamesResultWinner)
        glicko$Grass_rdGames[row_nr_loser]                <- calculateRDAfterGame(glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser], glicko$Grass_rdGames[row_nr_winner], GamesResultLoser)
      } else if(games$Surface[i] == "Clay") {
        glicko$Clay_RatingsGames[row_nr_winner]                   <- calculateNewGlicko(glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_rdGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser], GamesResultWinner)
        glicko$Clay_RatingsGames[row_nr_loser]                    <- calculateNewGlicko(glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser], glicko$Clay_rdGames[row_nr_winner], GamesResultLoser)
        
        glicko$Clay_rdGames[row_nr_winner]               <- calculateRDAfterGame(glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_rdGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser], GamesResultWinner)
        glicko$Clay_rdGames[row_nr_loser]                <- calculateRDAfterGame(glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser], glicko$Clay_rdGames[row_nr_winner], GamesResultLoser)
        
      } else if(games$Surface[i] == "Carpet") {
        #Since carpet is not in use since 2009 no carpet variables will be saved
      }
    } else {
      print("ERROR: Player cannot be matched with Glicko")
    }
  }
  return(games[(Nt_r + 1):Ntot, ])
}

InitializeGlicko <- function(glicko, rdInt = 350) {
  startRating <- 1500
  startRD     <- rdInt
  startRDHard <- rdInt
  
  numberOfPlayers <- nrow(glicko)
  
  #Add start rating and number of games
  glicko$RatingsGames   <- rep(startRating, numberOfPlayers)
  glicko$rdGames        <- rep(startRD, numberOfPlayers)
  glicko$last_game <- rep(NA, numberOfPlayers)
  
  #Create Speciality Ratings
  glicko$Hard_RatingsGames   <- glicko$Ratings
  glicko$Hard_rdGames        <- rep(startRDHard, numberOfPlayers)
  glicko$Hard_last_game <- glicko$last_game
  
  glicko$Grass_RatingsGames   <- glicko$Ratings
  glicko$Grass_rdGames        <- glicko$rd
  glicko$Grass_last_game <- glicko$last_game
  
  glicko$Clay_RatingsGames   <- glicko$Ratings
  glicko$Clay_rdGames        <- glicko$rd
  glicko$Clay_last_game <- glicko$last_game
  
  glicko$NotHard_RatingsGames   <- glicko$Ratings
  glicko$NotHard_rdGames        <- glicko$rd
  glicko$NotHard_last_game <- glicko$last_game
  
  glicko$Bo3_RatingsGames   <-  glicko$Ratings
  glicko$Bo3_rdGames        <- glicko$rd
  glicko$Bo3_last_game <- glicko$last_game
  
  glicko$Bo5_RatingsGames   <- glicko$Ratings
  glicko$Bo5_rdGames        <- glicko$rd
  glicko$Bo5_last_game <- glicko$last_game
  
  return(glicko)
}

InitializeGlickoVariablesForGames = function(dataset) {
  
  rows = nrow(dataset)
  
  dataset$UncertaintyGlicko        = rep(NA, rows)
  dataset$UncertaintyGlickoSurface = rep(NA, rows)
  
  dataset$Winner_glicko        = rep(NA, rows)
  dataset$Winner_glickord      = rep(NA, rows)
  dataset$Winner_glickoClay    = rep(NA, rows)
  dataset$Winner_glickordClay  = rep(NA, rows)
  dataset$Winner_glickoHard    = rep(NA, rows)
  dataset$Winner_glickordHard  = rep(NA, rows)
  dataset$Winner_glickoGrass   = rep(NA, rows)
  dataset$Winner_glickordGrass = rep(NA, rows)
  dataset$Winner_glickoBo3     = rep(NA, rows)
  dataset$Winner_glickordBo3   = rep(NA, rows)
  dataset$Winner_glickoBo5     = rep(NA, rows)
  dataset$Winner_glickordBo5   = rep(NA, rows)
  
  dataset$Loser_glicko        = rep(NA, rows)
  dataset$Loser_glickord      = rep(NA, rows)
  dataset$Loser_glickoClay    = rep(NA, rows)
  dataset$Loser_glickordClay  = rep(NA, rows)
  dataset$Loser_glickoHard    = rep(NA, rows)
  dataset$Loser_glickordHard  = rep(NA, rows)
  dataset$Loser_glickoGrass   = rep(NA, rows)
  dataset$Loser_glickordGrass = rep(NA, rows)
  dataset$Loser_glickoBo3     = rep(NA, rows)
  dataset$Loser_glickordBo3   = rep(NA, rows)
  dataset$Loser_glickoBo5     = rep(NA, rows)
  dataset$Loser_glickordBo5   = rep(NA, rows)
  
  dataset$Winner_expectationBasedOnGlicko = rep(NA, rows)
  dataset$Loser_expectationBasedOnGlicko  = rep(NA, rows)
  
  return(dataset)
}

#Formula for updating RD: RD' = sqrt(RD^2 + t * c ^ 2), with t the periods passed (days in our case)
updateRDBeforeGame <- function(rd, lastGameDate, gameDate, rdInt = 350, c = 3.2) {
  if(is.na(lastGameDate)) {
    return(rdInt)
  }
  df = "%m/%d/%Y"
  
  
  lastGameDate = as.Date(as.character(lastGameDate), format = df)
  gameDate  = as.Date(as.character(gameDate), format = df)
  
  timeDiff = as.numeric(abs(lastGameDate - gameDate))
  min(rdInt, sqrt(rd ^ 2 + timeDiff * c ^ 2))
}

g <- function(rd) {
  q = 0.0057565
  1 / sqrt(1 + 3 * q ^ 2 * rd ^ 2 / pi ^ 2)
}

calculateNewGlicko = function(ratingPlayer, ratingOpponent, rdPlayer, rdOpponent, result) {
  q = 0.0057565
  gRD = g(rdOpponent)
  E = 1 / (1 + 10 ^ (-gRD * (ratingPlayer - ratingOpponent)/ 400))
  dsquared = (q ^ 2 * gRD ^ 2 * E * (1 - E)) ^ - 1
  ratingPlayer + q / (1 / rdPlayer ^ 2 + 1 / dsquared) * gRD * (result - E)
}

calculateRDAfterGame <- function(ratingPlayer, ratingOpponent, rdPlayer, rdOpponent, result) {
  q = 0.0057565
  gRD = g(rdOpponent)
  E = 1 / (1 + 10 ^ (-gRD * (ratingPlayer - ratingOpponent)/ 400))
  dsquared = (q ^ 2 * gRD ^ 2 * E * (1 - E)) ^ - 1
  sqrt(1 / (1 / rdPlayer  ^ 2 + 1 / dsquared))
}



calculateGlickoVariable <- function(rating, rd1, rd2) {
  g(sqrt(rd1 ^ 2 + rd2 ^ 2)) * rating
}


calculateGames <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  Games <- wonGames + lostGames
}


calculateFractionNetBreakGamesWinnerWon <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  percentWonGames <- 0.5 + (wonGames - lostGames) / (wonGames + lostGames)
}