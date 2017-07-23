source("formulas.R")
source("addratingsformulas.R")
library(dplyr)

getGlickos = function(games, rdInt, c) {
  
  cHard <- 2.5
  rdIntHard <- 110
  
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
  glicko <- InitializeGlicko(player, rdInt)
  
  games <- RemoveWalkOvers(games)
  games <- InitializeGlickoVariablesForGames(games)
  
  Nall <- nrow(games)

  for (i in 1: Nall) {
    # get matching winner and loserplayer in glicko and save the rownr
    row_nr_winner <- which(glicko$id == games$idWinner[i])
    row_nr_loser  <- which(glicko$id == games$idLoser[i])
    
    if (row_nr_winner > 0 & row_nr_loser > 0) {
      gameDate <- games$Date[i]
      #update RDs before game
      glicko$rd[row_nr_winner] <- updateRDBeforeGame(glicko$rd[row_nr_winner], glicko$last_game[row_nr_winner], gameDate, rdInt, c)
      glicko$rd[row_nr_loser]  <- updateRDBeforeGame(glicko$rd[row_nr_loser], glicko$last_game[row_nr_loser], gameDate, rdInt, c)
      #Update date last game
      glicko$last_game[row_nr_winner] <- gameDate 
      glicko$last_game[row_nr_loser]  <- gameDate
      
      if (games$Best.of[i] == 3) {   
        glicko$Bo3_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Bo3_rd[row_nr_winner], glicko$Bo3_last_game[row_nr_winner], gameDate, rdInt, c)
        glicko$Bo3_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Bo3_rd[row_nr_loser], glicko$Bo3_last_game[row_nr_loser], gameDate, rdInt, c)
        #Update date last game
        glicko$Bo3_last_game[row_nr_winner] <- gameDate 
        glicko$Bo3_last_game[row_nr_loser]  <- gameDate
      } else {
        glicko$Bo5_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Bo5_rd[row_nr_winner], glicko$Bo5_last_game[row_nr_winner], gameDate, rdInt, c)
        glicko$Bo5_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Bo5_rd[row_nr_loser], glicko$Bo5_last_game[row_nr_loser], gameDate, rdInt, c)
        #Update date last game
        glicko$Bo5_last_game[row_nr_winner] <- gameDate 
        glicko$Bo5_last_game[row_nr_loser]  <- gameDate
      }  
      
      # surface dependent variables
      if (games$Surface[i] == "Hard") {
        glicko$Hard_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Hard_rd[row_nr_winner], glicko$Hard_last_game[row_nr_winner], gameDate, rdIntHard, cHard)
        glicko$Hard_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Hard_rd[row_nr_loser], glicko$Hard_last_game[row_nr_loser], gameDate, rdIntHard, cHard)
        #Update date last game
        glicko$Hard_last_game[row_nr_winner] <- gameDate 
        glicko$Hard_last_game[row_nr_loser]  <- gameDate
      } else { # Not Hard
        glicko$NotHard_rd[row_nr_winner] <- updateRDBeforeGame(glicko$NotHard_rd[row_nr_winner], glicko$NotHard_last_game[row_nr_winner], gameDate, rdInt, c)
        glicko$NotHard_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$NotHard_rd[row_nr_loser], glicko$NotHard_last_game[row_nr_loser], gameDate, rdInt, c)
        #Update date last game
        glicko$NotHard_last_game[row_nr_winner] <- gameDate 
        glicko$NotHard_last_game[row_nr_loser]  <- gameDate
      }
      if(games$Surface[i] == "Grass") {
        glicko$Grass_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Grass_rd[row_nr_winner], glicko$Grass_last_game[row_nr_winner], gameDate, rdInt, c)
        glicko$Grass_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Grass_rd[row_nr_loser], glicko$Grass_last_game[row_nr_loser], gameDate, rdInt, c)
        #Update date last game
        glicko$Grass_last_game[row_nr_winner] <- gameDate 
        glicko$Grass_last_game[row_nr_loser]  <- gameDate
      } else if(games$Surface[i] == "Clay") {
        glicko$Clay_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Clay_rd[row_nr_winner], glicko$Clay_last_game[row_nr_winner], gameDate, rdInt, c)
        glicko$Clay_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Clay_rd[row_nr_loser], glicko$Clay_last_game[row_nr_loser], gameDate, rdInt, c)
        #Update date last game
        glicko$Clay_last_game[row_nr_winner] <- gameDate 
        glicko$Clay_last_game[row_nr_loser]  <- gameDate
      } 
      else if(games$Surface[i] == "Carpet") {
        #Since carpet is not in use since 2009 no carpet variables will be saved
      }
      
      games$Winner_glicko[i]                            <- calculateGlickoVariable(glicko$Ratings[row_nr_winner], glicko$rd[row_nr_winner], glicko$rd[row_nr_loser])
      games$Winner_glickoClay[i]                        <- calculateGlickoVariable(glicko$Clay_Ratings[row_nr_winner], glicko$Clay_rd[row_nr_winner], glicko$Clay_rd[row_nr_loser])
      games$Winner_glickoHard[i]                        <- calculateGlickoVariable(glicko$Hard_Ratings[row_nr_winner], glicko$Hard_rd[row_nr_winner], glicko$Hard_rd[row_nr_loser])
      games$Winner_glickoGrass[i]                       <- calculateGlickoVariable(glicko$Grass_Ratings[row_nr_winner], glicko$Grass_rd[row_nr_winner], glicko$Grass_rd[row_nr_loser])
      games$Winner_glickoNotHard[i]                     <- calculateGlickoVariable(glicko$NotHard_Ratings[row_nr_winner], glicko$NotHard_rd[row_nr_winner], glicko$NotHard_rd[row_nr_loser])
      games$Winner_glickoBo3[i]                         <- calculateGlickoVariable(glicko$Bo3_Ratings[row_nr_winner], glicko$Bo3_rd[row_nr_winner], glicko$Bo3_rd[row_nr_loser])
      games$Winner_glickoBo5[i]                         <- calculateGlickoVariable(glicko$Bo5_Ratings[row_nr_winner], glicko$Bo5_rd[row_nr_winner], glicko$Bo5_rd[row_nr_loser])
      
      games$Loser_glicko[i]                             <- calculateGlickoVariable(glicko$Ratings[row_nr_loser], glicko$rd[row_nr_winner], glicko$rd[row_nr_loser])
      games$Loser_glickoClay[i]                         <- calculateGlickoVariable(glicko$Clay_Ratings[row_nr_loser], glicko$Clay_rd[row_nr_winner], glicko$Clay_rd[row_nr_loser])
      games$Loser_glickoHard[i]                         <- calculateGlickoVariable(glicko$Hard_Ratings[row_nr_loser], glicko$Hard_rd[row_nr_winner], glicko$Hard_rd[row_nr_loser])
      games$Loser_glickoGrass[i]                        <- calculateGlickoVariable(glicko$Grass_Ratings[row_nr_loser], glicko$Grass_rd[row_nr_winner], glicko$Grass_rd[row_nr_loser])
      games$Loser_glickoNotHard[i]                      <- calculateGlickoVariable(glicko$NotHard_Ratings[row_nr_loser], glicko$NotHard_rd[row_nr_winner], glicko$NotHard_rd[row_nr_loser])
      games$Loser_glickoBo3[i]                          <- calculateGlickoVariable(glicko$Bo3_Ratings[row_nr_loser], glicko$Bo3_rd[row_nr_winner], glicko$Bo3_rd[row_nr_loser])
      games$Loser_glickoBo5[i]                          <- calculateGlickoVariable(glicko$Bo5_Ratings[row_nr_loser], glicko$Bo5_rd[row_nr_winner], glicko$Bo5_rd[row_nr_loser])
      
      # Update glicko        
      glicko$Ratings[row_nr_winner]                        <- calculateNewGlicko(glicko$Ratings[row_nr_winner], glicko$Ratings[row_nr_loser], glicko$rd[row_nr_winner], glicko$rd[row_nr_loser], 1)
      glicko$Ratings[row_nr_loser]                         <- calculateNewGlicko(glicko$Ratings[row_nr_loser], glicko$Ratings[row_nr_winner], glicko$rd[row_nr_loser], glicko$rd[row_nr_winner], 0)
      
      glicko$rd[row_nr_winner] <- calculateRDAfterGame(glicko$Ratings[row_nr_winner], glicko$Ratings[row_nr_loser], glicko$rd[row_nr_winner], glicko$rd[row_nr_loser], 1)
      glicko$rd[row_nr_loser]  <- calculateRDAfterGame(glicko$Ratings[row_nr_loser], glicko$Ratings[row_nr_winner], glicko$rd[row_nr_loser], glicko$rd[row_nr_winner], 0)
      
      # bo3 and bo5 glickos  
      if (games$Best.of[i] == 3) {  
        glicko$Bo3_Ratings[row_nr_winner]                   <- calculateNewGlicko(glicko$Bo3_Ratings[row_nr_winner], glicko$Bo3_Ratings[row_nr_loser], glicko$Bo3_rd[row_nr_winner], glicko$Bo3_rd[row_nr_loser], 1)
        glicko$Bo3_Ratings[row_nr_loser]                    <- calculateNewGlicko(glicko$Bo3_Ratings[row_nr_loser], glicko$Bo3_Ratings[row_nr_winner], glicko$Bo3_rd[row_nr_loser], glicko$Bo3_rd[row_nr_winner], 0)
        
        glicko$Bo3_rd[row_nr_winner] <- calculateRDAfterGame(glicko$Bo3_Ratings[row_nr_winner], glicko$Bo3_Ratings[row_nr_loser], glicko$Bo3_rd[row_nr_winner], glicko$Bo3_rd[row_nr_loser], 1)
        glicko$Bo3_rd[row_nr_loser]  <- calculateRDAfterGame(glicko$Bo3_Ratings[row_nr_loser], glicko$Bo3_Ratings[row_nr_winner], glicko$Bo3_rd[row_nr_loser], glicko$Bo3_rd[row_nr_winner], 0)
      } else {        
        glicko$Bo5_Ratings[row_nr_winner]                   <- calculateNewGlicko(glicko$Bo5_Ratings[row_nr_winner], glicko$Bo5_Ratings[row_nr_loser], glicko$Bo5_rd[row_nr_winner], glicko$Bo5_rd[row_nr_loser], 1)
        glicko$Bo5_Ratings[row_nr_loser]                    <- calculateNewGlicko(glicko$Bo5_Ratings[row_nr_loser], glicko$Bo5_Ratings[row_nr_winner], glicko$Bo5_rd[row_nr_loser], glicko$Bo5_rd[row_nr_winner], 0)
        
        glicko$Bo5_rd[row_nr_winner] <- calculateRDAfterGame(glicko$Bo5_Ratings[row_nr_winner], glicko$Bo5_Ratings[row_nr_loser], glicko$Bo5_rd[row_nr_winner], glicko$Bo5_rd[row_nr_loser], 1)
        glicko$Bo5_rd[row_nr_loser]  <- calculateRDAfterGame(glicko$Bo5_Ratings[row_nr_loser], glicko$Bo5_Ratings[row_nr_winner], glicko$Bo5_rd[row_nr_loser], glicko$Bo5_rd[row_nr_winner], 0)
      }
      
      # surface dependent variables
      if (games$Surface[i] == "Hard") {
        glicko$Hard_Ratings[row_nr_winner]                  <- calculateNewGlicko(glicko$Hard_Ratings[row_nr_winner], glicko$Hard_Ratings[row_nr_loser], glicko$Hard_rd[row_nr_winner], glicko$Hard_rd[row_nr_loser], 1)
        glicko$Hard_Ratings[row_nr_loser]                   <- calculateNewGlicko(glicko$Hard_Ratings[row_nr_loser], glicko$Hard_Ratings[row_nr_winner], glicko$Hard_rd[row_nr_loser], glicko$Hard_rd[row_nr_winner], 0)
        
        glicko$Hard_rd[row_nr_winner]                       <- calculateRDAfterGame(glicko$Hard_Ratings[row_nr_winner], glicko$Hard_Ratings[row_nr_loser], glicko$Hard_rd[row_nr_winner], glicko$Hard_rd[row_nr_loser], 1)
        glicko$Hard_rd[row_nr_loser]                        <- calculateRDAfterGame(glicko$Hard_Ratings[row_nr_loser], glicko$Hard_Ratings[row_nr_winner], glicko$Hard_rd[row_nr_loser], glicko$Hard_rd[row_nr_winner], 0)
      } else { # Not Hard
        glicko$NotHard_Ratings[row_nr_winner]               <- calculateNewGlicko(glicko$NotHard_Ratings[row_nr_winner], glicko$NotHard_Ratings[row_nr_loser], glicko$NotHard_rd[row_nr_winner], glicko$NotHard_rd[row_nr_loser], 1)
        glicko$NotHard_Ratings[row_nr_loser]                <- calculateNewGlicko(glicko$NotHard_Ratings[row_nr_loser], glicko$NotHard_Ratings[row_nr_winner], glicko$NotHard_rd[row_nr_loser], glicko$NotHard_rd[row_nr_winner], 0)
        
        glicko$NotHard_rd[row_nr_winner]                    <- calculateRDAfterGame(glicko$NotHard_Ratings[row_nr_winner], glicko$NotHard_Ratings[row_nr_loser], glicko$NotHard_rd[row_nr_winner], glicko$NotHard_rd[row_nr_loser], 1)
        glicko$NotHard_rd[row_nr_loser]                     <- calculateRDAfterGame(glicko$NotHard_Ratings[row_nr_loser], glicko$NotHard_Ratings[row_nr_winner], glicko$NotHard_rd[row_nr_loser], glicko$NotHard_rd[row_nr_winner], 0)
        
      }
      if(games$Surface[i] == "Grass") {
        glicko$Grass_Ratings[row_nr_winner]                 <- calculateNewGlicko(glicko$Grass_Ratings[row_nr_winner], glicko$Grass_Ratings[row_nr_loser], glicko$Grass_rd[row_nr_winner], glicko$Grass_rd[row_nr_loser], 1)
        glicko$Grass_Ratings[row_nr_loser]                  <- calculateNewGlicko(glicko$Grass_Ratings[row_nr_loser], glicko$Grass_Ratings[row_nr_winner], glicko$Grass_rd[row_nr_loser], glicko$Grass_rd[row_nr_winner], 0)
        
        glicko$Grass_rd[row_nr_winner]                      <- calculateRDAfterGame(glicko$Grass_Ratings[row_nr_winner], glicko$Grass_Ratings[row_nr_loser], glicko$Grass_rd[row_nr_winner], glicko$Grass_rd[row_nr_loser], 1)
        glicko$Grass_rd[row_nr_loser]                       <- calculateRDAfterGame(glicko$Grass_Ratings[row_nr_loser], glicko$Grass_Ratings[row_nr_winner], glicko$Grass_rd[row_nr_loser], glicko$Grass_rd[row_nr_winner], 0)
        
      } else if(games$Surface[i] == "Clay") {
        glicko$Clay_Ratings[row_nr_winner]                  <- calculateNewGlicko(glicko$Clay_Ratings[row_nr_winner], glicko$Clay_Ratings[row_nr_loser], glicko$Clay_rd[row_nr_winner], glicko$Clay_rd[row_nr_loser], 1)
        glicko$Clay_Ratings[row_nr_loser]                   <- calculateNewGlicko(glicko$Clay_Ratings[row_nr_loser], glicko$Clay_Ratings[row_nr_winner], glicko$Clay_rd[row_nr_loser], glicko$Clay_rd[row_nr_winner], 0)
        
        glicko$Clay_rd[row_nr_winner]                       <- calculateRDAfterGame(glicko$Clay_Ratings[row_nr_winner], glicko$Clay_Ratings[row_nr_loser], glicko$Clay_rd[row_nr_winner], glicko$Clay_rd[row_nr_loser], 1)
        glicko$Clay_rd[row_nr_loser]                        <- calculateRDAfterGame(glicko$Clay_Ratings[row_nr_loser], glicko$Clay_Ratings[row_nr_winner], glicko$Clay_rd[row_nr_loser], glicko$Clay_rd[row_nr_winner], 0)
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
  startRDHard <- 110
  
  numberOfPlayers <- nrow(glicko)
  
  #Add start rating and number of games
  glicko$Ratings   <- rep(startRating, numberOfPlayers)
  glicko$rd        <- rep(startRD, numberOfPlayers)
  glicko$last_game <- rep(NA, numberOfPlayers)
  
  #Create Speciality Ratings
  glicko$Hard_Ratings   <- glicko$Ratings
  glicko$Hard_rd        <- rep(startRDHard, numberOfPlayers)
  glicko$Hard_last_game <- glicko$last_game
  
  glicko$Grass_Ratings   <- glicko$Ratings
  glicko$Grass_rd        <- glicko$rd
  glicko$Grass_last_game <- glicko$last_game
  
  glicko$Clay_Ratings   <- glicko$Ratings
  glicko$Clay_rd        <- glicko$rd
  glicko$Clay_last_game <- glicko$last_game
  
  glicko$NotHard_Ratings   <- glicko$Ratings
  glicko$NotHard_rd        <- glicko$rd
  glicko$NotHard_last_game <- glicko$last_game
  
  glicko$Bo3_Ratings   <-  glicko$Ratings
  glicko$Bo3_rd        <- glicko$rd
  glicko$Bo3_last_game <- glicko$last_game
  
  glicko$Bo5_Ratings   <- glicko$Ratings
  glicko$Bo5_rd        <- glicko$rd
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
