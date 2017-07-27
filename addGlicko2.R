rm(list = ls())
source("formulas.R")
source("addglickoformulas2.R")
source("addratingsformulas.R")

c            <- 2.6
RDMax        <- 130
winBonus     <- 0.18


cHard        <- 2.8
RDMaxHard    <- 130
winBonusHard <- 0.26

startTime <- Sys.time ()

allGames <- getAllGamesWithRating()
player   <- getPlayers()

# Create Ratings for all players, ratings are adapted after each match
glicko <- InitializeGlicko(player)

allGames <- RemoveWalkOvers(allGames)
allGames <- InitializeGlickoVariablesForGames(allGames)

Nall <- nrow(allGames)

#Matches
for (i in 1 : Nall) {
  # get matching winner and loserplayer in glicko and save the rownr
  row_nr_winner <- which(glicko$id == allGames$idWinner[i])
  row_nr_loser  <- which(glicko$id == allGames$idLoser[i])
  
  if (row_nr_winner > 0 & row_nr_loser > 0) {
    gameDate <- allGames$Date[i]
    #update RDs before game
    glicko$rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$rdGames[row_nr_winner], glicko$last_game[row_nr_winner], gameDate, maxRD = RDMax, c = c)
    glicko$rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$rdGames[row_nr_loser], glicko$last_game[row_nr_loser], gameDate, maxRD = RDMax, c = c)
     #Update date last game
    glicko$last_game[row_nr_winner] <- gameDate 
    glicko$last_game[row_nr_loser]  <- gameDate
    
    if (allGames$Best.of[i] == 3) {   
      glicko$Bo3_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Bo3_rdGames[row_nr_winner], glicko$Bo3_last_game[row_nr_winner], gameDate)
      glicko$Bo3_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Bo3_rdGames[row_nr_loser], glicko$Bo3_last_game[row_nr_loser], gameDate)
       #Update date last game
      glicko$Bo3_last_game[row_nr_winner] <- gameDate 
      glicko$Bo3_last_game[row_nr_loser]  <- gameDate
    } else {
      glicko$Bo5_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Bo5_rdGames[row_nr_winner], glicko$Bo5_last_game[row_nr_winner], gameDate)
      glicko$Bo5_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Bo5_rdGames[row_nr_loser], glicko$Bo5_last_game[row_nr_loser], gameDate)
           #Update date last game
      glicko$Bo5_last_game[row_nr_winner] <- gameDate 
      glicko$Bo5_last_game[row_nr_loser]  <- gameDate
    }  
    
    # surface dependent variables
    if (allGames$Surface[i] == "Hard") {
      glicko$Hard_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Hard_rdGames[row_nr_winner], glicko$Hard_last_game[row_nr_winner], gameDate, maxRD = RDMaxHard, c = cHard)
      glicko$Hard_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Hard_rdGames[row_nr_loser], glicko$Hard_last_game[row_nr_loser], gameDate, maxRD = RDMaxHard, c = cHard)
       #Update date last game
      glicko$Hard_last_game[row_nr_winner] <- gameDate 
      glicko$Hard_last_game[row_nr_loser]  <- gameDate
    } else { # Not Hard
      glicko$NotHard_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$NotHard_rdGames[row_nr_winner], glicko$NotHard_last_game[row_nr_winner], gameDate)
      glicko$NotHard_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$NotHard_rdGames[row_nr_loser], glicko$NotHard_last_game[row_nr_loser], gameDate)

      #Update date last game
      glicko$NotHard_last_game[row_nr_winner] <- gameDate 
      glicko$NotHard_last_game[row_nr_loser]  <- gameDate
    }
    if(allGames$Surface[i] == "Grass") {
      glicko$Grass_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Grass_rdGames[row_nr_winner], glicko$Grass_last_game[row_nr_winner], gameDate)
      glicko$Grass_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Grass_rdGames[row_nr_loser], glicko$Grass_last_game[row_nr_loser], gameDate)
      
      #Update date last game
      glicko$Grass_last_game[row_nr_winner] <- gameDate 
      glicko$Grass_last_game[row_nr_loser]  <- gameDate
    } else if(allGames$Surface[i] == "Clay") {
      glicko$Clay_rdGames[row_nr_winner] <- updateRDBeforeGame(glicko$Clay_rdGames[row_nr_winner], glicko$Clay_last_game[row_nr_winner], gameDate)
      glicko$Clay_rdGames[row_nr_loser]  <- updateRDBeforeGame(glicko$Clay_rdGames[row_nr_loser], glicko$Clay_last_game[row_nr_loser], gameDate)
        
      #Update date last game
      glicko$Clay_last_game[row_nr_winner] <- gameDate 
      glicko$Clay_last_game[row_nr_loser]  <- gameDate
    } 
    else if(allGames$Surface[i] == "Carpet") {
      #Since carpet is not in use since 2009 no carpet variables will be saved
    }
    
    allGames$Winner_glickoGames[i]                            <- calculateGlickoVariable(glicko$RatingsGames[row_nr_winner], glicko$rdGames[row_nr_winner], glicko$rdGames[row_nr_loser])
    allGames$Winner_glickoClayGames[i]                        <- calculateGlickoVariable(glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_rdGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser])
    allGames$Winner_glickoHardGames[i]                        <- calculateGlickoVariable(glicko$Hard_RatingsGames[row_nr_winner], glicko$Hard_rdGames[row_nr_winner], glicko$Hard_rdGames[row_nr_loser])
    allGames$Winner_glickoGrassGames[i]                       <- calculateGlickoVariable(glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_rdGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser])
    allGames$Winner_glickoNotHardGames[i]                     <- calculateGlickoVariable(glicko$NotHard_RatingsGames[row_nr_winner], glicko$NotHard_rdGames[row_nr_winner], glicko$NotHard_rdGames[row_nr_loser])
    allGames$Winner_glickoBo3Games[i]                         <- calculateGlickoVariable(glicko$Bo3_RatingsGames[row_nr_winner], glicko$Bo3_rdGames[row_nr_winner], glicko$Bo3_rdGames[row_nr_loser])
    allGames$Winner_glickoBo5Games[i]                         <- calculateGlickoVariable(glicko$Bo5_RatingsGames[row_nr_winner], glicko$Bo5_rdGames[row_nr_winner], glicko$Bo5_rdGames[row_nr_loser])
  
    allGames$Loser_glickoGames[i]                            <- calculateGlickoVariable(glicko$RatingsGames[row_nr_loser], glicko$rdGames[row_nr_loser], glicko$rdGames[row_nr_loser])
    allGames$Loser_glickoClayGames[i]                        <- calculateGlickoVariable(glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_rdGames[row_nr_loser], glicko$Clay_rdGames[row_nr_loser])
    allGames$Loser_glickoHardGames[i]                        <- calculateGlickoVariable(glicko$Hard_RatingsGames[row_nr_loser], glicko$Hard_rdGames[row_nr_loser], glicko$Hard_rdGames[row_nr_loser])
    allGames$Loser_glickoGrassGames[i]                       <- calculateGlickoVariable(glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_rdGames[row_nr_loser], glicko$Grass_rdGames[row_nr_loser])
    allGames$Loser_glickoNotHardGames[i]                     <- calculateGlickoVariable(glicko$NotHard_RatingsGames[row_nr_loser], glicko$NotHard_rdGames[row_nr_loser], glicko$NotHard_rdGames[row_nr_loser])
    allGames$Loser_glickoBo3Games[i]                         <- calculateGlickoVariable(glicko$Bo3_RatingsGames[row_nr_loser], glicko$Bo3_rdGames[row_nr_loser], glicko$Bo3_rdGames[row_nr_loser])
    allGames$Loser_glickoBo5Games[i]                         <- calculateGlickoVariable(glicko$Bo5_RatingsGames[row_nr_loser], glicko$Bo5_rdGames[row_nr_loser], glicko$Bo5_rdGames[row_nr_loser])
    
    
    # Update glicko  
    numberOfGames      <- calculateGames(allGames[i, ])
    if(numberOfGames <= 10) {
      next()
    }
  
    
    FractionNetBreaksWinner  <- calculateFractionNetBreakGamesWinnerWon(allGames[i, ])
    
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
    if (allGames$Best.of[i] == 3) {  
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
    if (allGames$Surface[i] == "Hard") {
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
    if(allGames$Surface[i] == "Grass") {
      glicko$Grass_RatingsGames[row_nr_winner]                   <- calculateNewGlicko(glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_rdGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser], GamesResultWinner)
      glicko$Grass_RatingsGames[row_nr_loser]                    <- calculateNewGlicko(glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser], glicko$Grass_rdGames[row_nr_winner], GamesResultLoser)
      
      glicko$Grass_rdGames[row_nr_winner]               <- calculateRDAfterGame(glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_rdGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser], GamesResultWinner)
      glicko$Grass_rdGames[row_nr_loser]                <- calculateRDAfterGame(glicko$Grass_RatingsGames[row_nr_loser], glicko$Grass_RatingsGames[row_nr_winner], glicko$Grass_rdGames[row_nr_loser], glicko$Grass_rdGames[row_nr_winner], GamesResultLoser)
    } else if(allGames$Surface[i] == "Clay") {
      glicko$Clay_RatingsGames[row_nr_winner]                   <- calculateNewGlicko(glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_rdGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser], GamesResultWinner)
      glicko$Clay_RatingsGames[row_nr_loser]                    <- calculateNewGlicko(glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser], glicko$Clay_rdGames[row_nr_winner], GamesResultLoser)
      
      glicko$Clay_rdGames[row_nr_winner]               <- calculateRDAfterGame(glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_rdGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser], GamesResultWinner)
      glicko$Clay_rdGames[row_nr_loser]                <- calculateRDAfterGame(glicko$Clay_RatingsGames[row_nr_loser], glicko$Clay_RatingsGames[row_nr_winner], glicko$Clay_rdGames[row_nr_loser], glicko$Clay_rdGames[row_nr_winner], GamesResultLoser)
      
    } else if(allGames$Surface[i] == "Carpet") {
      #Since carpet is not in use since 2009 no carpet variables will be saved
    }
  } else {
    print("ERROR: Player cannot be matched with Glicko")
  }
}

saveDatasetsWithRating(allGames)

endTime <- Sys.time()

loadtime <- endTime - startTime
print(loadtime)