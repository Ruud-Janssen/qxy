rm(list = ls())
source("formulas.R")
source("addglickoformulas.R")
source("addratingsformulas.R")

startTime <- Sys.time ()

allGames <- getAllGamesWithRating()
player   <- getPlayers()

# Create Ratings for all players, ratings are adapted after each match
glicko <- InitializeGlicko(player)

allGames <- RemoveWalkOvers(allGames)
allGames <- InitializeGlickoVariablesForGames(allGames)

Nall <- nrow(allGames)

for (i in 1: Nall) {
  # get matching winner and loserplayer in glicko and save the rownr
  row_nr_winner <- which(glicko$id == allGames$idWinner[i])
  row_nr_loser  <- which(glicko$id == allGames$idLoser[i])
  
  if (row_nr_winner > 0 & row_nr_loser > 0) {
    gameDate <- allGames$Date[i]
    #update RDs before game
    glicko$rd[row_nr_winner] <- updateRDBeforeGame(glicko$rd[row_nr_winner], glicko$last_game[row_nr_winner], gameDate)
    glicko$rd[row_nr_loser]  <- updateRDBeforeGame(glicko$rd[row_nr_loser], glicko$last_game[row_nr_loser], gameDate)
    #Update date last game
    glicko$last_game[row_nr_winner] <- gameDate 
    glicko$last_game[row_nr_loser]  <- gameDate
    
    if (allGames$Best.of[i] == 3) {   
      glicko$Bo3_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Bo3_rd[row_nr_winner], glicko$Bo3_last_game[row_nr_winner], gameDate)
      glicko$Bo3_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Bo3_rd[row_nr_loser], glicko$Bo3_last_game[row_nr_loser], gameDate)
      #Update date last game
      glicko$Bo3_last_game[row_nr_winner] <- gameDate 
      glicko$Bo3_last_game[row_nr_loser]  <- gameDate
    } else {
      glicko$Bo5_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Bo5_rd[row_nr_winner], glicko$Bo5_last_game[row_nr_winner], gameDate)
      glicko$Bo5_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Bo5_rd[row_nr_loser], glicko$Bo5_last_game[row_nr_loser], gameDate)
      #Update date last game
      glicko$Bo5_last_game[row_nr_winner] <- gameDate 
      glicko$Bo5_last_game[row_nr_loser]  <- gameDate
    }  
      
    # surface dependent variables
    if (allGames$Surface[i] == "Hard") {
      glicko$Hard_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Hard_rd[row_nr_winner], glicko$Hard_last_game[row_nr_winner], gameDate)
      glicko$Hard_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Hard_rd[row_nr_loser], glicko$Hard_last_game[row_nr_loser], gameDate)
      #Update date last game
      glicko$Hard_last_game[row_nr_winner] <- gameDate 
      glicko$Hard_last_game[row_nr_loser]  <- gameDate
    } else { # Not Hard
      glicko$NotHard_rd[row_nr_winner] <- updateRDBeforeGame(glicko$NotHard_rd[row_nr_winner], glicko$NotHard_last_game[row_nr_winner], gameDate)
      glicko$NotHard_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$NotHard_rd[row_nr_loser], glicko$NotHard_last_game[row_nr_loser], gameDate)
      #Update date last game
      glicko$NotHard_last_game[row_nr_winner] <- gameDate 
      glicko$NotHard_last_game[row_nr_loser]  <- gameDate
    }
    if(allGames$Surface[i] == "Grass") {
      glicko$Grass_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Grass_rd[row_nr_winner], glicko$Grass_last_game[row_nr_winner], gameDate)
      glicko$Grass_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Grass_rd[row_nr_loser], glicko$Grass_last_game[row_nr_loser], gameDate)
      #Update date last game
      glicko$Grass_last_game[row_nr_winner] <- gameDate 
      glicko$Grass_last_game[row_nr_loser]  <- gameDate
    } else if(allGames$Surface[i] == "Clay") {
      glicko$Clay_rd[row_nr_winner] <- updateRDBeforeGame(glicko$Clay_rd[row_nr_winner], glicko$Clay_last_game[row_nr_winner], gameDate)
      glicko$Clay_rd[row_nr_loser]  <- updateRDBeforeGame(glicko$Clay_rd[row_nr_loser], glicko$Clay_last_game[row_nr_loser], gameDate)
      #Update date last game
      glicko$Clay_last_game[row_nr_winner] <- gameDate 
      glicko$Clay_last_game[row_nr_loser]  <- gameDate
    } 
    else if(allGames$Surface[i] == "Carpet") {
      #Since carpet is not in use since 2009 no carpet variables will be saved
    }

    allGames$Winner_glicko[i]                            <- calculateGlickoVariable(glicko$Ratings[row_nr_winner], glicko$rd[row_nr_winner], glicko$rd[row_nr_loser])
    allGames$Winner_glickoClay[i]                        <- calculateGlickoVariable(glicko$Clay_Ratings[row_nr_winner], glicko$Clay_rd[row_nr_winner], glicko$Clay_rd[row_nr_loser])
    allGames$Winner_glickoHard[i]                        <- calculateGlickoVariable(glicko$Hard_Ratings[row_nr_winner], glicko$Hard_rd[row_nr_winner], glicko$Hard_rd[row_nr_loser])
    allGames$Winner_glickoGrass[i]                       <- calculateGlickoVariable(glicko$Grass_Ratings[row_nr_winner], glicko$Grass_rd[row_nr_winner], glicko$Grass_rd[row_nr_loser])
    allGames$Winner_glickoNotHard[i]                     <- calculateGlickoVariable(glicko$NotHard_Ratings[row_nr_winner], glicko$NotHard_rd[row_nr_winner], glicko$NotHard_rd[row_nr_loser])
    allGames$Winner_glickoBo3[i]                         <- calculateGlickoVariable(glicko$Bo3_Ratings[row_nr_winner], glicko$Bo3_rd[row_nr_winner], glicko$Bo3_rd[row_nr_loser])
    allGames$Winner_glickoBo5[i]                         <- calculateGlickoVariable(glicko$Bo5_Ratings[row_nr_winner], glicko$Bo5_rd[row_nr_winner], glicko$Bo5_rd[row_nr_loser])
 
    allGames$Loser_glicko[i]                             <- calculateGlickoVariable(glicko$Ratings[row_nr_loser], glicko$rd[row_nr_winner], glicko$rd[row_nr_loser])
    allGames$Loser_glickoClay[i]                         <- calculateGlickoVariable(glicko$Clay_Ratings[row_nr_loser], glicko$Clay_rd[row_nr_winner], glicko$Clay_rd[row_nr_loser])
    allGames$Loser_glickoHard[i]                         <- calculateGlickoVariable(glicko$Hard_Ratings[row_nr_loser], glicko$Hard_rd[row_nr_winner], glicko$Hard_rd[row_nr_loser])
    allGames$Loser_glickoGrass[i]                        <- calculateGlickoVariable(glicko$Grass_Ratings[row_nr_loser], glicko$Grass_rd[row_nr_winner], glicko$Grass_rd[row_nr_loser])
    allGames$Loser_glickoNotHard[i]                      <- calculateGlickoVariable(glicko$NotHard_Ratings[row_nr_loser], glicko$NotHard_rd[row_nr_winner], glicko$NotHard_rd[row_nr_loser])
    allGames$Loser_glickoBo3[i]                          <- calculateGlickoVariable(glicko$Bo3_Ratings[row_nr_loser], glicko$Bo3_rd[row_nr_winner], glicko$Bo3_rd[row_nr_loser])
    allGames$Loser_glickoBo5[i]                          <- calculateGlickoVariable(glicko$Bo5_Ratings[row_nr_loser], glicko$Bo5_rd[row_nr_winner], glicko$Bo5_rd[row_nr_loser])
  
    # Update glicko        
    glicko$Ratings[row_nr_winner]                        <- calculateNewGlicko(glicko$Ratings[row_nr_winner], glicko$Ratings[row_nr_loser], glicko$rd[row_nr_winner], glicko$rd[row_nr_loser], 1)
    glicko$Ratings[row_nr_loser]                         <- calculateNewGlicko(glicko$Ratings[row_nr_loser], glicko$Ratings[row_nr_winner], glicko$rd[row_nr_loser], glicko$rd[row_nr_winner], 0)
  
    glicko$rd[row_nr_winner] <- calculateRDAfterGame(glicko$Ratings[row_nr_winner], glicko$Ratings[row_nr_loser], glicko$rd[row_nr_winner], glicko$rd[row_nr_loser], 1)
    glicko$rd[row_nr_loser]  <- calculateRDAfterGame(glicko$Ratings[row_nr_loser], glicko$Ratings[row_nr_winner], glicko$rd[row_nr_loser], glicko$rd[row_nr_winner], 0)
    
    # bo3 and bo5 glickos  
    if (allGames$Best.of[i] == 3) {  
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
    if (allGames$Surface[i] == "Hard") {
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
    if(allGames$Surface[i] == "Grass") {
      glicko$Grass_Ratings[row_nr_winner]                 <- calculateNewGlicko(glicko$Grass_Ratings[row_nr_winner], glicko$Grass_Ratings[row_nr_loser], glicko$Grass_rd[row_nr_winner], glicko$Grass_rd[row_nr_loser], 1)
      glicko$Grass_Ratings[row_nr_loser]                  <- calculateNewGlicko(glicko$Grass_Ratings[row_nr_loser], glicko$Grass_Ratings[row_nr_winner], glicko$Grass_rd[row_nr_loser], glicko$Grass_rd[row_nr_winner], 0)
      
      glicko$Grass_rd[row_nr_winner]                      <- calculateRDAfterGame(glicko$Grass_Ratings[row_nr_winner], glicko$Grass_Ratings[row_nr_loser], glicko$Grass_rd[row_nr_winner], glicko$Grass_rd[row_nr_loser], 1)
      glicko$Grass_rd[row_nr_loser]                       <- calculateRDAfterGame(glicko$Grass_Ratings[row_nr_loser], glicko$Grass_Ratings[row_nr_winner], glicko$Grass_rd[row_nr_loser], glicko$Grass_rd[row_nr_winner], 0)
      
    } else if(allGames$Surface[i] == "Clay") {
      glicko$Clay_Ratings[row_nr_winner]                  <- calculateNewGlicko(glicko$Clay_Ratings[row_nr_winner], glicko$Clay_Ratings[row_nr_loser], glicko$Clay_rd[row_nr_winner], glicko$Clay_rd[row_nr_loser], 1)
      glicko$Clay_Ratings[row_nr_loser]                   <- calculateNewGlicko(glicko$Clay_Ratings[row_nr_loser], glicko$Clay_Ratings[row_nr_winner], glicko$Clay_rd[row_nr_loser], glicko$Clay_rd[row_nr_winner], 0)
      
      glicko$Clay_rd[row_nr_winner]                       <- calculateRDAfterGame(glicko$Clay_Ratings[row_nr_winner], glicko$Clay_Ratings[row_nr_loser], glicko$Clay_rd[row_nr_winner], glicko$Clay_rd[row_nr_loser], 1)
      glicko$Clay_rd[row_nr_loser]                        <- calculateRDAfterGame(glicko$Clay_Ratings[row_nr_loser], glicko$Clay_Ratings[row_nr_winner], glicko$Clay_rd[row_nr_loser], glicko$Clay_rd[row_nr_winner], 0)
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