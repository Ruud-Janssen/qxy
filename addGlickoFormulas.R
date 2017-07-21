InitializeGlicko <- function(player, startRating, startRD){
  glicko <- InitializeGlickoVariables(player)
}

InitializeGlickoVariables <- function(glicko) {
  startRating <- 1500
  startRD     <- 350
  
  numberOfPlayers <- nrow(glicko)
  
  #Add start rating and number of games
  glicko$Ratings   <- rep(startRating, numberOfPlayers)
  glicko$rd        <- rep(startRD, numberOfPlayers)
  glicko$last_game <- rep(NA, numberOfPlayers)
  
  #Create Speciality Ratings
  glicko$Hard_Ratings   <- glicko$Ratings
  glicko$Hard_rd        <- glicko$rd
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

getMatchDetailsGlicko = function(game, glicko) {
  matchDetails = list()
  
  matchDetails$Winner  = game$Winner
  matchDetails$Loser   = game$Loser
  matchDetails$Surface = game$Surface
  if(is.na(matchDetails$Surface)) {
    matchDetails$Surface = "Missing"
  }
  
  matchDetails$Best.of = game$Best.of
  matchDetails$Date    = game$Date
  
  matchDetails$IndexWinner = match(matchDetails$Winner, glicko$Players)
  matchDetails$IndexLoser  = match(matchDetails$Loser, glicko$Players)

  return(matchDetails)
}

addGlickoVariables = function(Games, glicko, i, matchDetails) {
  Games$Winner_glicko[i]          = glicko$Ratings[matchDetails$IndexWinner]
  Games$Winner_glickord[i]        = glicko$rd[matchDetails$IndexWinner]
  Games$Winner_glickoClay[i]      = glicko$Clay_Ratings[matchDetails$IndexWinner]
  Games$Winner_glickordClay[i]    = glicko$Clay_rd[matchDetails$IndexWinner]
  Games$Winner_glickoHard[i]      = glicko$Hard_Ratings[matchDetails$IndexWinner]
  Games$Winner_glickordHard[i]    = glicko$Hard_rd[matchDetails$IndexWinner]
  Games$Winner_glickoGrass[i]     = glicko$Grass_Ratings[matchDetails$IndexWinner]
  Games$Winner_glickordGrass[i]   = glicko$Grass_rd[matchDetails$IndexWinner]
  Games$Winner_glickoNotHard[i]   = glicko$NotHard_Ratings[matchDetails$IndexWinner]
  Games$Winner_glickordNotHard[i] = glicko$NotHard_rd[matchDetails$IndexWinner]
  Games$Winner_glickoBo3[i]       = glicko$Bo3_Ratings[matchDetails$IndexWinner]
  Games$Winner_glickordBo3[i]     = glicko$Bo3_rd[matchDetails$IndexWinner]
  Games$Winner_glickoBo5[i]       = glicko$Bo5_Ratings[matchDetails$IndexWinner]
  Games$Winner_glickordBo5[i]     = glicko$Bo5_rd[matchDetails$IndexWinner]
  
  Games$Loser_glicko[i]          = glicko$Ratings[matchDetails$IndexLoser]
  Games$Loser_glickord[i]        = glicko$rd[matchDetails$IndexLoser]
  Games$Loser_glickoClay[i]      = glicko$Clay_Ratings[matchDetails$IndexLoser]
  Games$Loser_glickordClay[i]    = glicko$Clay_rd[matchDetails$IndexLoser]
  Games$Loser_glickoHard[i]      = glicko$Hard_Ratings[matchDetails$IndexLoser]
  Games$Loser_glickordHard[i]    = glicko$Hard_rd[matchDetails$IndexLoser]
  Games$Loser_glickoGrass[i]     = glicko$Grass_Ratings[matchDetails$IndexLoser]
  Games$Loser_glickordGrass[i]   = glicko$Grass_rd[matchDetails$IndexLoser]
  Games$Loser_glickoNotHard[i]   = glicko$NotHard_Ratings[matchDetails$IndexLoser]
  Games$Loser_glickordNotHard[i] = glicko$NotHard_rd[matchDetails$IndexLoser]
  Games$Loser_glickoBo3[i]       = glicko$Bo3_Ratings[matchDetails$IndexLoser]
  Games$Loser_glickordBo3[i]     = glicko$Bo3_rd[matchDetails$IndexLoser]
  Games$Loser_glickoBo5[i]       = glicko$Bo5_Ratings[matchDetails$IndexLoser]
  Games$Loser_glickordBo5[i]     = glicko$Bo5_rd[matchDetails$IndexLoser]
  
  #Games$Winner_expectationBasedOnRating[i] = 1 - 1 / (1 + 10 ^ ((rating$Ratings[matchDetails$IndexWinner] 
  #                                                               - rating$Ratings[matchDetails$IndexLoser])/ 400))
  #Games$Loser_expectationBasedOnRating[i]  = 1 - Games$Winner_expectationBasedOnRating[i]
  
  return(Games)
}

#Formula for updating RD: RD' = sqrt(RD^2 + t * c ^ 2), with t the periods passed (days in our case)
updateRDBeforeGame <- function(rd, lastGameDate, gameDate, c = 3.2) {
  if(is.na(lastGameDate)) {
    return(350)
  }
  df = "%m/%d/%Y"
  
  lastGameDate = as.Date(as.character(lastGameDate), format = df)
  gameDate  = as.Date(as.character(gameDate), format = df)
  
  timeDiff = as.numeric(abs(lastGameDate - gameDate))
  min(350, sqrt(rd ^ 2 + timeDiff * c ^ 2))
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