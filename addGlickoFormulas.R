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

#Formula for updating RD: RD' = sqrt(RD^2 + t * c ^ 2), with t the periods passed (days in our case)
updateRDBeforeGame <- function(rd, lastGameDate, gameDate, c = 2.5, maxRD = 110) {
  if(is.na(lastGameDate)) {
    return(maxRD)
  }
  df = "%m/%d/%Y"
  
  lastGameDate = as.Date(as.character(lastGameDate), format = df)
  gameDate  = as.Date(as.character(gameDate), format = df)
  
  timeDiff = as.numeric(abs(lastGameDate - gameDate))
  min(maxRD, sqrt(rd ^ 2 + timeDiff * c ^ 2))
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