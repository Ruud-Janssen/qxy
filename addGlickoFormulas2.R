InitializeGlicko <- function(player, startRating, startRD){
  glicko <- InitializeGlickoVariables(player)
}

InitializeGlickoVariables <- function(glicko) {
  startRating <- 1500
  startRD     <- 350
  
  numberOfPlayers <- nrow(glicko)
  
  #Add start rating for games and number of matches
  glicko$RatingsGames   <- rep(startRating, numberOfPlayers)
  glicko$rdGames        <- rep(startRD, numberOfPlayers)
  glicko$last_game <- rep(NA, numberOfPlayers)
  #Create Speciality Ratings
  glicko$Hard_RatingsGames   <- glicko$Ratings
  glicko$Hard_rdGames        <- glicko$rd
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
  
  dataset$Winner_glickoGames        = rep(NA, rows)
  dataset$Winner_glickordGames     = rep(NA, rows)
  dataset$Winner_glickoClayGames    = rep(NA, rows)
  dataset$Winner_glickordClayGames  = rep(NA, rows)
  dataset$Winner_glickoHardGames    = rep(NA, rows)
  dataset$Winner_glickordHardGames  = rep(NA, rows)
  dataset$Winner_glickoGrassGames   = rep(NA, rows)
  dataset$Winner_glickordGrassGames = rep(NA, rows)
  dataset$Winner_glickoBo3Games     = rep(NA, rows)
  dataset$Winner_glickordBo3Games   = rep(NA, rows)
  dataset$Winner_glickoBo5Games     = rep(NA, rows)
  dataset$Winner_glickordBo5Games   = rep(NA, rows)
  
  dataset$Winner_glickoPoints        = rep(NA, rows)
  dataset$Winner_glickordPoints     = rep(NA, rows)
  dataset$Winner_glickoClayPoints    = rep(NA, rows)
  dataset$Winner_glickordClayPoints  = rep(NA, rows)
  dataset$Winner_glickoHardPoints    = rep(NA, rows)
  dataset$Winner_glickordHardPoints  = rep(NA, rows)
  dataset$Winner_glickoGrassPoints   = rep(NA, rows)
  dataset$Winner_glickordGrassPoints = rep(NA, rows)
  dataset$Winner_glickoBo3Points     = rep(NA, rows)
  dataset$Winner_glickordBo3Points   = rep(NA, rows)
  dataset$Winner_glickoBo5Points     = rep(NA, rows)
  dataset$Winner_glickordBo5Points   = rep(NA, rows)
  
  
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
  
  dataset$Loser_glickoGames        = rep(NA, rows)
  dataset$Loser_glickordGames     = rep(NA, rows)
  dataset$Loser_glickoClayGames    = rep(NA, rows)
  dataset$Loser_glickordClayGames  = rep(NA, rows)
  dataset$Loser_glickoHardGames    = rep(NA, rows)
  dataset$Loser_glickordHardGames  = rep(NA, rows)
  dataset$Loser_glickoGrassGames   = rep(NA, rows)
  dataset$Loser_glickordGrassGames = rep(NA, rows)
  dataset$Loser_glickoBo3Games     = rep(NA, rows)
  dataset$Loser_glickordBo3Games   = rep(NA, rows)
  dataset$Loser_glickoBo5Games     = rep(NA, rows)
  dataset$Loser_glickordBo5Games   = rep(NA, rows)
  
  dataset$Loser_glickoPoints        = rep(NA, rows)
  dataset$Loser_glickordPoints     = rep(NA, rows)
  dataset$Loser_glickoClayPoints    = rep(NA, rows)
  dataset$Loser_glickordClayPoints  = rep(NA, rows)
  dataset$Loser_glickoHardPoints    = rep(NA, rows)
  dataset$Loser_glickordHardPoints  = rep(NA, rows)
  dataset$Loser_glickoGrassPoints   = rep(NA, rows)
  dataset$Loser_glickordGrassPoints = rep(NA, rows)
  dataset$Loser_glickoBo3Points     = rep(NA, rows)
  dataset$Loser_glickordBo3Points   = rep(NA, rows)
  dataset$Loser_glickoBo5Points     = rep(NA, rows)
  dataset$Loser_glickordBo5Points   = rep(NA, rows)
  
  dataset$Winner_expectationBasedOnGlicko = rep(NA, rows)
  dataset$Loser_expectationBasedOnGlicko  = rep(NA, rows)

  return(dataset)
}

#Formula for updating RD: RD' = sqrt(RD^2 + t * c ^ 2), with t the periods passed (days in our case)
updateRDBeforeGame <- function(rd, lastGameDate, gameDate, maxRD = 40, c = 1) {
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

calculateNewGlicko = function(ratingPlayer, ratingOpponent, rdPlayer, rdOpponent, result, mp = 1) {
  q = 0.0057565
  gRD = g(rdOpponent)
  E = 1 / (1 + 10 ^ (-gRD * (ratingPlayer - ratingOpponent)/ 400))
  dsquared = (q ^ 2 * gRD ^ 2 * E * (1 - E)) ^ - 1
  ratingPlayer + mp * q / (1 / rdPlayer ^ 2 + 1 / dsquared) * gRD * (result - E)
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

calculateFractionNetBreakGamesWinnerWon <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  percentWonGames <- 0.5 + (wonGames - lostGames) / (wonGames + lostGames)
}
calculateGames <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  Games <- wonGames + lostGames
}