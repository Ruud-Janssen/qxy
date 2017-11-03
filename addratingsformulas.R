library(stringr)
library(dplyr)

InitializeRating = function(player){

  numberOfPlayers <- nrow(player)
  rating <- player %>% mutate(Ratings = 1500,
                              games = 0,
                              games_won = 0,
                              
                              Hard_Ratings = Ratings,
                              Hard_games = 0,
                              Hard_games_won = 0,
                              
                              Grass_Ratings = Ratings,
                              Grass_games = 0,
                              Grass_games_won = 0,
                              
                              Clay_Ratings = Ratings,
                              Clay_games = 0,
                              Clay_games_won = 0,
                              
                              NotHard_Ratings = Ratings,
                              NotHard_games = 0,
                              NotHard_games_won = 0,
                              
                              Bo3_Ratings = Ratings,
                              Bo3_games = 0,
                              Bo3_games_won = 0,
                              
                              Bo5_Ratings = Ratings,
                              Bo5_games = 0,
                              Bo5_games_won = 0)
}

InitializeRatingServeReturn = function(rating){
  numberOfPlayers <- nrow(rating)
  rating <- rating %>% mutate(ServeRatings = 1550,
                              ReturnRatings = 1450,
                              games = 0,
                              games_won = 0,
                              
                              Hard_ServeRatings = ServeRatings,
                              Hard_ReturnRatings = ReturnRatings,
                              Hard_games = 0,
                              Hard_games_won = 0
                              )
}

SetContinentsAndCountries <- function(rating) {
  numberOfPlayers <- nrow(rating)
  
  countrycodes <- read.table("Data/datasets/countrycodes.csv", header = T, sep = ",", 
                            quote = "\"", fill = TRUE)
  
  rating <- rating %>% mutate(Country = NA,
                              Continent = NA)
  
  for (i in 1 : length(rating$playername)) {
    country             <- rating$Nationality[i]
    countryCodePlayer   <- match(country, countrycodes$IOC)
    rating$Country[i]   <- as.character(countrycodes$name[countryCodePlayer])
    rating$Continent[i] <- as.character(countrycodes$Continent[countryCodePlayer])
  }
  return(rating)
}

SetContinentsAndNationalities = function(rating) {
  numberOfPlayers <- nrow(rating)
   
  atp_players <- read.table("Data/datasets/atp_players2.csv", header = T, sep = ",", 
                            quote = "\"", fill = TRUE)
   
  atp_players  <- atp_players %>% mutate(
    firstName = str_replace_all(trimws(tolower(str_replace_all(firstName, "[^[:alnum:]]", " "))), "  ", " "), 
    lastName = str_replace_all(trimws(tolower(str_replace_all(lastName, "[^[:alnum:]]", " "))), "  ", " "),
    Nationality = NA,
    Handed = NA
  )
   
   for (i in 1 : length(rating$playername)) {
     name <- rating$playername[i]
     if (name == "") {
       next()
     }
     name <- gsub("Jr.", "", name)
     
     name <- unlist(strsplit(as.character(name), ' (?=[^ ]+$)', perl = TRUE))
#     name = unlist(strsplit(as.character(name), ' (?=[^ ]+$)', perl=TRUE))
     lastName  <- name[1]
     firstName <- name[2]
     
    
     indexLastName <- grep(lastName, atp_players$lastName ,ignore.case=TRUE)
    if (sum(!is.na(indexLastName)) > 0) {
       for (playerNumberAtp in indexLastName) {
         if (startsWith(as.character(atp_players$firstName[playerNumberAtp]), substring(firstName, 1, 1))) {
           rating$Nationality[i] <- as.character(atp_players$Nationality[playerNumberAtp])
           rating$Handed[i]      <- as.character(atp_players$Handed[playerNumberAtp])
         }
       }
     } else {
       if (lastName == "Nadal Parera"){
         lastName <- "Nadal"
       }
       if (lastName == "Hantschek") {
         lastName <- "Hantschk"
       }
       if (lastName =="Roger-Vasselin"){
         lastName <- "Vasselin"
       }
       if (lastName ==" Hajek"){
         lastName <- "Hajek"
       }
       
       lastName <- gsub("-", " ", lastName)
       lastName <- gsub("\'", "", lastName)
      
       indexLastName <- grep(lastName, atp_players$lastName ,ignore.case=TRUE)
       if (sum(!is.na(indexLastName)) > 0) {
         for (j in 1:length(indexLastName)) {
           playerNumberAtp <- indexLastName[j]
           if (startsWith(as.character(atp_players$firstName[playerNumberAtp]), substring(firstName, 1, 1))) {
             rating$Nationality[i] <- as.character(atp_players$Nationality[playerNumberAtp])
             rating$Handed[i]      <- as.character(atp_players$Handed[playerNumberAtp])
           }
         }
       }
     }
   }
   
   #Add country codes and continent, apparantly the files uses IOC
   countrycodes <- read.table("Data/datasets/countrycodes.csv", header = T, sep = ",", 
                             quote = "\"", fill = TRUE)
   
   rating$Country   <- NA
   rating$Continent <- NA
   
   for (i in 1 : length(rating$playername)) {
     country             <- rating$Nationality[i]
     countryCodePlayer   <- match(country, countrycodes$IOC)
     rating$Country[i]   <- as.character(countrycodes$name[countryCodePlayer])
     rating$Continent[i] <- as.character(countrycodes$Continent[countryCodePlayer])
   }
   return(rating)
}

#This functions creates empty lists for locations, ratings and uncertainty
InitializeRatingVariablesForGames = function(dataset){
  
  rows <- nrow(dataset)
  
  dataset <- dataset %>% mutate(Country      = NA,
                                Winner_home  = NA,
                                Loser_home   = NA,
                                WinnerisHome = NA,
                                LoserisHome  = NA,
                                
                                Uncertainty        = NA,
                                Uncertainty2       = NA,
                                UncertaintySurface = NA,
                                UncertaintyBestOf  = NA,
                                
                                Winner_rating        = NA,
                                Winner_ratingClay    = NA,
                                Winner_ratingHard    = NA,
                                Winner_ratingNotHard = NA,
                                Winner_ratingGrass   = NA,
                                Winner_ratingBo3     = NA,
                                Winner_ratingBo5     = NA,
                                
                                Loser_rating        = NA, 
                                Loser_ratingClay    = NA,
                                Loser_ratingHard    = NA,
                                Loser_ratingNotHard = NA,
                                Loser_ratingGrass   = NA,
                                Loser_ratingBo3     = NA,
                                Loser_ratingBo5     = NA,
                                
                                Winner_expectationBasedOnRating        = NA,
                                Loser_expectationBasedOnRating         = NA,
                                Winner_expectationSurfaceBasedOnRating = NA,
                                Loser_expectationSurfaceBasedOnRating  = NA,
                                Winner_expectationBestOfBasedOnRating  = NA,
                                Loser_expectationBestOfBasedOnRating   = NA,
                                
                                Winner_skillBo3 = NA,
                                Winner_skillBo5 = NA,
                                Loser_skillBo3  = NA,
                                Loser_skillBo5  = NA
                                )
}



getWinExpectationBasedOnRating = function(rating_winner, rating_loser, perspective = "winner"){
  Winner_expectationBasedOnRating <- 1 - 1 / (1 + 10 ^ ((rating_winner - rating_loser) / 400))
  if (perspective == "loser") { 1 - Winner_expectationBasedOnRating } else { Winner_expectationBasedOnRating }
}


getUncertainty <- function (total_matches_winner, total_matches_loser, type_uncertainty = 1) {
  uncertainty <- 2
  if (type_uncertainty == 1) {
    if (total_matches_winner != 0 & total_matches_loser != 0) {
      uncertainty <- 1 / (total_matches_winner * total_matches_loser)  
    }
  } else if (type_uncertainty == 2) {
    if (total_matches_winner != 0 & total_matches_loser != 0) {
      uncertainty <- 1 / min(total_matches_winner, total_matches_loser)
    }
  }
  return (uncertainty)
}

getBo5vsBo3Skill <- function(total_won_matches_Bo5, total_won_matches_Bo3, total_matches_Bo5, total_matches_Bo3, min_games_required = 10) {
  if (total_matches_Bo5 <= min_games_required | total_matches_Bo3 <= min_games_required) {
    #if no games in bo5 or bo3 the skill will be 0
    Bo5vsBo3_skill <- 0
  } else {
    perc_won_matches_Bo5 <- total_won_matches_Bo5 / total_matches_Bo5
    perc_won_matches_Bo3 <- total_won_matches_Bo3 / total_matches_Bo3
    Bo5vsBo3_skill       <- perc_won_matches_Bo5 - perc_won_matches_Bo3
  }
  return (Bo5vsBo3_skill)
}

# 
# getBo5vsBo3SkillBasedOnRating <- function(Bo5_plus_score, Bo3_plus_score, total_matches_Bo5, total_matches_Bo3, min_games_required = 10) {
#   if (total_matches_Bo5 <= min_games_required | total_matches_Bo3 <= min_games_required) {
#     #if no games in bo5 or bo3 the skill will be 0
#     Bo5vsBo3_skill_based_on_rating <- 0
#   } else {
#     mean_Bo5_plus_score <- Bo5_plus_score / total_matches_Bo5
#     mean_Bo3_plus_score <- Bo3_plus_score / total_matches_Bo3
#     Bo5vsBo3_skill_based_on_rating <- mean_Bo5_plus_score - mean_Bo3_plus_score
#   }
#   return (Bo5vsBo3_skill_based_on_rating)
# }

calculateNewRating <- function(current_rating, total_matches, expectation_based_on_rating, result) {
  Kfactor <- K(total_matches)

  #OLD CODE IN COMMENT but already defined elsewhere, expectationWinner = 1 - 1 / (1 + 10 ^ ((ratingWinner - ratingLoser)/ 400))
  new_rating <- current_rating + Kfactor * (result - expectation_based_on_rating)
}

K <- function(total_matches) {
  15
}


calculateFractionNetBreakGamesWinnerWon <- function(row) {
  wonGames             <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames            <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  percentWonBreakGames <- 0.5 + (wonGames - lostGames) / (wonGames + lostGames)
}

calculateFractionGamesWinnerWon <- function(row) {
  wonGames        <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames       <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  percentWonGames <- wonGames / (wonGames + lostGames)
}


calculateGames <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  Games     <- wonGames + lostGames
}