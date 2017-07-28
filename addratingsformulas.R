library(stringr)

InitializeRating = function(player){
  #player = SetContinentsAndNationalities(player)
  rating <- InitializeRatingVariables(player)
 
}

InitializeRatingVariables = function(rating){
  numberOfPlayers = nrow(rating)
  
  #Add start rating and number of games
  rating$Ratings = rep(1500, numberOfPlayers)
  rating$games = rep(0, numberOfPlayers)
  rating$games_won = rep(0, numberOfPlayers)
  
  #Create Speciality Ratings
  rating$Hard_Ratings = rating$Ratings
  rating$Hard_games = rating$games
  rating$Hard_games = rating$games
  rating$Hard_games_won = rating$games
  
  rating$Grass_Ratings = rating$Ratings
  rating$Grass_games = rating$games
  rating$Grass_games_won = rating$games
  
  rating$Clay_Ratings = rating$Ratings
  rating$Clay_games = rating$games
  rating$Clay_games_won = rating$games
  
  rating$NotHard_Ratings = rating$Ratings
  rating$NotHard_games =  rating$games
  rating$NotHard_games_won =  rating$games
  
  rating$Bo3_Ratings =  rating$Ratings
  rating$Bo3_games = rating$games
  rating$Bo3_games_won = rating$games
  
  rating$Bo5_Ratings = rating$Ratings
  rating$Bo5_games = rating$games
  rating$Bo5_games_won = rating$games

  return(rating)
}

SetContinentsAndNationalities = function(rating) {
   numberOfPlayers = nrow(rating)
   
   atp_players = read.table("Data/datasets/atp_players2.csv", header = T, sep = ",", 
                            quote = "\"", fill = TRUE)
   
   atp_players  <- mutate(atp_players , firstName = str_replace_all(trimws(tolower(str_replace_all(firstName, "[^[:alnum:]]", " "))), "  ", " "))
   atp_players  <- mutate(atp_players , lastName = str_replace_all(trimws(tolower(str_replace_all(lastName, "[^[:alnum:]]", " "))), "  ", " "))
   
  
  rating$Nationality <- rep(NA, numberOfPlayers)
  rating$Handed      <- rep(NA, numberOfPlayers)
   
   for (i in 1 : length(rating$playername)) {
     name <- rating$playername[i]
     if (name == "") {
       next()
     }
     name <- gsub("Jr.", "", name)
     
     name <- unlist(strsplit(as.character(name), ' (?=[^ ]+$)', perl = TRUE))
#     name = unlist(strsplit(as.character(name), ' (?=[^ ]+$)', perl=TRUE))
     lastName <- name[1]
     firstName <- name[2]
     
    
     indexLastName <- grep(lastName, atp_players$lastName ,ignore.case=TRUE)
    if (sum(!is.na(indexLastName)) > 0) {
       for (playerNumberAtp in indexLastName) {
         if (startsWith(as.character(atp_players$firstName[playerNumberAtp]), substring(firstName, 1, 1))) {
           rating$Nationality[i] = as.character(atp_players$Nationality[playerNumberAtp])
           rating$Handed[i] = as.character(atp_players$Handed[playerNumberAtp])
         }
       }
     } else {
       if (lastName == "Nadal Parera"){
         lastName = "Nadal"
       }
       if (lastName == "Hantschek") {
         lastName = "Hantschk"
       }
       if (lastName =="Roger-Vasselin"){
         lastName = "Vasselin"
       }
       if (lastName ==" Hajek"){
         lastName = "Hajek"
       }
       
       lastName = gsub("-", " ", lastName)
       lastName = gsub("\'", "", lastName)
      
       indexLastName = grep(lastName, atp_players$lastName ,ignore.case=TRUE)
       if (sum(!is.na(indexLastName)) > 0) {
         for (j in 1:length(indexLastName)) {
           playerNumberAtp = indexLastName[j]
           if (startsWith(as.character(atp_players$firstName[playerNumberAtp]), substring(firstName, 1, 1))) {
             rating$Nationality[i] = as.character(atp_players$Nationality[playerNumberAtp])
             rating$Handed[i] = as.character(atp_players$Handed[playerNumberAtp])
           }
         }
       }
     }
   }
   
   #Add country codes and continent, apparantly the files uses IOC
   countrycodes = read.table("Data/datasets/countrycodes.csv", header = T, sep = ",", 
                             quote = "\"", fill = TRUE)
   
   rating$Country = rep(NA, numberOfPlayers)
   rating$Continent = rep(NA, numberOfPlayers)
   
   for (i in 1 : length(rating$playername)) {
     country = rating$Nationality[i]
     countryCodePlayer = match(country, countrycodes$IOC)
     rating$Country[i] = as.character(countrycodes$name[countryCodePlayer])
     rating$Continent[i] = as.character(countrycodes$Continent[countryCodePlayer])
   }
   return(rating)
}

#This functions creates empty lists for locations, ratings and uncertainty
InitializeRatingVariablesForGames = function(dataset){
  
  rows = nrow(dataset)
  
   dataset$Country = rep(NA, rows)
   dataset$Winner_home = rep(NA, rows)
   dataset$Loser_home = rep(NA, rows)
   dataset$WinnerisHome = rep(NA, rows)
   dataset$LoserisHome = rep(NA, rows)
  
  # dataset$Winner_games       = rep(NA, rows)
  # dataset$Loser_games        = rep(NA, rows)
  dataset$Uncertainty        = rep(NA, rows)
  dataset$Uncertainty2       = rep(NA, rows)
  dataset$UncertaintySurface = rep(NA, rows)
  dataset$UncertaintyBestOf  = rep(NA, rows)
  
  dataset$Winner_rating      = rep(NA, rows)
  dataset$Winner_ratingClay  = rep(NA, rows)
  dataset$Winner_ratingHard  = rep(NA, rows)
  dataset$Winner_ratingNotHard  = rep(NA, rows)
  dataset$Winner_ratingGrass = rep(NA, rows)
  dataset$Winner_ratingBo3   = rep(NA, rows)
  dataset$Winner_ratingBo5   = rep(NA, rows)
  
  dataset$Loser_rating      = rep(NA, rows)
  dataset$Loser_ratingClay  = rep(NA, rows)
  dataset$Loser_ratingHard  = rep(NA, rows)
  dataset$Loser_ratingNotHard  = rep(NA, rows)
  dataset$Loser_ratingGrass = rep(NA, rows)
  dataset$Loser_ratingBo3   = rep(NA, rows)
  dataset$Loser_ratingBo5   = rep(NA, rows)
  
  dataset$Winner_expectationBasedOnRating = rep(NA, rows)
  dataset$Loser_expectationBasedOnRating  = rep(NA, rows)
  dataset$Winner_expectationSurfaceBasedOnRating = rep(NA, rows)
  dataset$Loser_expectationSurfaceBasedOnRating  = rep(NA, rows)
  dataset$Winner_expectationBestOfBasedOnRating = rep(NA, rows)
  dataset$Loser_expectationBestOfBasedOnRating  = rep(NA, rows)
  
  dataset$Winner_skillBo3  = rep(NA, rows)
  dataset$Winner_skillBo5  = rep(NA, rows)
  dataset$Loser_skillBo3  = rep(NA, rows)
  dataset$Loser_skillBo5  = rep(NA, rows)
  
  return(dataset)
}



getWinExpectationBasedOnRating = function(rating_winner, rating_loser, perspective = "winner"){
  Winner_expectationBasedOnRating = 1 - 1 / (1 + 10 ^ ((rating_winner - rating_loser) / 400))
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

# addHomePlayers = function(Games, rating, i, matchDetails){
#   Games$Country[i]        = matchDetails$Country
#   Games$Winner_country[i] = matchDetails$Winner_country
#   Games$Loser_country[i]  = matchDetails$Loser_country
#   Games$WinnerisHome[i]   = as.numeric(matchDetails$Winner_country == matchDetails$Country)
#   Games$LoserisHome[i]    = as.numeric(matchDetails$Loser_country == matchDetails$Country)
#   
#   if (is.na(Games$WinnerisHome[i])) {
#     Games$WinnerisHome[i] = 0
#   }
#   
#   if (is.na(Games$LoserisHome[i])) {
#     Games$LoserisHome[i] = 0
#   }
#   return(Games)
# }



getBo5vsBo3Skill <- function(total_won_matches_Bo5, total_won_matches_Bo3, total_matches_Bo5, total_matches_Bo3, min_games_required = 10) {
  if (total_matches_Bo5 <= min_games_required | total_matches_Bo3 <= min_games_required) {
    #if no games in bo5 or bo3 the skill will be 0
    Bo5vsBo3_skill <- 0
  } else {
    perc_won_matches_Bo5 <- total_won_matches_Bo5 / total_matches_Bo5
    perc_won_matches_Bo3 <- total_won_matches_Bo3 / total_matches_Bo3
    Bo5vsBo3_skill <- perc_won_matches_Bo5 - perc_won_matches_Bo3
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
  #Got changed after found out that constant 20.6 is better when you remove a lot of the games
  #return (250 / (numberOfGames + 12) ^ 0.44)
  25
}