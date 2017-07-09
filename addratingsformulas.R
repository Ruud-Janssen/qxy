InitializeRating = function(winners, losers){
  winners_all = as.data.frame(matrix(nrow = length(winners), ncol = 0))
  winners_all$Players = winners
  
  losers_all = as.data.frame(matrix(nrow = length(losers), ncol = 0))
  losers_all$Players = losers
  
  rating = rbind(winners_all, losers_all)
  rating = unique(rating)
  
  numberOfPlayers = nrow(rating)
  
  
  #Add start rating and number of games
  rating$Ratings = rep(1500, numberOfPlayers)
  rating$games = rep(0, numberOfPlayers)
  
  #Create Surface Ratings
  rating$Hard_Ratings = rating$Ratings
  rating$Hard_games = rating$games
  
  rating$Grass_Ratings = rating$Ratings
  rating$Grass_games = rating$games
  
  rating$Clay_Ratings = rating$Ratings
  rating$Clay_games = rating$games
  
  rating$Carpet_Ratings = rating$Ratings
  rating$Carpet_games = rating$games
  
  rating$NotHard_Ratings = rating$Ratings
  rating$NotHard_games =  rating$games
  
  rating$Bo5Played = rep(0, numberOfPlayers)
  rating$Bo5Won = rep(0, numberOfPlayers)
  rating$Bo5PlusScore = rep(0, numberOfPlayers)
  
  rating$Bo3Played = rep(0, numberOfPlayers)
  rating$Bo3Won = rep(0, numberOfPlayers)
  rating$Bo3PlusScore = rep(0, numberOfPlayers)
  
  
  atp_players = read.table("Data/datasets/atp_players.csv", header = T, sep = ",", 
                           quote = "\"", fill = TRUE)
  
  rating$Nationality = rep(NA, numberOfPlayers)
  rating$Handed = rep(NA, numberOfPlayers)
  
  for(i in 1 : length(rating$Players)) {
    name = rating$Players[i]
    if(name == ""){
      next()
    }
    name = gsub("Jr.", "", name)
    
    name = unlist(strsplit(as.character(name), '.', fixed = TRUE))[1]
    name = unlist(strsplit(as.character(name), ' (?=[^ ]+$)', perl=TRUE))
    lastName = name[1]
    firstName = name[2]
    
    
    indexLastName = grep(lastName, atp_players$lastName ,ignore.case=TRUE)
    if(sum(!is.na(indexLastName)) > 0) {
      for(j in 1:length(indexLastName)){
        playerNumberAtp = indexLastName[j]
        if(startsWith(as.character(atp_players$firstName[playerNumberAtp]), substring(firstName, 1, 1))) {
          rating$Nationality[i] = as.character(atp_players$Nationality[playerNumberAtp])
          rating$Handed[i] = as.character(atp_players$Handed[playerNumberAtp])
        }
      }
    } else {
      if(lastName == "Nadal-Parera"){
        lastName = "Nadal"
      }
      if(lastName == "Hantschek") {
        lastName = "Hantschk"
      }
      if(lastName =="Roger-Vasselin"){
        lastName = "Vasselin"
      }
      if(lastName ==" Hajek"){
        lastName = "Hajek"
      }
      
      
      lastName = gsub("-", " ", lastName)
      lastName = gsub("\'", "", lastName)
      
      indexLastName = grep(lastName, atp_players$lastName ,ignore.case=TRUE)
      if(sum(!is.na(indexLastName)) > 0) {
        for(j in 1:length(indexLastName)){
          playerNumberAtp = indexLastName[j]
          if(startsWith(as.character(atp_players$firstName[playerNumberAtp]), substring(firstName, 1, 1))) {
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
  
  for(i in 1 : length(rating$Players)) {
    country = rating$Nationality[i]
    countryCodePlayer = match(country, countrycodes$IOC)
    rating$Country[i] = as.character(countrycodes$name[countryCodePlayer])
    rating$Continent[i] = as.character(countrycodes$Continent[countryCodePlayer])
  }
  
  return(rating)
}

#This functions creates empty lists for locations, ratings and uncertainty
InitializeRatingVariables = function(dataset){
  
  rows = nrow(dataset)
  
  dataset$Country = rep(NA, rows)
  dataset$Winner_home = rep(NA, rows)
  dataset$Loser_home = rep(NA, rows)
  dataset$WinnerisHome = rep(NA, rows)
  dataset$LoserisHome = rep(NA, rows)
  
  dataset$Winner_games = rep(NA, rows)
  dataset$Loser_games = rep(NA, rows)
  dataset$Uncertainty = rep(NA, rows)
  dataset$Uncertainty2 = rep(NA, rows)
  
  dataset$Winner_rating = rep(NA, rows)
  dataset$Winner_ratingClay = rep(NA, rows)
  dataset$Winner_ratingHard = rep(NA, rows)
  dataset$Winner_ratingGrass = rep(NA, rows)
  
  dataset$Loser_rating = rep(NA, rows)
  dataset$Loser_ratingClay = rep(NA, rows)
  dataset$Loser_ratingHard = rep(NA, rows)
  dataset$Loser_ratingGrass = rep(NA, rows)
  
  dataset$Winner_skillBo5 = rep(NA, rows)
  dataset$Winner_skillBo3 = rep(NA, rows)
  dataset$Loser_skillBo5 = rep(NA, rows)
  dataset$Loser_skillBo3 = rep(NA, rows)
  
  dataset$Winner_expectationBasedOnRating = rep(NA, rows)
  dataset$Loser_expectationBasedOnRating = rep(NA, rows)
  
  dataset$Winner_skillBo5PlusScores = rep(NA, rows)
  dataset$Winner_skillBo3PlusScores = rep(NA, rows)
  
  dataset$Loser_skillBo5PlusScores = rep(NA, rows)
  dataset$Loser_skillBo3PlusScores = rep(NA, rows)
  
  return(dataset)
}

#Note that we assume here that the walkovers are removed
saveDatasetsWithRating = function(allGames, rating){
  
  Nt_r = nrow(RemoveWalkOvers(read.table("Data/datasets/train_rating.csv",  header = T, sep = ",", quote = "\"",
                                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)))
  Nt_m = nrow(RemoveWalkOvers(read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)))
  Ncv = nrow(RemoveWalkOvers(read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)))
  Ntest = nrow(RemoveWalkOvers(read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                                          colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)))
  
  firstindextrain_rating = 1
  lastindextrain_rating = Nt_r
  train_rating = allGames[firstindextrain_rating : lastindextrain_rating, ]
  
  firstindextrain_model = lastindextrain_rating + 1
  lastindextrain_model = lastindextrain_rating + Nt_m
  train_model = allGames[firstindextrain_model : lastindextrain_model, ]
  
  firstindexcv = lastindextrain_model + 1
  lastindexcv = lastindextrain_model + Ncv
  cv = allGames[firstindexcv : lastindexcv, ]
  
  firstindextest = lastindexcv + 1
  lastindextest = lastindexcv + Ntest
  test = allGames[firstindextest: lastindextest, ]
  
  write.csv(file = "Data/datasets/train_ratingWithRatings.csv", train_rating, row.names=FALSE)
  write.csv(file = "Data/datasets/train_modelWithRatings.csv", train_model, row.names=FALSE)
  write.csv(file = "Data/datasets/cvWithRatings.csv", cv, row.names=FALSE)
  write.csv(file = "Data/datasets/testWithRatings.csv", test, row.names=FALSE)
  
  write.csv(file = "Data/datasets/ratingafterTest.csv", 
            rating, row.names=FALSE)
}

addRatingVariables = function(Games, rating, i, matchDetails){
  Games$Winner_rating[i] = rating$Ratings[matchDetails$IndexWinner]
  Games$Winner_ratingClay[i] = rating$Clay_Ratings[matchDetails$IndexWinner]
  Games$Winner_ratingHard[i] = rating$Hard_Ratings[matchDetails$IndexWinner]
  Games$Winner_ratingGrass[i] = rating$Grass_Ratings[matchDetails$IndexWinner]
  Games$Winner_ratingNotHard[i] = rating$NotHard_Ratings[matchDetails$IndexWinner]
  
  Games$Loser_rating[i] = rating$Ratings[matchDetails$IndexLoser]
  Games$Loser_ratingClay[i] = rating$Clay_Ratings[matchDetails$IndexLoser]
  Games$Loser_ratingHard[i] = rating$Hard_Ratings[matchDetails$IndexLoser]
  Games$Loser_ratingGrass[i] = rating$Grass_Ratings[matchDetails$IndexLoser]
  Games$Loser_ratingNotHard[i] = rating$NotHard_Ratings[matchDetails$IndexLoser]
  
  Games$Winner_expectationBasedOnRating[i] = 1 - 1 / (1 + 10 ^ ((rating$Ratings[matchDetails$IndexWinner] 
                                                              - rating$Ratings[matchDetails$IndexLoser])/ 400))
  Games$Loser_expectationBasedOnRating[i] = 1 - Games$Winner_expectationBasedOnRating[i]
  
  return(Games)
}

getMatchDetails = function(game, rating){
  matchDetails = list()
  
  matchDetails$Winner = game$Winner
  matchDetails$Loser = game$Loser
  matchDetails$Surface = game$Surface
  matchDetails$Best.of = game$Best.of
  
  matchDetails$IndexWinner = match(matchDetails$Winner, rating$Players)
  matchDetails$IndexLoser = match(matchDetails$Loser, rating$Players)
  
  matchDetails$Winner_games = rating$games[matchDetails$IndexWinner]
  matchDetails$Loser_games = rating$games[matchDetails$IndexLoser]
  
  matchDetails$Location = game$Location
  matchDetails$Winner_country = rating$Country[matchDetails$IndexWinner]
  matchDetails$Loser_country = rating$Country[matchDetails$IndexLoser]
  
  citycountry = read.table("Data/datasets/citycountry.csv",  header = T, sep = ",", quote = "\"",
             colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  
  matchDetails$Country = citycountry$country[match(matchDetails$Location, citycountry$city)]
  
  
  return(matchDetails)
}

addUncertaintyAndGames = function(Games, i, matchDetails){
  Games$Winner_games[i] = matchDetails$Winner_games
  Games$Loser_games[i] = matchDetails$Loser_games
  
  if(Games$Winner_games[i] == 0 | Games$Loser_games[i] == 0) {
    Games$Uncertainty[i] = 2
  } else {
    Games$Uncertainty[i] = 1 / (Games$Winner_games[i] * Games$Loser_games[i])
  }
  
  if(Games$Winner_games[i] == 0 | Games$Loser_games[i] == 0) {
    Games$Uncertainty2[i] = 2
  } else {
    Games$Uncertainty2[i] = 1 / min(Games$Winner_games[i], Games$Loser_games[i])
  }
  
  return(Games)
}

addHomePlayers = function(Games, rating, i, matchDetails){
  Games$Country[i] = matchDetails$Country
  Games$Winner_country[i] = matchDetails$Winner_country
  Games$Loser_country[i] = matchDetails$Loser_country
  Games$WinnerisHome[i] = as.numeric(matchDetails$Winner_country == matchDetails$Country)
  Games$LoserisHome[i] = as.numeric(matchDetails$Loser_country == matchDetails$Country)
  
  if(is.na(Games$WinnerisHome[i])) {
    Games$WinnerisHome[i] = 0
  }
  
  if(is.na(Games$LoserisHome[i])) {
    Games$LoserisHome[i] = 0
  }
  
  return(Games)
}

addSkillsBoX = function(Games, rating, i, matchDetails){
  winnerBo5Skill = getBo5Skill(rating, matchDetails$IndexWinner)
  loserBo5Skill = getBo5Skill(rating, matchDetails$IndexLoser)
  
  winnerBo3Skill = -winnerBo5Skill
  loserBo3Skill = -loserBo5Skill
  
  Games$Winner_skillBo5[i] = winnerBo5Skill
  Games$Winner_skillBo3[i] = winnerBo3Skill
  Games$Loser_skillBo5[i] = loserBo5Skill
  Games$Loser_skillBo3[i] = loserBo3Skill
  
  Games$Winner_skillBo5PlusScores[i] = getBo5SkillBasedOnRating(rating, matchDetails$IndexWinner)
  Games$Winner_skillBo3PlusScores[i] =  - Games$Winner_skillBo5PlusScores[i]
  
  Games$Loser_skillBo5PlusScores[i] = getBo5SkillBasedOnRating(rating, matchDetails$IndexLoser)
  Games$Loser_skillBo3PlusScores[i] = -Games$Loser_skillBo5PlusScores[i]

    
  return(Games)
}

#if no games in bo5 or bo3 the skill will be 0
getBo5Skill = function(rating, indexPlayer){
  minGamesRequired = 5
  if(rating$Bo5Played[indexPlayer] < minGamesRequired | rating$Bo3Played[indexPlayer] < minGamesRequired){
    return(0)
  }
  
  percentageWinsBo5 = rating$Bo5Won[indexPlayer] / rating$Bo5Played[indexPlayer]
  percentageWinsBo3 = rating$Bo3Won[indexPlayer] / rating$Bo3Played[indexPlayer]
  Bo5Skill = percentageWinsBo5 - percentageWinsBo3
  
  return(Bo5Skill)
}

getBo5SkillBasedOnRating = function(rating, indexPlayer){
  minGamesRequired = 0
  if(rating$Bo5Played[indexPlayer] > minGamesRequired) {
    meanBo5PlusScore = rating$Bo5PlusScore[indexPlayer] / rating$Bo5Played[indexPlayer]
  } else {
    meanBo5PlusScore = 0
  }
  if(rating$Bo3Played[indexPlayer] > minGamesRequired) {
  meanBo3PlusScore = rating$Bo3PlusScore[indexPlayer] / rating$Bo3Played[indexPlayer]
  } else {
    meanBo3PlusScore = 0
  }
  Bo5Skill = meanBo5PlusScore - meanBo3PlusScore
  
  return(Bo5Skill)
}