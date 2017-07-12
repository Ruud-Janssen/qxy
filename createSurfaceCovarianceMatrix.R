source("addratingsformulas.R")
source("formulas.R")

initializeGamesAndGamesWon = function(Players) {
  numberOfPlayers = nrow(Players)
  
  Players$HardPlayed = rep(0, numberOfPlayers)
  Players$HardWon = rep(0, numberOfPlayers)
  
  Players$GrassPlayed = rep(0, numberOfPlayers)
  Players$GrassWon = rep(0, numberOfPlayers)
  
  Players$ClayPlayed = rep(0, numberOfPlayers)
  Players$ClayWon = rep(0, numberOfPlayers)
  
  Players$Bo3Played = rep(0, numberOfPlayers)
  Players$Bo3Won = rep(0, numberOfPlayers)
  
  Players$Bo5Played = rep(0, numberOfPlayers)
  Players$Bo5Won = rep(0, numberOfPlayers)

  return(Players)
}
getWinnerLoserSurface = function(game, Players) {
  matchDetails = list()
  matchDetails$Winner = game$Winner
  matchDetails$Loser = game$Loser
  matchDetails$Surface = game$Surface
  matchDetails$Best.of = game$Best.of
  
  matchDetails$IndexWinner = match(matchDetails$Winner, Players$Players)
  matchDetails$IndexLoser = match(matchDetails$Loser, Players$Players)
  return(matchDetails)
}
updateGameResults = function(Players, matchDetails) {
  if (matchDetails$Surface == "Hard") {
    Players$HardPlayed = addAGame(Players$HardPlayed, matchDetails$IndexWinner, matchDetails$IndexLoser)
    Players$HardWon[matchDetails$IndexWinner] = Players$HardWon[matchDetails$IndexWinner] + 1
  } else if (matchDetails$Surface == "Grass") {
    Players$GrassPlayed = addAGame(Players$GrassPlayed, matchDetails$IndexWinner, matchDetails$IndexLoser)
    Players$GrassWon[matchDetails$IndexWinner] = Players$GrassWon[matchDetails$IndexWinner] + 1
  } else if (matchDetails$Surface == "Clay") {
    Players$ClayPlayed = addAGame(Players$ClayPlayed, matchDetails$IndexWinner, matchDetails$IndexLoser)
    Players$ClayWon[matchDetails$IndexWinner] = Players$ClayWon[matchDetails$IndexWinner] + 1
  }
  
  if (matchDetails$Best.of == 3) {
    Players$Bo3Played = addAGame(Players$Bo3Played, matchDetails$IndexWinner, matchDetails$IndexLoser)
    Players$Bo3Won[matchDetails$IndexWinner] = Players$Bo3Won[matchDetails$IndexWinner] + 1
  } else {
    Players$Bo5Played = addAGame(Players$Bo5Played, matchDetails$IndexWinner, matchDetails$IndexLoser)
    Players$Bo5Won[matchDetails$IndexWinner] = Players$Bo5Won[matchDetails$IndexWinner] + 1
  }
  return(Players)
}

train_model = read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

Players = SetUniquePlayers(train_model$Winner, train_model$Loser)
Players = initializeGamesAndGamesWon(Players)

for (i in 1:nrow(train_model)) {
  matchDetails = getWinnerLoserSurface(train_model[i, ], Players)
  Players = updateGameResults(Players, matchDetails)
}

Players = Players[Players$HardPlayed != 0, ]
Players = Players[Players$GrassPlayed != 0, ]
Players = Players[Players$ClayPlayed !=0, ]

PercentagesWon = list()
PercentagesWon$Hard  = Players$HardWon / Players$HardPlayed
PercentagesWon$Grass = Players$GrassWon / Players$GrassPlayed
PercentagesWon$Clay = Players$ClayWon / Players$ClayPlayed

corMatrix = cor(cbind(PercentagesWon$Hard, PercentagesWon$Grass, PercentagesWon$Clay))


