library(dplyr)
library(readr)
library(lubridate)
source("constants.r")

Expectation <- function(diff) {
  1 - 1 / (1 + 10 ^ (diff / 400))
}


LogLoss = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps) 
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}


RemoveWalkOvers = function(Data){
  i <- which(Data$Comment %in% c("Walkover", "Walover"))
  Data = Data[-i, ]
}


#returns a vector containing the next game if available and 
#an index indicating whether it's the winner (1), loser (2)
#or no next game (0)
FindNextGame = function(idPlayer, idWinners, idLosers, nrPreviousGames){
  nextWin = match(idPlayer, idWinners)
  nextLoss = match(idPlayer, idLosers)
  
  nextGame = list()
  
  if(is.na(nextWin) & is.na(nextLoss)){
    nextGame$number = NA
    nextGame$player = 0
    return(nextGame)
  }
  
  if(is.na(nextWin) & !is.na(nextLoss)) {
    nextGame$number = nextLoss + nrPreviousGames
    nextGame$player = 2
    return(nextGame)
  } else if(!is.na(nextWin) & is.na(nextLoss)){
    nextGame$number = nextWin + nrPreviousGames
    nextGame$player = 1
    return(nextGame)
  }
  
  if(nextWin < nextLoss) {
    nextGame$number = nextWin + nrPreviousGames
    nextGame$player = 1
  } else if (nextLoss < nextWin){
    nextGame$number = nextLoss + nrPreviousGames
    nextGame$player = 2
  }
  return(nextGame)
}

FindDaysDiff = function(dates){
  date1 = as.Date(as.character(dates[1]), format = "%m/%d/%Y" )
  date2 = as.Date(as.character(dates[2]), format = "%m/%d/%Y" )
  
  abs(as.numeric(date2 - date1))
}
FindnextGameSamePlayers = function(winner, loser, winners, losers, previousMatches) {
  winner_innext_winner = previousMatches + which(winners %in% winner) 
  winner_innext_loser = previousMatches + which(losers %in% winner) 
  
  loser_innext_winner = previousMatches + which(winners %in% loser) 
  loser_innext_loser = previousMatches + which(losers %in% loser) 
  
  sameOrders = intersect(winner_innext_winner, loser_innext_loser)
  oppOrders = intersect(winner_innext_loser, loser_innext_winner)
  
  sameOrders = sameOrders[1]
  oppOrders = oppOrders[1]
  
  nextGame = list()
  
  if(!is.na(sameOrders) & !is.na(oppOrders)) {
    if(sameOrders < oppOrders) {
      nextGame$Number = sameOrders
      nextGame$Order = 1
    } else {
      nextGame$Number = oppOrders
      nextGame$Order = 2
    }
  } else if(!is.na(sameOrders)) {
    nextGame$Number = sameOrders
    nextGame$Order = 1
  } else if(!is.na(oppOrders)) {
    nextGame$Number = oppOrders
    nextGame$Order = 2
  } else {
    nextGame$Order = 0
  }  
  
  return(nextGame)
}

getAllGamesWithoutRating2 = function() {
  train_rating = read_csv("Data/datasets/train_rating.csv", col_names = T, quote = "\"")
  train_model = read_csv("Data/datasets/train_model.csv", col_names = T, quote = "\"")
  val = read_csv("Data/datasets/val.csv", col_names = T, quote = "\"")
  test = read_csv("Data/datasets/test.csv", col_names = T, quote = "\"")
  
  allGames = dplyr::bind_rows(train_rating, train_model, val, test)
  
  #everything is now character, however some columns need to be numeric
  # if ("idWinner" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, 
  #                      idWinner = as.numeric(idWinner),
  #                      idLoser = as.numeric(idLoser)
  #   ) 
  # }
  # 
  # if ("Result" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, Result = as.numeric(Result)) 
  # }
  # 
  # if ("HeadtoHead" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, 
  #                      HeadtoHead = as.numeric(HeadtoHead),
  #                      HeadtoHeadMatches = as.numeric(HeadtoHeadMatches),
  #                      LastHeadtoHead = as.numeric(LastHeadtoHead)
  #   ) 
  # }
  return(allGames)
}


getAllGamesWithoutRating = function() {
  train_rating = read.table("Data/datasets/train_rating.csv", header = T, sep = ",", quote = "\"",
                            stringsAsFactors = FALSE, fill = TRUE)
  train_model = read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                           stringsAsFactors = FALSE, fill = TRUE)
  val = read.table("Data/datasets/val.csv", header = T, sep = ",", quote = "\"", 
                  stringsAsFactors = FALSE, fill = TRUE)
  test = read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                    stringsAsFactors = FALSE, fill = TRUE)
  
  allGames = bind_rows(train_rating, train_model, val, test)
  
  #everything is now character, however some columns need to be numeric
  # if ("idWinner" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, 
  #                      idWinner = as.numeric(idWinner),
  #                      idLoser = as.numeric(idLoser)
  #   ) 
  # }
  # 
  # if ("Result" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, Result = as.numeric(Result)) 
  # }
  # 
  # if ("HeadtoHead" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, 
  #                      HeadtoHead = as.numeric(HeadtoHead),
  #                      HeadtoHeadMatches = as.numeric(HeadtoHeadMatches),
  #                      LastHeadtoHead = as.numeric(LastHeadtoHead)
  #   ) 
  # }
  return(allGames)
}


getAllGamesWithRating = function() {
  train_rating = read.table("Data/datasets/train_ratingWithRatings.csv", header = T, sep = ",", quote = "\"",
                            stringsAsFactors = TRUE, fill = TRUE)
  train_model = read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                           stringsAsFactors = TRUE, fill = TRUE)
  val = read.table("Data/datasets/valWithRatings.csv", header = T, sep = ",", quote = "\"", 
                  stringsAsFactors = TRUE, fill = TRUE)
  test = read.table("Data/datasets/testWithRatings.csv", header = T, sep = ",", quote = "\"",
                    stringsAsFactors = TRUE, fill = TRUE)
  
  allGames <- dplyr::bind_rows(train_rating, train_model, val, test)
  
  #everything is now character, however some columns need to be numeric
  # if ("idWinner" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, 
  #                      idWinner = as.numeric(idWinner),
  #                      idLoser = as.numeric(idLoser)
  #   ) 
  # }
  # 
  # if ("Result" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, Result = as.numeric(Result)) 
  # }
  # 
  # if ("HeadtoHead" %in% colnames(allGames)) {
  #   allGames <- mutate(allGames, 
  #                      HeadtoHead = as.numeric(HeadtoHead),
  #                      HeadtoHeadMatches = as.numeric(HeadtoHeadMatches),
  #                      LastHeadtoHead = as.numeric(LastHeadtoHead)
  #   ) 
  # }
  return(allGames)
}


getPlayers = function() {
  player <- read.table("Data/source_data_Sackmann/atp_players.csv", header = F, sep = ",", quote = "\"",
                       colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  names(player) <- c("id", "firstName", "secondName", "Handed", "birthDate", "Nationality")
  
  if ("id" %in% colnames(player)) {
    player <- mutate(player, id = as.numeric(id)) 
  }
  return(player)
}

getATPPlayers <- function() {
  atp_players <- read.table("Data/datasets/atp_players2.csv", header = T, sep = ",", quote = "\"", fill = TRUE)
  
  # remove the ID's which are double in this list
  # it is important that systems referencing this list by id (not name) also use this converted list
  # the function convert_ATP_player_ID does this
  
  #  "109613" = 104834,
  #  "103863" = 103862,
  #  "103806" = 103805,
  #  "109254" = 103473,
  #  "110685" = 104711,
  #  "109251" = 109250,
  #  "109365" = 108774,
  #  "104084" = 104083,
  #  "105314" = 105313,
  #  "103921" = 103920,
  #  "103651" = 103650,
  #  "104458" = 104457,
  #  "127301" = 110774
  list_ID_duplicated_players <- c(
    109613,
    103863,
    103806,
    109254,
    110685,
    109251,
    109365,
    104084,
    105314,
    103921,
    103651,
    104458,
    127301
  )
  
  atp_players <- filter(atp_players, !ID %in% list_ID_duplicated_players)
}


convert_ATP_player_ID <- function (ID) {
  id_converted <- switch (as.character(ID),
                          "109613" = 104834,
                          "103863" = 103862,
                          "103806" = 103805,
                          "109254" = 103473,
                          "110685" = 104711,
                          "109251" = 109250,
                          "109365" = 108774,
                          "104084" = 104083,
                          "105314" = 105313,
                          "103921" = 103920,
                          "103651" = 103650,
                          "104458" = 104457,
                          "127301" = 110774
  )
  
  if(is.null(id_converted)) { return (ID) } else { return (id_converted) }
}


saveDatasetsWithoutRating = function(allGames){
  df <- "%Y-%m-%d"
  allGames$Date <- as.Date(allGames$Date, df)
  
  train_rating <- allGames %>% filter(Date <= fdTrain_Rating)   
  train_model  <- allGames %>% filter(Date >  fdTrain_Rating, Date < fdTrain_Model)   
  val          <- allGames %>% filter(Date >  fdTrain_Model , Date < fdVal) 
  test         <- allGames %>% filter(Date >  fdVal         , Date < fdTest)
  
  write.csv(file = "Data/datasets/train_rating.csv", train_rating, row.names = FALSE)
  write.csv(file = "Data/datasets/train_model.csv", train_model, row.names = FALSE)
  write.csv(file = "Data/datasets/val.csv", val, row.names = FALSE)
  write.csv(file = "Data/datasets/test.csv", test, row.names = FALSE)
}


#Note that we assume here that the walkovers are removed
saveDatasetsWithRating = function(allGames, rating = NULL){
  
  df <- "%Y-%m-%d"
  allGames$Date <- as.Date(allGames$Date, df)
  
  train_rating <- allGames %>% filter(Date <= fdTrain_Rating)   
  train_model  <- allGames %>% filter(Date >  fdTrain_Rating, Date < fdTrain_Model)   
  val          <- allGames %>% filter(Date >  fdTrain_Model,  Date < fdVal) 
  test         <- allGames %>% filter(Date >  fdVal,          Date < fdTest)
  
  write.csv(file = "Data/datasets/train_ratingWithRatings.csv", train_rating, row.names=FALSE)
  write.csv(file = "Data/datasets/train_modelWithRatings.csv", train_model, row.names=FALSE)
  write.csv(file = "Data/datasets/valWithRatings.csv", val, row.names=FALSE)
  write.csv(file = "Data/datasets/testWithRatings.csv", test, row.names=FALSE)
  
  if(!is.null(rating)) {
    write.csv(file = "Data/datasets/ratingafterTest.csv", 
              rating, row.names=FALSE)
  }
}


savePlayers = function(player) {
  write.csv(file = "Data/datasets/players.csv", player, row.names=FALSE)
}

calculateGames <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  Games <- wonGames + lostGames
}

saveDatasets2 <- function(df, dir, filename, lvl = "", ext = ".csv") {
  if(!lvl == "") { lvl <- paste("_", lvl, sep = "") }
  write.table(file = paste(dir, filename, lvl, ext, sep = ""), df, row.names=FALSE)
}

saveDatasets <- function(df, dir, filename, lvl = "", ext = ".csv") {
   if(!lvl == "") { lvl <- paste("_", lvl, sep = "") }
   write.csv(file = paste(dir, filename, lvl, ext, sep = ""), df, row.names=FALSE)
}

getDatasets2 <- function(dir, filename, ext= ".csv", lvl = "", last = F, change_datatype=TRUE) {
  if (lvl == "") {
    dir_filename = paste(dir, filename, ext, sep = "")
  } else {
    dir_filename = paste(dir, filename, "_", lvl, ext, sep = "")
  }
  
  df <- read_csv(dir_filename, col_names = T)
  
  # if (change_datatype) {
  #   if ("StartDate" %in% colnames(df)) {
  #     df <- df %>% mutate(
  #       StartDate = ymd(StartDate)
  #     )
  #   }
  #   if ("Date" %in% colnames(df)) {
  #     df <- df %>% mutate(
  #       Date = ymd(Date)
  #     )
  #   }
  #   if ("Date.x" %in% colnames(df)) {
  #     df <- df %>% mutate(
  #       Date.x = ymd(Date.x)
  #     )
  #   }
  #   if ("Date.y" %in% colnames(df)) {
  #     df <- df %>% mutate(
  #       Date.y = ymd(Date.y)
  #     )
  #   }
  # }
  
  return(df)
} 

getDatasets <- function(dir, filename, ext=".csv", lvl = "", last = F, change_datatype = TRUE) {
  if (lvl == "") {
    dir_filename = paste(dir, filename, ext, sep = "")
  } 
  else {
    dir_filename = paste(dir, filename, "_", lvl, ext, sep = "")
  }
  
  # TODO if last == T, search the file list and get the last added file
  #Support for extra variables of read table
  
  df <- read.table(dir_filename, header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  
  
  #everything is now character, however some columns need to be numeric
  if (change_datatype) {
    if ("id" %in% colnames(df)) {
      df <- df %>% mutate(id = as.integer(id)) 
    }
    
    if ("PSW" %in% colnames(df)) {
      df <- df %>% mutate(PSW = as.numeric(PSW))
    }
    
    if ("PSL" %in% colnames(df)) {
      df <- df %>% mutate(PSL = as.numeric(PSL))
    }

    if ("StartDate" %in% colnames(df)) {
      df <- df %>% mutate(
                   StartDate = as.Date(StartDate, format="%Y-%m-%d")
      )
    }
    if ("Date" %in% colnames(df)) {
      df <- df %>% mutate(
                          Date = as.Date(Date, format="%Y-%m-%d")
      )
    }
    if ("Date.x" %in% colnames(df)) {
      df <- df %>% mutate(
                   Date.x = as.Date(Date.x, format="%Y-%m-%d")
      )
    }
    if ("Date.y" %in% colnames(df)) {
      df <- df %>% mutate(
                   Date.y = as.Date(Date.y, format="%Y-%m-%d")
      )
    }
    if ("idWinner" %in% colnames(df)) {
      df <- df %>% mutate( 
                         idWinner = as.integer(idWinner),
                         idLoser = as.integer(idLoser)
      ) 
    }
    if ("id_atp" %in% colnames(df)) {
      df <- df %>% mutate( 
                   id_atp = as.integer(id_atp)
      ) 
    }
    if ("id_Sackmann" %in% colnames(df)) {
      df <- df %>% mutate(
                   id_Sackmann = as.integer(id_Sackmann)
      ) 
    }
    if ("Result" %in% colnames(df)) {
      df <- df %>% mutate(Result = as.numeric(Result)) 
    }
    if ("HeadtoHead" %in% colnames(df)) {
      df <- df %>% mutate( 
                         HeadtoHead = as.numeric(HeadtoHead),
                         HeadtoHeadMatches = as.numeric(HeadtoHeadMatches),
                         LastHeadtoHead = as.numeric(LastHeadtoHead)
      )
    }
  }
  return (df)
}

calculateFractionGamesWinnerWon <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  percentWonGames <- wonGames / (wonGames + lostGames)
}

calculateFractionNetBreakGamesWinnerWon <- function(row) {
  wonGames  <- sum(as.numeric(c(row$W1, row$W2, row$W3, row$W4, row$W5)), na.rm = TRUE)
  lostGames <- sum(as.numeric(c(row$L1, row$L2, row$L3, row$L4, row$L5)), na.rm = TRUE)
  percentWonBreakGames <- 0.5 + (wonGames - lostGames) / (wonGames + lostGames)
}

changeGameWon <- function(pServer) {
  pS <- pServer
  pR <- 1 - pServer
  pS^4 * (1 + 4 * pR + 10 * pR^2) + 20 * (pS * pR) ^ 3 * pS ^ 2 / (1 - 2 * pS * pR) 
}
