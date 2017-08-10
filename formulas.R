library(dplyr)


Expectation <- function(diff) {
  1 - 1 / (1 + 10 ^ (diff / 400))
}


LogLoss = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps) 
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}


RemoveWalkOvers = function(Data){
  Data = Data[Data$Comment != "Walkover", ]
  Data = Data[Data$Comment != "Walover", ]
}


#returns a vector containing the next game if available and 
#an index indicating whether it's the winner (1), loser (2)
#or no next game (0)
FindNextGame = function(name, winners, losers, nrPreviousGames){
  nextWin = match(name, winners)
  nextLoss = match(name, losers)
  
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
  
  abs(as.numeric(date2-date1))
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




getAllGamesWithoutRating = function() {
  train_rating = read.table("Data/datasets/train_rating.csv", header = T, sep = ",", quote = "\"",
                            colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  train_model = read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                           colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  cv = read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                  colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  test = read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                    colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
  
  allGames = dplyr::bind_rows(train_rating, train_model, cv, test)
  
  #everything is now character, however some columns need to be numeric
  if ("idWinner" %in% colnames(allGames)) {
    allGames <- mutate(allGames, 
                       idWinner = as.numeric(idWinner),
                       idLoser = as.numeric(idLoser)
    ) 
  }
  
  if ("Result" %in% colnames(allGames)) {
    allGames <- mutate(allGames, Result = as.numeric(Result)) 
  }
  
  if ("HeadtoHead" %in% colnames(allGames)) {
    allGames <- mutate(allGames, 
                       HeadtoHead = as.numeric(HeadtoHead),
                       HeadtoHeadMatches = as.numeric(HeadtoHeadMatches),
                       LastHeadtoHead = as.numeric(LastHeadtoHead)
    ) 
  }
  return(allGames)
}


getAllGamesWithRating = function() {
  train_rating = read.table("Data/datasets/train_ratingWithRatings.csv", header = T, sep = ",", quote = "\"",
                            colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  train_model = read.table("Data/datasets/train_modelWithRatings.csv", header = T, sep = ",", quote = "\"",
                           colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  cv = read.table("Data/datasets/cvWithRatings.csv", header = T, sep = ",", quote = "\"", 
                  colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  test = read.table("Data/datasets/testWithRatings.csv", header = T, sep = ",", quote = "\"",
                    colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  
  allGames <- dplyr::bind_rows(train_rating, train_model, cv, test)
  
  #everything is now character, however some columns need to be numeric
  if ("idWinner" %in% colnames(allGames)) {
    allGames <- mutate(allGames, 
                       idWinner = as.numeric(idWinner),
                       idLoser = as.numeric(idLoser)
    ) 
  }
  
  if ("Result" %in% colnames(allGames)) {
    allGames <- mutate(allGames, Result = as.numeric(Result)) 
  }
  
  if ("HeadtoHead" %in% colnames(allGames)) {
    allGames <- mutate(allGames, 
                       HeadtoHead = as.numeric(HeadtoHead),
                       HeadtoHeadMatches = as.numeric(HeadtoHeadMatches),
                       LastHeadtoHead = as.numeric(LastHeadtoHead)
    ) 
  }
  return(allGames)
}


getPlayers = function() {
  player <- read.table("Data/datasets/players.csv", header = T, sep = ",", quote = "\"",
                       colClasses = "character", stringsAsFactors = TRUE, fill = TRUE)
  
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
  
  Nt_r = nrow(read.table("Data/datasets/train_rating.csv",  header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE))
  Nt_m = nrow(read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE))
  Ncv = nrow(read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                        colClasses = "character", stringsAsFactors = FALSE, fill = TRUE))
  Ntest = nrow(read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                          colClasses = "character", stringsAsFactors = FALSE, fill = TRUE))
  
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
  
  write.csv(file = "Data/datasets/train_rating.csv", train_rating, row.names=FALSE)
  write.csv(file = "Data/datasets/train_model.csv", train_model, row.names=FALSE)
  write.csv(file = "Data/datasets/cv.csv", cv, row.names=FALSE)
  write.csv(file = "Data/datasets/test.csv", test, row.names=FALSE)
}


#Note that we assume here that the walkovers are removed
saveDatasetsWithRating = function(allGames, rating = NULL){
  
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



saveDatasets <- function(df, dir, filename, lvl = "", ext = ".csv") {
   if(!lvl == "") { lvl <- paste("_", lvl, sep = "") }
   write.csv(file = paste(dir, filename, lvl, ext, sep = ""), df, row.names=FALSE)
}

getDatasets <- function(dir, filename, ext=".csv", lvl = "", last = F, change_datatype=TRUE) {
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
      df <- mutate(df, id = as.integer(id)) 
    }

    if ("StartDate" %in% colnames(df)) {
      df <- mutate(df,
                   StartDate = as.Date(StartDate, format="%Y-%m-%d")
      )
    }
    if ("Date" %in% colnames(df)) {
      df <- mutate(df,
                          Date = as.Date(Date, format="%Y-%m-%d")
      )
    }
    if ("Date.x" %in% colnames(df)) {
      df <- mutate(df,
                   Date.x = as.Date(Date.x, format="%Y-%m-%d")
      )
    }
    if ("Date.y" %in% colnames(df)) {
      df <- mutate(df,
                   Date.y = as.Date(Date.y, format="%Y-%m-%d")
      )
    }
    if ("idWinner" %in% colnames(df)) {
      df <- mutate(df, 
                         idWinner = as.integer(idWinner),
                         idLoser = as.integer(idLoser)
      ) 
    }
    if ("id_atp" %in% colnames(df)) {
      df <- mutate(df, 
                   id_atp = as.integer(id_atp)
      ) 
    }
    if ("id_Sackmann" %in% colnames(df)) {
      df <- mutate(df, 
                   id_Sackmann = as.integer(id_Sackmann)
      ) 
    }
    if ("Result" %in% colnames(df)) {
      df <- mutate(df, Result = as.numeric(Result)) 
    }
    if ("HeadtoHead" %in% colnames(df)) {
      df <- mutate(df, 
                         HeadtoHead = as.numeric(HeadtoHead),
                         HeadtoHeadMatches = as.numeric(HeadtoHeadMatches),
                         LastHeadtoHead = as.numeric(LastHeadtoHead)
      )
    }
  }
  return (df)
}

#Note that we assume here that the walkovers are removed
saveDatasetsWithRating = function(allGames, rating = NULL){
  
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
  
  if(!is.null(rating)) {
    write.csv(file = "Data/datasets/ratingafterTest.csv", 
              rating, row.names=FALSE)
  }
}

