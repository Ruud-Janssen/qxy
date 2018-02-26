#Creates the variable w_svpt_won and w_rtpt_won
#For the missing values there will be an approximation used
#By first looking at matches with the same set scores
#If these are missing scores which have the same total won games per player are used

library(dplyr)
library(stringr)

rm(list = ls())
source("formulas.r")
source("constants.r")
source("approximateScoresMissingGamesFormulas.R")

##Code
allGames <- getAllGamesWithoutRating()
allGames <- allGames %>% mutate(Date = as.Date(Date, DF))

allGames <- allGames %>% mutate(pointsMissing  = is.na(w_svpt) | w_svpt == 0 | l_svpt == 0,
                                w_svpt_won     = w_1stWon + w_2ndWon,
                                w_rtpt_won     = l_svpt - l_1stWon - l_2ndWon,
                                w_svptpercent  = w_svpt_won / w_svpt,
                                w_rtptpercent  = 1 - w_rtpt_won / allGames$l_svpt,
                                GamesWonWinner = NA,
                                GamesWonLoser  = NA,
                                orderedScore   = NA)

message(paste("there are in total", count(allGames), "matches in the dataset \n", 
              "with", sum(allGames$pointsMissing) ,
              "games have point missing data and", count(allGames) - sum(allGames$pointsMissing),
              "have point data, the goal of this file is to fill as many of the missing points data \n",
              "as possible by looking for similar games who do have point data"))

##Creates the orderedScore, GamesWonWinner and GamesWonLoser variables for the games
for(i in 1 : nrow(allGames)){

  totalScore <- data.frame(wins = c(allGames$W1[i], allGames$W2[i], allGames$W3[i], allGames$W4[i], allGames$W5[i]), 
                          lost = c(allGames$L1[i], allGames$L2[i], allGames$L3[i], allGames$L4[i], allGames$L5[i]))
  totalScore <- na.omit(totalScore)
  totalScore <- totalScore %>% arrange(desc(wins), desc(lost))
  
  stringOrderedScore <- ""
  for(j in 1:  nrow(totalScore)) {
    stringOrderedScore <- paste(stringOrderedScore, as.character(totalScore$wins[j]), "-", as.character(totalScore$lost[j])," ", sep = "")
  }
  allGames$orderedScore[i]   <- stringOrderedScore
  allGames$GamesWonWinner[i] <- sum(totalScore$wins, na.rm = T)
  allGames$GamesWonLoser[i]  <- sum(totalScore$lost, na.rm = T)
}

##resultsDatabase is used for approximating the missing score values for the Barto rating system
resultsDatabase <- tibble(Score                        = character(),
                          GamesWonWinner               = integer(),
                          GamesWonLoser                = integer(),
                          Matches                      = integer(),
                          PercentServePointsWonWinner  = double(),
                          ServePointsWinner            = double(),
                          PercentReturnPointsWonWinner = double(),
                          ReturnPointsWinner           = double()
                          )

##Add games up to train_model
resultsDatabaseUpToTrain_Model <- resultsDatabase %>% addMatchesToResultsDatabase(allGames %>% filter(Date <= fdTrain_Model))
##Add games from validation set
resultsDatabaseUpToVal <- resultsDatabaseUpToTrain_Model %>% addMatchesToResultsDatabase(allGames %>% filter(Date > fdTrain_Model, Date <= fdVal))

##Approximate scores missing games up to validationData 
indexesMissingPointsUpToVal <- which(allGames$pointsMissing == 1 & allGames$Date <= fdVal)
allGames[indexesMissingPointsUpToVal, c("w_svpt", "w_svpt_won", "l_svpt" , "w_rtpt_won", "pointsMissing")] <-
  allGames %>% slice(indexesMissingPointsUpToVal) %>% 
  select(orderedScore, GamesWonWinner, GamesWonLoser, w_svpt, w_svpt_won, l_svpt, w_rtpt_won, pointsMissing) %>%
  approximateScoreMissingGame(resultsDatabase = resultsDatabaseUpToTrain_Model) 

##Approximate scores missing games for testData
indexesMissingPointsTest <- which(allGames$pointsMissing == 1 & allGames$Date > fdVal)
allGames[indexesMissingPointsTest, c("w_svpt", "w_svpt_won", "l_svpt" , "w_rtpt_won", "pointsMissing")] <-
  allGames %>% slice(indexesMissingPointsTest) %>% 
  select(orderedScore, GamesWonWinner, GamesWonLoser, w_svpt, w_svpt_won, l_svpt, w_rtpt_won, pointsMissing) %>%
  approximateScoreMissingGame(resultsDatabase = resultsDatabaseUpToVal) 

message(paste("After imputing missing points there are in total", count(allGames), "matches in the dataset \n", 
              "with", sum(allGames$pointsMissing) ,
              "games have point missing data and", count(allGames) - sum(allGames$pointsMissing),
              "have point data"))


allGames <- allGames %>% select(-w_svptpercent, -w_rtptpercent)

saveDatasetsWithoutRating(allGames)
