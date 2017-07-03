rm(list = ls())
library(dplyr)
source("D:/Betting/Tennis/formulas.r")


setwd("D:/Betting/Tennis/Data/all_unaltered save guard")

data2000<-read.table("2000.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2001<-read.table("2001.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2002<-read.table("2002.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2003<-read.table("2003.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2004<-read.table("2004.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2005<-read.table("2005.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2006<-read.table("2006.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2007<-read.table("2007.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2008<-read.table("2008.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2009<-read.table("2009.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2010<-read.table("2010.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2011<-read.table("2011.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2012<-read.table("2012.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2013<-read.table("2013.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2014<-read.table("2014.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

train = dplyr::bind_rows(data2000, data2001, data2002, data2003, data2004, data2005, data2006, data2007, data2008, 
              data2009, data2010,data2011,data2012, data2013, data2014)

train = train[, c(7,10,11)]

data2015<-read.table("2015.csv", header = T, sep = ",", quote = "", stringsAsFactors = FALSE)
data2016<-read.table("2016.csv", header = T, sep = ",", quote = "", stringsAsFactors = FALSE)

test = rbind(data2015, data2016)
test = test[, c(7,10,11,12,13, 35, 36)]

#Making ranking row numerical and missing values equal to 9999
test[, 4] = as.numeric(test[, 4])
test[, 5] = as.numeric(test[, 5])
for(i in 1: nrow(test)){
  if(is.na(test[i, 4])) {
    test[i, 4] = 9999
  }
  if(is.na(test[i, 5])) {
    test[i, 5] = 9999
  }
}

rm(data2000, data2001, data2002, data2003, data2004, data2005, data2006, data2007, data2008, data2009,
   data2010, data2011, data2012, data2013, data2014, data2015, data2016)

allWinners = as.data.frame(rbind(train[2], test[2]))
allLosers = as.data.frame(rbind(train[3], test[3]))

rating = InitializeRating(allWinners$Winner, allLosers$Loser) 

rm(allWinners, allLosers) 
for (i in 1:length(train[, 1])) {
  winner = train[i, 2]
  loser = train[i, 3]
  surface = train[i, 1]
  
  rating = UpdateRating(rating, winner, loser, surface)
}

sumlogloss = 0 
sumlogloss20GamesPlus = 0
N20GamesPlus = 0

correctlypredicted = 0 
WPScorrectlypredicted = 0 
NumberOfStatisticsCalculated = 0 


Ntest = nrow(test)
#at least one player top 30
Ntopmatches = 0
sumtoplogloss = 0
correctlyTopPredicted = 0

predictedvalues = as.data.frame(matrix(nrow = Ntest, ncol = 0))

bets = as.data.frame(matrix(nrow = Ntest, ncol = 0))
bets$result = rep(0, Ntest)
bets$bet = rep(0, Ntest)

for(i in 1: Ntest) {
  winner = test[i, 2]
  loser = test[i, 3]
  surface = train[i, 1]
  
  winnerIndex = match(winner, rating$Players)
  loserIndex = match(loser, rating$Players)
  
  winnerGames = rating$games[winnerIndex]
  loserGames = rating$games[loserIndex]
  
  expectationWinner = CalculateExpectation(rating, surface, winner, loser)
  
  predictedvalues$pred[i] = expectationWinner
  predictedvalues$y[i] = 1
  
  expectationLoser = 1 - expectationWinner
  
  if(winnerGames > 25 & loserGames > 25) {
    if(!is.na(test$PSW[i] & is.na(test$PSL[i]))) {
      rake = 1 / test$PSW[i] + 1 / test$PSL[i]
      if(rake < 1.04) {
        if((expectationWinner * test$PSW[i] - 1) > 0.15) {
          bets$bet[i] = (expectationWinner * (test$PSW[i] + 1) - 1) / test$PSW[i]
          bets$result[i] = bets$bet[i]*(test$PSW[i] - 1)
        } else if((expectationLoser * test$PSL[i] - 1) > 0.15) {
          bets$bet[i] = (expectationLoser * (test$PSL[i] + 1) - 1) / test$PSL[i]
          bets$result[i] = -bets$bet[i]
        }
      }
    }
  }
  
  if(test[i, 4] < 51 & test[i, 5] < 51 & test[i, 4] > -1 & test[i, 5] > -1) {
    Ntopmatches = Ntopmatches + 1
    sumtoplogloss = sumtoplogloss + log(expectationWinner)
    if(expectationWinner > 0.5) {
      correctlyTopPredicted = correctlyTopPredicted + 1
    } 
  }
  
  if(rating$games[winnerIndex] > 20 & rating$games[loserIndex] > 20) {
    sumlogloss20GamesPlus = sumlogloss20GamesPlus + log(expectationWinner)
    N20GamesPlus = N20GamesPlus + 1
  }
    
  #sumlogloss
  sumlogloss = sumlogloss + log(expectationWinner)
  
  #accuracy
  if(expectationWinner > 0.5) {
    correctlypredicted = correctlypredicted + 1
  } 
  
  if(test[i,4] < test[i, 5]) {
    WPScorrectlypredicted = WPScorrectlypredicted + 1
  }
  
  #update rating
  rating = UpdateRating(rating, winner, loser, surface)
}

logloss = -sumlogloss / Ntest
#Check Log Loss
LogLoss(predictedvalues$pred, predictedvalues$y)
logLoss20gamesPlus = -sumlogloss20GamesPlus / N20GamesPlus
toplogloss = -sumtoplogloss / Ntopmatches
accuracy = correctlypredicted / Ntest
topaccuracy = correctlyTopPredicted / Ntopmatches
wpsaccuracy = WPScorrectlypredicted / Ntest

print(logloss)
print(logLoss20gamesPlus)
print(toplogloss)
print(accuracy)
print(topaccuracy)
print(wpsaccuracy)


