#Creates the variable w_svpt_won and w_rtpt_won
#For the missing values there will be an approximation used
#By first looking at matches with the same set scores
#If these are missing scores which have the same total won games per player are used

library(dplyr)
library(stringr)
library(purrr)

rm(list = ls())
source("formulas.r")
source("constants.r")
source("approximateScoresMissingGamesFormulas.R")

##Code
allGames <- getAllGamesWithoutRating()
allGames <- allGames %>% mutate(Date = as.Date(Date, DF))

allGames <- allGames %>% mutate(pointsMissing  = is.na(w_svpt),
                                w_svpt_won     = w_1stWon + w_2ndWon,
                                w_rtpt_won     = l_svpt - l_1stWon - l_2ndWon,
                                w_svptpercent  = (w_1stWon + w_2ndWon) / w_svpt,
                                w_rtptpercent  = 1 - (l_1stWon + l_2ndWon) / allGames$l_svpt,
                                GamesWonWinner = sum(c(W1, W2, W3, W4, W5), na.rm = T),
                                GamesWonLoser  = sum(c(L1, L2, L3, L4, L5), na.rm = T),
                                orderedScore   = NA)

##Creates the orderedScore variable for the games
for(i in 1 : nrow(allGames)){

  totalScore <- data.frame(wins = c(allGames$W1[i], allGames$W2[i], allGames$W3[i], allGames$W4[i], allGames$W5[i]), 
                          lost = c(allGames$L1[i], allGames$L2[i], allGames$L3[i], allGames$L4[i], allGames$L5[i]))
  totalScore <- na.omit(totalScore)
  totalScore <- totalScore %>% arrange(desc(wins), desc(lost))
  
  stringOrderedScore <- ""
  for(j in 1:  nrow(totalScore)) {
    stringOrderedScore <- paste(stringOrderedScore, as.character(totalScore$wins[j]), "-", as.character(totalScore$lost[j])," ", sep = "")
  }
  allGames$orderedScore[i] = stringOrderedScore
}

##resultsDatabase is used for approximating the missing score values for the Barto rating system
resultsDatabase <- tibble(Score                        = NA,
                          GamesWonWinner               = NA,
                          GamesWonLoser                = NA,
                          Matches                      = NA,
                          PercentServePointsWonWinner  = NA,
                          ServePointsWinner            = NA,
                          PercentReturnPointsWonWinner = NA,
                          ReturnPointsWinner           = NA
                          )

##Add games up to train_model
resultsDatabase <- resultsDatabase %>% addMatchesToResultsDatabase(allGames %>% filter(Date <= fdTrain_Model))

indexesMissingPoints <- which(allGames$pointsMissing == 1)

##Approximate scores missing games up to validationData 
allGames %>% slice(indexesMissingPoints) %>% filter(Date <= fdVal) %>% select(w_svpt, w_svpt_won, l_svpt, w_rtpt_won) <-
  allGames %>% slice(indexesMissingPoints) %>% filter(Date <= fdVal) %>% select(orderedScore) %>%
  apply(1, approximateScoreMissingGame, resultsDatabase = resultsDatabase) 


allGames %>% filter(Date <= fdVal) <- 

##Filling the games in allGames for which the match scores are missing

for(i in indexesMissingPoints) {
  if((allGames$orderedScore[i] %in% resultsDatabase$Score)) {
    iDB <- match(allGames$orderedScore[i], resultsDatabase$Score)
    
    allGames$w_svpt[i]     <- resultsDatabase %>% slice(iDB) %>% select(ServePointsWinner) %>% round() %>% as.numeric()
    allGames$w_svpt_won[i] <- 
      resultsDatabase %>% slice(iDB) %>% mutate(w_1stIn = PercentServePointsWonWinner * ServePointsWinner) %>% 
      select(w_1stIn) %>% round() %>% as.numeric()
    
    allGames$l_svpt[i]     <- resultsDatabase %>% slice(iDB) %>% select(ReturnPointsWinner) %>% round() %>% as.numeric()
    allGames$w_rtpt_won[i] <- 
      resultsDatabase %>% slice(iDB) %>% mutate(w_1stIn = PercentReturnPointsWonWinner * ReturnPointsWinner) %>% 
      select(w_1stIn) %>% round() %>% as.numeric()
  } else {
    iDB <- which(resultsDatabase$GamesWonWinner == allGames$GamesWonWinner[i] & 
                resultsDatabase$GamesWonLoser == allGames$GamesWonLoser[i])
    if(!is_empty(iDB)) {
      allGames$w_svpt[i]     <- 
        resultsDatabase %>% slice(iDB) %>% 
        summarise(AvgServePointsWinner = sum(ServePointsWinner * Matches) / sum(Matches)) %>% 
        round() %>% as.numeric()  
      allGames$w_svpt_won[i] <- 
        resultsDatabase %>% slice(iDB) %>% 
        summarise(AvgPercentServePointsWinner = sum(PercentServePointsWonWinner * Matches) / sum(Matches) * allGames$w_svpt[i]) %>% 
        round() %>% as.numeric()
      
      allGames$l_svpt[i]     <- 
        resultsDatabase %>% slice(iDB) %>% summarise(AvgReturnPointsWinner = sum(ReturnPointsWinner * Matches) / sum(Matches)) %>% 
        round() %>% as.numeric()  
      allGames$w_rtpt_won[i] <- 
        resultsDatabase %>% slice(iDB) %>%  
        summarise(AvgPercentReturnPointsWinner = sum(PercentReturnPointsWonWinner * Matches) / sum(Matches) * allGames$l_svpt[i]) %>% 
        round() %>% as.numeric() 
    }
  }
} 

allGames <- allGames %>% select(-w_svptpercent, -w_rtptpercent) 
