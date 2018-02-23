#Creates the variable w_svpt_won and w_rtpt_won
#For the missing values there will be an approximation used
#By first looking at matches with the same set scores
#If these are missing scores which have the same total won games per player are used

#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)
library(purrr)

rm(list = ls())
source("formulas.r")
source("constants.r")

train_rating <- read.table("Data/datasets/train_ratingWithRatings.csv"
                          , header = T, sep = ",", quote = "\"", fill = TRUE)
train_model <- read.table("Data/datasets/train_modelWithRatings.csv"
                         , header = T, sep = ",", quote = "\"", fill = TRUE)

allGames <- rbind(train_rating, train_model)

allGames <- allGames %>% mutate(pointsMissing  = is.na(w_svpt),
                                w_svpt_won     = w_1stWon + w_2ndWon,
                                w_rtpt_won     = l_svpt - l_1stWon - l_2ndWon,
                                w_svptpercent  = (w_1stWon + w_2ndWon) / w_svpt,
                                w_rtptpercent  = 1 - (l_1stWon + l_2ndWon) / allGames$l_svpt,
                                GamesWonWinner = sum(c(W1, W2, W3, W4, W5), na.rm = T),
                                GamesWonLoser  = sum(c(L1, L2, L3, L4, L5), na.rm = T),
                                orderedScore   = NA)


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

resultsDataBase <-     tibble(Score                        = NA,
                              GamesWonWinner               = NA,
                              GamesWonLoser                = NA,
                              Matches                      = NA,
                              PercentServePointsWonWinner  = NA,
                              ServePointsWinner            = NA,
                              PercentReturnPointsWonWinner = NA,
                              ReturnPointsWinner           = NA
                              )

indexesHavingPoints <- which(allGames$pointsMissing == 0)

for(i in indexesHavingPoints) {
  index <- match(allGames$orderedScore[i], resultsDataBase$Score)

  if(!is.na(index)) {
    resultsDataBase$PercentServePointsWonWinner[index] <- 
      (resultsDataBase$PercentServePointsWonWinner[index] * resultsDataBase$Matches[index] + 
         allGames$w_svptpercent[i]) / (resultsDataBase$Matches[index] + 1)
    resultsDataBase$ServePointsWinner[index] <- 
      (resultsDataBase$ServePointsWinner[index] * resultsDataBase$Matches[index] + 
         allGames$w_svpt[i]) / (resultsDataBase$Matches[index] + 1)
    
    resultsDataBase$PercentReturnPointsWonWinner[index] <- 
      (resultsDataBase$PercentReturnPointsWonWinner[index] * resultsDataBase$Matches[index] + 
         allGames$w_rtptpercent[i]) / (resultsDataBase$Matches[index] + 1)
    resultsDataBase$ReturnPointsWinner[index] <- 
      (resultsDataBase$ReturnPointsWinner[index] * resultsDataBase$Matches[index] + 
         allGames$l_svpt[i]) / (resultsDataBase$Matches[index] + 1)
    resultsDataBase$Matches[index] <- resultsDataBase$Matches[index] + 1
  } else {
    Score                        <- allGames$orderedScore[i]
    GamesWonWinner               <- allGames$GamesWonWinner[i]
    GamesWonLoser                <- allGames$GamesWonLoser[i]
    Matches                      <- 1
    PercentServePointsWonWinner  <- allGames$w_svptpercent[i]
    ServePointsWinner            <- allGames$w_svpt[i]
    PercentReturnPointsWonWinner <- allGames$w_rtptpercent[i]
    ReturnPointsWinner           <- allGames$l_svpt[i]

    resultsDataBase              <- resultsDataBase %>% add_row(Score, GamesWonWinner, GamesWonLoser, Matches, 
                                                                PercentServePointsWonWinner, ServePointsWinner, 
                                                                PercentReturnPointsWonWinner, ReturnPointsWinner)
  }
}

resultsDataBase <- resultsDataBase %>% arrange(desc(Matches))

indexesMissingPoints <- which(allGames$pointsMissing == 1)

for(i in indexesMissingPoints) {
  if((allGames$orderedScore[i] %in% resultsDataBase$Score)) {
    iDB <- match(allGames$orderedScore[i], resultsDataBase$Score)
    
    allGames$w_svpt[i]     <- resultsDataBase %>% slice(iDB) %>% select(ServePointsWinner) %>% round() %>% as.numeric()
    allGames$w_svpt_won[i] <- 
      resultsDataBase %>% slice(iDB) %>% mutate(w_1stIn = PercentServePointsWonWinner * ServePointsWinner) %>% 
      select(w_1stIn) %>% round() %>% as.numeric()
    
    allGames$l_svpt[i]     <- resultsDataBase %>% slice(iDB) %>% select(ReturnPointsWinner) %>% round() %>% as.numeric()
    allGames$w_rtpt_won[i] <- 
      resultsDataBase %>% slice(iDB) %>% mutate(w_1stIn = PercentReturnPointsWonWinner * ReturnPointsWinner) %>% 
      select(w_1stIn) %>% round() %>% as.numeric()
  } else {
    iDB <- which(resultsDataBase$GamesWonWinner == allGames$GamesWonWinner[i] & 
                resultsDataBase$GamesWonLoser == allGames$GamesWonLoser[i])
    if(!is_empty(iDB)) {
      allGames$w_svpt[i]     <- 
        resultsDataBase %>% slice(iDB) %>% 
        summarise(AvgServePointsWinner = sum(ServePointsWinner * Matches) / sum(Matches)) %>% 
        round() %>% as.numeric()  
      allGames$w_svpt_won[i] <- 
        resultsDataBase %>% slice(iDB) %>% 
        summarise(AvgPercentServePointsWinner = sum(PercentServePointsWonWinner * Matches) / sum(Matches) * allGames$w_svpt[i]) %>% 
        round() %>% as.numeric()
      
      allGames$l_svpt[i]     <- 
        resultsDataBase %>% slice(iDB) %>% summarise(AvgReturnPointsWinner = sum(ReturnPointsWinner * Matches) / sum(Matches)) %>% 
        round() %>% as.numeric()  
      allGames$w_rtpt_won[i] <- 
        resultsDataBase %>% slice(iDB) %>%  
        summarise(AvgPercentReturnPointsWinner = sum(PercentReturnPointsWonWinner * Matches) / sum(Matches) * allGames$l_svpt[i]) %>% 
        round() %>% as.numeric() 
    }
  }
} 

allGames <- allGames %>% select(-w_svptpercent, -w_rtptpercent) 
