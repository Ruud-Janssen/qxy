addMatchesToResultsDatabase <- function(resultsDatabase, games) {
  indexesHavingPoints <- which(games$pointsMissing == 0)
  
  ##Fills the resultsDatabase by taking the average score
  for(i in indexesHavingPoints) {
    index <- match(games$orderedScore[i], resultsDatabase$Score)
    
    if(!is.na(index)) {
      resultsDatabase$PercentServePointsWonWinner[index] <- 
        (resultsDatabase$PercentServePointsWonWinner[index] * resultsDatabase$Matches[index] + 
           games$w_svptpercent[i]) / (resultsDatabase$Matches[index] + 1)
      resultsDatabase$ServePointsWinner[index] <- 
        (resultsDatabase$ServePointsWinner[index] * resultsDatabase$Matches[index] + 
           games$w_svpt[i]) / (resultsDatabase$Matches[index] + 1)
      
      resultsDatabase$PercentReturnPointsWonWinner[index] <- 
        (resultsDatabase$PercentReturnPointsWonWinner[index] * resultsDatabase$Matches[index] + 
           games$w_rtptpercent[i]) / (resultsDatabase$Matches[index] + 1)
      resultsDatabase$ReturnPointsWinner[index] <- 
        (resultsDatabase$ReturnPointsWinner[index] * resultsDatabase$Matches[index] + 
           games$l_svpt[i]) / (resultsDatabase$Matches[index] + 1)
      resultsDatabase$Matches[index] <- resultsDatabase$Matches[index] + 1
    } else {
      Score                        <- games$orderedScore[i]
      GamesWonWinner               <- games$GamesWonWinner[i]
      GamesWonLoser                <- games$GamesWonLoser[i]
      Matches                      <- 1
      PercentServePointsWonWinner  <- games$w_svptpercent[i]
      ServePointsWinner            <- games$w_svpt[i]
      PercentReturnPointsWonWinner <- games$w_rtptpercent[i]
      ReturnPointsWinner           <- games$l_svpt[i]
      
      resultsDatabase              <- resultsDatabase %>% add_row(Score, GamesWonWinner, GamesWonLoser, Matches, 
                                                                  PercentServePointsWonWinner, ServePointsWinner, 
                                                                  PercentReturnPointsWonWinner, ReturnPointsWinner)
    }
  }
  
  resultsDatabase %>% arrange(desc(Matches))
}


approximateScoreMissingGame <- function(orderedScore, resultsDatabase) {
  if(orderedScore %in% resultsDatabase$Score) {
    iDB <- match(orderedScore, resultsDatabase$Score)
    
    w_svpt     <- resultsDatabase %>% slice(iDB) %>% select(ServePointsWinner) %>% round() %>% as.numeric()
    w_svpt_won <- 
      resultsDatabase %>% slice(iDB) %>% mutate(w_1stIn = PercentServePointsWonWinner * ServePointsWinner) %>% 
      select(w_1stIn) %>% round() %>% as.numeric()
    
    l_svpt     <- resultsDatabase %>% slice(iDB) %>% select(ReturnPointsWinner) %>% round() %>% as.numeric()
    w_rtpt_won <- 
      resultsDatabase %>% slice(iDB) %>% mutate(w_1stIn = PercentReturnPointsWonWinner * ReturnPointsWinner) %>% 
      select(w_1stIn) %>% round() %>% as.numeric()
  } else {
    iDB <- which(resultsDatabase$GamesWonWinner == allGames$GamesWonWinner[i] & 
                   resultsDatabase$GamesWonLoser == allGames$GamesWonLoser[i])
    if(!is_empty(iDB)) {
      w_svpt     <- 
        resultsDatabase %>% slice(iDB) %>% 
        summarise(AvgServePointsWinner = sum(ServePointsWinner * Matches) / sum(Matches)) %>% 
        round() %>% as.numeric()  
      w_svpt_won <- 
        resultsDatabase %>% slice(iDB) %>% 
        summarise(AvgPercentServePointsWinner = sum(PercentServePointsWonWinner * Matches) / sum(Matches) * allGames$w_svpt[i]) %>% 
        round() %>% as.numeric()
      
      l_svpt     <- 
        resultsDatabase %>% slice(iDB) %>% summarise(AvgReturnPointsWinner = sum(ReturnPointsWinner * Matches) / sum(Matches)) %>% 
        round() %>% as.numeric()  
      w_rtpt_won <- 
        resultsDatabase %>% slice(iDB) %>%  
        summarise(AvgPercentReturnPointsWinner = sum(PercentReturnPointsWonWinner * Matches) / sum(Matches) * allGames$l_svpt[i]) %>% 
        round() %>% as.numeric() 
    } else {
      w_svpt     <- NA
      w_svpt_won <- NA
      l_svpt     <- NA
      w_rtpt_won <- NA
    }
  }
  return(c(w_svpt = w_svpt, w_svpt_won = w_svpt_won, l_svpt = l_svpt, w_rtpt_won = w_rtpt_won))
}