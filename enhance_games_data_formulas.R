parse_score_column_Sackmann <- function(allMatches) {

  require(tidyr)
  # copy for testing purposes
  #allMatches$score2 <- allMatches$score
  
  # remove tie break points between "( )"
  allMatches <- mutate(allMatches, score = gsub( "*\\(.*?\\) *", "", score))
  
  
  #vars <- c("W1","L1","W2","L2","W3","L3","W4","L4","W5","L5", "rest", "rest1", "rest2")
  vars <- c("W1","L1","W2","L2","W3","L3","W4","L4","W5","L5")
  
  #split vars in variables
  allMatches <- allMatches %>% 
    separate(score, into = vars, sep = "[- ]+", extra = "drop", fill = "right")
  
  # test results
  # distinct(scoreTest, rest2)
  # distinct(scoreTest, rest1)
  # distinct(scoreTest, rest)
  # distinct(allMatches, W1)
  # distinct(allMatches, L1)
  # distinct(allMatches, W2)
  # distinct(allMatches, L2)
  # distinct(allMatches, W3)
  # distinct(allMatches, W4)
  # distinct(allMatches, W5)
  # distinct(allMatches, W1)
  # distinct(scoreTest, as.numeric(W1))
  # distinct(allGames, W5)
  # distinct(allGames, as.numeric(W5))
  
  # remove text like RET, DEF, Walkover... and turn it into NA 
  allMatches <- allMatches %>% 
    mutate(
      W1 = as.numeric(W1), L1 = as.numeric(L1),
      W2 = as.numeric(W2), L2 = as.numeric(L2),
      W3 = as.numeric(W3), L3 = as.numeric(L3),
      W4 = as.numeric(W4), L4 = as.numeric(L4),
      W5 = as.numeric(W5), L5 = as.numeric(L5)    
    )
  
  #Wsets
  #Lsets
  allMatches <- allMatches %>% 
    rowwise() %>% mutate(
      W1set = getSets(W1, L1, W2, L2, W3, L3, W4, L4, W5, L5)[1],
      L1set = getSets(W1, L1, W2, L2, W3, L3, W4, L4, W5, L5)[2]
    ) 
  
  allMatches <- allMatches %>% 
    mutate(
      W1 = as.character(W1),
      L1 = as.character(L1),
      W2 = as.character(W2),
      L2 = as.character(L2),
      W3 = as.character(W3),
      L3 = as.character(L3),
      W4 = as.character(W4),
      L4 = as.character(L4),
      W5 = as.character(W5),
      L5 = as.character(L5)
    )
  
  allMatches <- allMatches %>% 
    mutate(
      W1 = ifelse(is.na(W1), "", W1),
      L1 = ifelse(is.na(L1), "", L1),
      W2 = ifelse(is.na(W2), "", W2),
      L2 = ifelse(is.na(L2), "", L2),
      W3 = ifelse(is.na(W3), "", W3),
      L3 = ifelse(is.na(L3), "", L3),
      W4 = ifelse(is.na(W4), "", W4),
      L4 = ifelse(is.na(L4), "", L4),
      W5 = ifelse(is.na(W5), "", W5),
      L5 = ifelse(is.na(L5), "", L5)
  )
  return (allMatches)
}



getSet <- function (W, L) {
  if (is.na(W) | is.na(L) | !is.numeric(W) | !is.numeric(L)) {
    return(c(0, 0))
  }
  else if (L == W) {
    return(c(0, 0))
  }
  else { 
    if (W > L & W >= 6) {
      return (c(1, 0)) 
    } 
    else if (L > W & L >= 6) { 
      return (c(0, 1)) 
    } else {
      return (c(0, 0)) 
    }
  }
}

getSets <- function (W1, L1, W2, L2, W3, L3, W4, L4, W5, L5) {
  getSet(W1, L1) + getSet(W2, L2) + getSet(W3, L3) + getSet(W4, L4) + getSet(W5, L5)
}