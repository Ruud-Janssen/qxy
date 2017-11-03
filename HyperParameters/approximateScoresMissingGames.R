#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)
library(purrr)

rm(list = ls())
source("formulas.r")
source("constants.r")

train_rating = read.table("Data/datasets/train_ratingWithRatings.csv"
                          , header = T, sep = ",", quote = "\"", fill = TRUE)
train_model = read.table("Data/datasets/train_modelWithRatings.csv"
                         , header = T, sep = ",", quote = "\"", fill = TRUE)

allGames <- rbind(train_rating, train_model)

allGames <- allGames %>% filter(atp_match == 1)
allGames <- allGames %>% arrange(Date)

allGames$w_svpt   <- as.numeric(allGames$w_svpt)
allGames$w_1stWon <- as.numeric(allGames$w_1stWon)
allGames$w_2ndWon <- as.numeric(allGames$w_2ndWon)

allGames$l_svpt   <- as.numeric(allGames$l_svpt)
allGames$l_1stWon <- as.numeric(allGames$l_1stWon)
allGames$l_2ndWon <- as.numeric(allGames$l_2ndWon)

allGames <- allGames %>% mutate(pointsMissing = is.na(w_svpt))

allGames <- allGames %>% mutate(orderedScore = NA,
                                percentWinnerWonOnServe  = NA,
                                percentWinnerWonOnReturn = NA)


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
  
t <- table(allGames$orderedScore)
t <- t[rev(order(t))]
plot(t)


table(t < 15)
table(t < 10)
table(t < 5)
table(t < 4)
table(t < 3)
table(t < 2)

t2 <- table(allGames %>% filter(pointsMissing == 1) %>% select(orderedScore))
t2 <- t2[rev(order(t2))]
plot(t)
  