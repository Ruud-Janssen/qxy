rm(list = ls())
library(dplyr)
#install.packages("tidyr")
library(tidyr)
library(stringr)

source("formulas.r")

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



atp_matches_dir <- "Data/SackmannData/atp"
atp_matches_files <- Sys.glob(paste(atp_matches_dir, "/*", sep = ""))

allAtpMatches <- bind_rows(lapply(atp_matches_files, function(x) read.table(x, header = T, sep = ",", quote = "", colClasses = "character", 
                                                        stringsAsFactors = FALSE, fill = TRUE)))
allAtpMatches$atp_match <- 1

atp_challengers_dir <- "Data/SackmannData/challengers"
atp_challengers_files <- Sys.glob(paste(atp_challengers_dir, "/*", sep = ""))

allChallengers <- bind_rows(lapply(atp_challengers_files, function(x) read.table(x, header = T, sep = ",", quote = "", colClasses = "character", 
                                                                             stringsAsFactors = FALSE, fill = TRUE)))
allChallengers$atp_match <- 0

allMatches <- bind_rows(allAtpMatches, allChallengers)

df <- "%Y%m%d"

allMatches <- mutate(allMatches, tourney_date = as.Date(as.character(tourney_date), format = df ))
allMatches <- allMatches[with(allMatches, order(tourney_date, atp_match)), ]

allMatches <- allMatches[allMatches$tourney_date >= as.Date("20000101", format = df), ]

allMatches <- dplyr::rename(allMatches, 
                            idWinner = winner_id, 
                            idLoser = loser_id, 
                            Surface = surface, 
                            Best.of = best_of, 
                            Date = tourney_date, 
                            Winner = winner_name,
                            Loser = loser_name
)
# copy for testing purposes
allMatches$score2 <- allMatches$score

# remove tie break points between "( )"
allMatches <- mutate(allMatches, score = gsub( "*\\(.*?\\) *", "", score))


#vars <- c("W1","L1","W2","L2","W3","L3","W4","L4","W5","L5", "rest", "rest1", "rest2")
vars <- c("W1","L1","W2","L2","W3","L3","W4","L4","W5","L5")

#split vars in variables
allMatches <- allMatches %>% 
  separate(score, into = vars, sep = "[- ]+")

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

# allMatches <- allMatches %>% 
#   mutate(W1set = 
#            ifelse (is.na(W1), "-", 
#             ifelse (is.na(L1), "-",
#              ifelse (W1 > L1, "W", 
#               ifelse (L1 > W1, "L", "?")
#              )
#             )
#            )
#   )
# distinct(allMatches, W1set)
# allMatches[allMatches$W1set == "?", ]

#Wsets
#Lsets

# getSets(6,4,4,6,7,6,"", "", "", "")

allMatches <- allMatches %>% 
  rowwise() %>% mutate(
    W1set = getSets(W1, L1, W2, L2, W3, L3, W4, L4, W5, L5)[1],
    L1set = getSets(W1, L1, W2, L2, W3, L3, W4, L4, W5, L5)[2]
  ) 
           
winner <- distinct(allMatches, Winner)
names(winner) <- c("playername")
loser <- distinct(allMatches, Loser)
names(loser) <- c("playername")
playerAllMatches <- distinct(rbind(winner, loser))
playerAllMatches <- arrange(playerAllMatches, playername)
# player$id <- seq.int(nrow(player)) 

playerAllMatches2 <- mutate(playerAllMatches, playername2 = str_replace_all(trimws(tolower(str_replace_all(playername, "[^[:alnum:]]", " "))), "  ", " "))




player <- getPlayers()











data2000 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2000, ]
data2001 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2001, ]
data2002 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2002, ]
data2003 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2003, ]
data2004 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2004, ]
data2005 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2005, ]
data2006 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2006, ]
data2007 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2007, ]
data2008 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2008, ]
data2009 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2009, ]
data2010 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2010, ]
data2011 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2011, ]
data2012 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2012, ]
data2013 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2013, ]
data2014 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2014, ]
data2015 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2015, ]
data2016 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2016, ]
data2017 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2017, ]
