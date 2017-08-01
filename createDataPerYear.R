rm(list = ls())
library(dplyr)

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

df = "%Y%m%d"

allMatches <- mutate(allMatches, tourney_date = as.Date(as.character(tourney_date), format = df ))
allMatches <- allMatches[with(allMatches, order(tourney_date, atp_match)), ]

allMatches <- allMatches[allMatches$tourney_date >= as.Date("20000101", format = df), ]

allMatches <- dplyr::rename(allMatches, idWinner = winner_id)
allMatches <- dplyr::rename(allMatches, idLoser = loser_id)
allMatches <- dplyr::rename(allMatches, Surface = surface)
allMatches <- dplyr::rename(allMatches, Best.of = best_of)
allMatches <- dplyr::rename(allMatches, Date = tourney_date)
allMatches <- dplyr::rename(allMatches, Winner = winner_name)
allMatches <- dplyr::rename(allMatches, Loser = loser_name)

allMatches <- mutate(allMatches, score = strsplit(score, "[- ]+"))



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
