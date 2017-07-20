# In the allgames data.frame a tennis player is addressed with its name.
# However this name is sometimes spelled differently. 
# In here we address this problem and add an unique id for each player (id's are ordered by name)
# As well we add a match id based on the id's of both players formula = [smallest id]-[largest id]
# And we add an result column which is either 1 or -1, 1 is victory for the smallest id and 1 is a victory for the largest id
# the player unique names and id file is saved as well

#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)

rm(list = ls())
source("formulas.r")

allGames <- getAllGamesWithoutRating()

if("idWinner" %in% colnames(allGames)) {
  allGames <- subset(allGames, select = -c(idWinner) )
}
if("idLoser" %in% colnames(allGames)) {
  allGames <- subset(allGames, select = -c(idLoser) )
}

allGames <- mutate(allGames, Winner2 = str_replace_all(trimws(tolower(str_replace_all(Winner, "[^[:alnum:]]", " "))), "  ", " "))
allGames <- mutate(allGames, Loser2 = str_replace_all(trimws(tolower(str_replace_all(Loser, "[^[:alnum:]]", " "))), "  ", " "))
allGames <- allGames %>% filter(Winner2 != "" | Loser2 != "")

allGames[allGames$Winner2 == "chela j i", "Winner2"] <- "chela j"
allGames[allGames$Loser2 == "chela j i", "Loser2"] <- "chela j"

allGames[allGames$Winner2 == "del potro j m", "Winner2"] <- "del potro j"
allGames[allGames$Loser2 == "del potro j m", "Loser2"] <- "del potro j"

allGames[allGames$Winner2 == "estrella burgos v", "Winner2"] <- "estrella v"
allGames[allGames$Loser2 == "estrella burgos v", "Loser2"] <- "estrella v"

allGames[allGames$Winner2 == "gallardo valles m", "Winner2"] <- "gallardo m"
allGames[allGames$Loser2 == "gallardo valles m", "Loser2"] <- "gallardo m"

allGames[allGames$Winner2 == "haji a", "Winner2"] <- "hajji a"
allGames[allGames$Loser2 == "haji a", "Loser2"] <- "hajji a"

allGames[allGames$Winner2 == "haider mauer a", "Winner2"] <- "haider maurer a"
allGames[allGames$Loser2 == "haider mauer a", "Loser2"] <- "haider maurer a"

allGames[allGames$Winner2 == "hantschek m", "Winner2"] <- "hantschk m"
allGames[allGames$Loser2 == "hantschek m", "Loser2"] <- "hantschk m"

allGames[allGames$Winner2 == "jun w s", "Winner2"] <- "jun w"
allGames[allGames$Loser2 == "jun w s", "Loser2"] <- "jun w"

allGames[allGames$Winner2 == "kunitcin i", "Winner2"] <- "kunitsyn i"
allGames[allGames$Loser2 == "kunitcin i", "Loser2"] <- "kunitsyn i"

allGames[allGames$Winner2 == "lisnard j r", "Winner2"] <- "lisnard j"
allGames[allGames$Loser2 == "lisnard j r", "Loser2"] <- "lisnard j"

allGames[allGames$Winner2 == "mathieu p h", "Winner2"] <- "mathieu p"
allGames[allGames$Loser2 == "mathieu p h", "Loser2"] <- "mathieu p"

allGames[allGames$Winner2 == "sanchez de luna j a", "Winner2"] <- "sanchez de luna j"
allGames[allGames$Loser2 == "sanchez de luna j a", "Loser2"] <- "sanchez de luna j"

#madrid??!!??!!
allGames[allGames$Winner2 == "riba madrid p", "Winner2"] <- "riba p"
allGames[allGames$Loser2 == "riba madrid p", "Loser2"] <- "riba p"

#initials based split, there is granollers,m and granollers pujol,g nothing else
allGames[allGames$Winner2 == "granollers g", "Winner2"] <- "granollers pujol g"
allGames[allGames$Loser2 == "granollers g", "Loser2"] <- "granollers pujol g"
allGames[allGames$Winner2 == "granollers pujol m", "Winner2"] <- "granollers m"
allGames[allGames$Loser2 == "granollers pujol m", "Loser2"] <- "granollers m"




winner <- allGames %>% distinct(Winner2)
names(winner) <- c("playername")

loser <- allGames %>% distinct(Loser2)
names(loser) <- c("playername")

player <- distinct(rbind(winner, loser))

player <- arrange(player, playername)
player$id <- seq.int(nrow(player)) 





allGames <- left_join(allGames, player, by = c("Winner2" = "playername") )
allGames <- dplyr::rename(allGames, idWinner = id)

allGames <- left_join(allGames, player, by = c("Loser2" = "playername") )
allGames <- dplyr::rename(allGames, idLoser = id)

allGames <- mutate(allGames, Match = ifelse(idWinner < idLoser, paste(idWinner, idLoser, sep="-"), paste(idLoser, idWinner, sep="-")))
allGames <- mutate(allGames, Result = ifelse(idWinner < idLoser, 1, -1))


if("Winner2" %in% colnames(allGames)) {
  allGames <- subset(allGames, select = -c(Winner2) )
}
if("Loser2" %in% colnames(allGames)) {
  allGames <- subset(allGames, select = -c(Loser2) )
}

saveDatasetsWithoutRating(allGames)

savePlayers(player)
