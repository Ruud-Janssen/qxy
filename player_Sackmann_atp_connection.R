

# Player connection

winner <- distinct(allMatches, Winner)
names(winner) <- c("playername")
loser <- distinct(allMatches, Loser)
names(loser) <- c("playername")
playerAllMatches <- distinct(rbind(winner, loser))
playerAllMatches <- arrange(playerAllMatches, playername)
playerAllMatches2 <- mutate(playerAllMatches, playername2 = str_replace_all(trimws(tolower(str_replace_all(playername, "[^[:alnum:]]", " "))), "  ", " "))

id_winner <- distinct(allMatches, as.integer(idWinner))
names(id_winner) <- c("idPlayer")
id_loser <- distinct(allMatches, as.integer(idLoser))
names(id_loser) <- c("idPlayer")
playerAllMatchesId <- distinct(rbind(id_winner, id_loser))
playerAllMatchesId <- arrange(playerAllMatchesId, idPlayer)
# player$id <- seq.int(nrow(player)) 


player <- getPlayers()

atp_players <- getATPPlayers()



match_players <- left_join(playerAllMatchesId, atp_players, by = c("idPlayer" = "ID") )
match_players <- arrange(match_players, lastName, firstName)

match_players2 <- match_players %>% group_by(lastName, ID2)
match_players3 <- match_players2 %>% summarise(n = n()) %>% filter(n > 1)
#4x -> ID2 = 19740128
filter(match_players, ID2 == 19740128)

match_players4 <- inner_join(match_players3, match_players, by = c("lastName" = "lastName", "ID2" = "ID2"))
