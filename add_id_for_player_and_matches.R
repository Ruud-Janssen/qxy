# There are 2 sources: 
# - ATP (only atp games, contains exact day played, and ODDs, More detailed location), 47k games
# - Sackmann (atp, challenger and others), seems more accurate in data (names and results), 153.6k games
#
# In ATP a tennis player is addressed with its name [Federer R], no id's.
# However this name is sometimes spelled differently.
#
# In here we match players from set ATP and players from set Sackmann
# Based on this match and the day the game is played we find matches
#
# The idea is to have high quality integrated data (validation)
# This includes as well manual work
#
# The output is:
# - Unique playerlist, links to ATP and Sackmann
# - improved ATP data
# - improved Sackmann data
# - matching file between ATP and Sackmann
# 
# The output files will be saved

library(dplyr)
library(stringr)

rm(list = ls())
source("formulas.r")
source("constants.r")
source("playerNameFormulas.r")
source("tournamentFormulas.R")

all_atp_matches <- getDatasets(dir_result_datasets, "all_atp_matches", lvl = "lvl2")
all_Sackmann_matches <- getDatasets(dir_result_datasets, "all_Sackmann_matches", lvl = "lvl2")

all_atp_matches <- create_matching_player_names(all_atp_matches)
all_atp_matches <- update_misspelled_player_names_atp(all_atp_matches)

all_Sackmann_matches <- create_matching_player_names(all_Sackmann_matches)
all_Sackmann_matches <- update_misspelled_player_names_sackmann(all_Sackmann_matches)

# if("idWinner" %in% colnames(allGames)) {
#   allGames <- subset(allGames, select = -c(idWinner) )
# }
# if("idLoser" %in% colnames(allGames)) {
#   allGames <- subset(allGames, select = -c(idLoser) )
# }


# FIRST PART EXTRACT AND CLEANSE PLAYER NAMES
unique_player_list_from_atp_matches <- get_unique_playerlist_from_matches(all_atp_matches)
unique_player_list_from_Sackmann_matches <- get_unique_playerlist_from_matches(all_Sackmann_matches) 

unique_player_list_from_atp_matches$playernameSplit <- strsplit(unique_player_list_from_atp_matches$playername, "[ ]")
unique_player_list_from_Sackmann_matches$playernameSplit <- strsplit(unique_player_list_from_Sackmann_matches$playername, "[ ]")

# for each ATP player check splitted name parts for matching splitted names parts of Sackmann
unique_player_list_from_atp_matches$matched <- lapply(unique_player_list_from_atp_matches$playername, match_player)
matched_player_list <- unique_player_list_from_atp_matches


matched_player_list <- matched_player_list %>% rowwise() %>%
  mutate( 
    matchedNr = length(matched)
  )


# now we have a player list with 0 to n possible matches based on the splitted part matching

# no match found was found for 38 players
# playerMatches0 <- matched_player_list[player$matchedNr == 0, ]

# now there are two possibilities, the ATP player has a different name somehow or he does not exist in the Sackmann data
# the players which do exist are manually added, or adapted so it does match 

# this is no guarantee that the other matched players are correct, it could be that a name was similar but is not the right person
# for now this is not a problem

# TEST in this way we can lookup values
# unique_player_list_from_Sackmann_matches[grepl("nadal+", unique_player_list_from_Sackmann_matches$playername), ]

# POSSIBLE IMPROVEMENT MAKE IT 3 DATA FRAMES instead of a 2 and a matching list in a field (SEE FOR AN EXAMPLE THE SOLUTION FOR MATCHING matches)
# 1) [PLAYER ID ATP] + information
# 2) [PLAYER ID SACKMANN] + information
# 3) [PLAYER ID ATP, PLAYER ID SACKMANN] # the matches

manual_match <- get_manual_match_between_atp_sackmann()
manual_non_match <- get_manual_non_match_between_atp_sackmann()

matched_player_list <- left_join(matched_player_list, manual_non_match, by = c("playername"="playeratp"))
matched_player_list <- left_join(matched_player_list, manual_match, by = c("playername"="playeratp"))

# ifelse cannot work with NULL values therefore switch it to NA
matched_player_list <- matched_player_list %>% rowwise() %>%
  mutate(
    matched = ifelse(is.null(matched), list(NA), list(matched))
  )

matched_player_list <- matched_player_list %>% rowwise() %>%
  mutate(
    matched = ifelse(is.na(playerSackmann), list(matched), list(playerSackmann))
  )

matched_player_list <- matched_player_list %>% rowwise() %>%
  mutate( 
    matchedNr = length(matched)
  )


# at this point we have a match between players when possible, but we don't know for sure which one. 
# By matching the tournament games (there is a huge overlap between both sources, we try to identify the correct names)

# first add the playermatched list to the games
all_atp_matches <- left_join(all_atp_matches, matched_player_list, by = c("Winner2" = "playername") )
# rename fields
all_atp_matches <- all_atp_matches %>%
  rename(
    Winner_player_matched = matched,
    Winner_player_manual = playerManual
  ) %>%
  select(-playerSackmann, -matchedNr)

all_atp_matches <- left_join(all_atp_matches, matched_player_list, by = c("Loser2" = "playername") )
# rename fields
all_atp_matches <- all_atp_matches %>%
  rename(
    Loser_player_matched = matched,
    Loser_player_manual = playerManual
  ) %>%
  select(-playerSackmann, -matchedNr)



all_Sackmann_matches <- adapt_start_date_tournaments(all_Sackmann_matches)
tournament_date_Sackmann <- distinct(select(all_Sackmann_matches, c("tourney_name", "Date")))

tournament_date <- distinct(select(all_atp_matches, c("Location", "Tournament", "Date")))
tournament_date <- arrange(tournament_date, Location, Tournament, Date)
tournament_date <- add_start_date_of_tournament(tournament_date)

all_atp_matches <- left_join(all_atp_matches, tournament_date, by = c("Location" = "Location", "Tournament" = "Tournament", "Date" = "Date"))

# now we loop through all_atp_matches
# and we search the match in Sackmann
# we have to traverse the possible names and look up each individual item
# too limit the amount of filters we could split up the dates in individual lists


all_Sackmann_matches$id <- seq.int(nrow(all_Sackmann_matches))
all_atp_matches$id_atp <- seq.int(nrow(all_atp_matches))

starttime <- Sys.time()

start <- as.Date("2000-01-01", format="%Y-%m-%d")
end   <- as.Date("2017-01-01", format="%Y-%m-%d")

all_atp_matches_matched <- match_tournament_games_atp_sackmann2 (all_atp_matches, all_Sackmann_matches, start, end)

endtime <- Sys.time()

print(starttime)
print(endtime) #takes about 30 minutes

# analyse all_atp_matches_matched
# below we also find the old code to deal with extracting the information from the list
# however this solution did not work as multiple length of lists are not allowed within a DF, so only a single match was returned

s0 <- anti_join(all_atp_matches, all_atp_matches_matched, by = c("id_atp" = "id_atp"))
n0 <- nrow(s0)
  
s <- all_atp_matches_matched %>%
  group_by(id_atp) %>%
  summarize(n = n())

set1 <- s %>%
    filter(n == 1)

set2 <- s %>%
  filter(n == 2)

set3 <- s %>%
  filter(n == 3)

set4 <- s %>%
  filter(n > 3)

n1 <- nrow(set1)
n2 <- nrow(set2)
n3 <- nrow(set3)
n4 <- nrow(set4)

s1 <- inner_join(all_atp_matches, set1, by = c("id_atp" = "id_atp"))
s2 <- inner_join(all_atp_matches, set2, by = c("id_atp" = "id_atp"))
s3 <- inner_join(all_atp_matches, set1, by = c("id_atp" = "id_atp"))
s4 <- inner_join(all_atp_matches, set1, by = c("id_atp" = "id_atp"))

ALL_MATCHES <- inner_join(all_atp_matches_matched, all_atp_matches, by = c("id_atp" = "id_atp"))
ALL_MATCHES <- inner_join(ALL_MATCHES, all_Sackmann_matches, by = c("id_Sackmann" = "id"))
ALL_MATCHES <- ALL_MATCHES %>%
  filter(!id_atp %in% set2$id_atp)
ALL_MATCHES <- arrange(ALL_MATCHES, id_atp, id_Sackmann)




# Old code to analyse the list result
# n <- nrow(small_all_atp_matches)
# 
# m0 <- 0
# m1 <- 0
# m2 <- 0
# m3 <- 0
# 
# for (i in 1 : n) {
#   c <- small_all_atp_matches$Sackman_matched[i]
#   if (length(c) == 0) { m0 <- m0 + 1 }
#   if (length(c) == 1) {
#     if (is.null(unlist(c))) { 
#       m0 <- m0 + 1 
#     } else { 
#       if (is.na(unlist(c))) { 
#         m0 <- m0 + 1 
#       } 
#       else { 
#         m1 <- m1 + 1 
#       }
#     }
#   }
#   if(length(c) == 2) { m2 <- m2 + 1 }
#   if(length(c) > 2) { m3 <- m3 + 1 }
# }
# 
# sm <- small_all_atp_matches %>% rowwise() %>%
#   filter(is.null(unlist(Sackman_matched)))
# 
# sm2 <- small_all_atp_matches %>% rowwise() %>%
#   filter(!is.null(unlist(Sackman_matched)))
# 
# 
# sm3 <- sm2 %>% rowwise() %>%
#   filter(is.na(unlist(Sackman_matched)))
# 
# sm4 <- sm2 %>% rowwise() %>%
#   filter(!is.na(unlist(Sackman_matched)))
# 
# 
# t <- distinct(select(sm3, c("Tournament", "Location", "StartDate")))
# t2 <- distinct(select(sm4, c("Tournament", "Location", "StartDate")))
# 
# 
# 
# 
# z <- all_Sackmann_matches %>%
#   filter(Date > "2012-09-25" & Date < "2012-10-05")
#   Beijing
# rm(z, sm, sm2, sm3, sm4, t, t2, c, i, n, m0, m1,m2,m3,start,starttime,end,endtime, small_all_atp_matches)
# 
# z2 <- all_Sackmann_matches %>%
#   filter(Winner2 == "lleyton hewitt" & Loser2 == "tommy haas") #103720, 103163



# Not everything is matched, but lets first check whether the found games are indeed the same, based on the sets
ALL_MATCHES_correct <- ALL_MATCHES %>%
  filter((W1.x == W1.y & L1.x == L1.y & W2.x == W2.y & L2.x == L2.y & W3.x == W3.y & L3.x == L3.y & W4.x == W4.y & L4.x == L4.y & W5.x == W5.y & L5.x == L5.y))

# old code
# m0 <- 0
# m1 <- 0
# 
# for (i in 1 : nrow(sm4)) {
#   c <- unlist(sm4$Sackman_matched[i])
#   S <- allMatches[c, ]
#   A <- sm4[i, ]
#   if(A$W1 == S$W1 & A$L1 == S$L1 & A$W2 == S$W2 & A$L2 == S$L2 & A$W3 == S$W3 & A$L3 == S$L3 & A$W4 == S$W4 & A$L4 == S$L4 & A$W5 == S$W5 & A$L5 == S$L5) {
#     m1 <- m1 + 1
#   }
#   else {
#     m0 <- m0 + 1
#     print(A[ , c("Location", "Tournament", "Date", "Winner", "Loser", "W1", "L1", "W2", "L2", "W3", "L3", "W4", "L4", "W5", "L5")])
#     print(S[ , c("tourney_name", "Date", "Winner", "Loser", "W1", "L1", "W2", "L2", "W3", "L3", "W4", "L4", "W5", "L5")])
#     #print(A[ , c("Date")])
#     print("-------------------")
#   }
# }


 
# colnames(ALL_MATCHES)

# now we match de playernames based on the found matches between these sets
player_matches_winner <- ALL_MATCHES[ , c("Winner.x", "Winner2.x", "Winner.y", "idWinner")] 
names(player_matches_winner) <- c("playername", "playername2", "Sackmann_playername", "Sackmann_id")

player_matches_loser <- ALL_MATCHES[ , c("Loser.x", "Loser2.x", "Loser.y", "idLoser")] 
names(player_matches_loser) <- c("playername", "playername2", "Sackmann_playername", "Sackmann_id")

player_matches <- distinct(rbind(player_matches_winner, player_matches_loser))
player_matches <- arrange(player_matches, playername)

# old code
# all_Sackmann_players_and_matches <- allMatches[ , c("Winner", "idWinner", "Loser", "idLoser", "id")]
# names(all_Sackmann_players_and_matches) <- c("Sackmann_Winner", "Sackmann_idWinner", "Sackmann_Loser", "Sackmann_idLoser", "id")
# all_atp_matches_Matched <- sm4 # the games with a match
# 
# all_atp_matches_Matched <- all_atp_matches_Matched %>% rowwise() %>%
#   mutate(
#     Sackman_matched2 = as.integer(unlist(Sackman_matched))
#   )
# 
# all_atp_matches_Matched <- inner_join(all_atp_matches_Matched, all_Sackmann_players_and_matches, by = c("Sackman_matched2" = "id"))
# 
# player_matches_winner <- distinct(select(all_atp_matches_Matched, c("Winner", "Winner2", "Sackmann_Winner", "Sackmann_idWinner")))
# names(player_matches_winner) <- c("playername", "playername2", "Sackmann_playername", "Sackmann_id")
# player_matches_loser <- distinct(select(all_atp_matches_Matched, c("Loser", "Loser2", "Sackmann_Loser", "Sackmann_idLoser")))
# names(player_matches_loser) <- c("playername", "playername2", "Sackmann_playername", "Sackmann_id")
# player_matches <- distinct(rbind(player_matches_winner, player_matches_loser))
# player_matches <- arrange(player_matches, playername)
# x1 <- all_atp_matches_Matched[all_atp_matches_Matched$Winner == "Daniel T." | all_atp_matches_Matched$Loser == "Daniel T.", c("Tournament", "StartDate","Winner", "Loser", "Sackmann_Winner", "Sackmann_Loser", "Sackman_matched", "Sackman_matched2")]
# 
# all_Sackmann_matches[all_Sackmann_matches$id == 124735, ]


# remove some columns before saving (lists cannot be saved)
# colnames(all_atp_matches)
all_atp_matches <- all_atp_matches %>%
  select(c(-playernameSplit.x, -playernameSplit.y)) %>%
  select(c(-id.x, -id.y)) %>%
  select(c(-Winner_player_matched, -Loser_player_matched))

# colnames(ALL_MATCHES)
ALL_MATCHES <- ALL_MATCHES %>%
  select(c(-playernameSplit.x, -playernameSplit.y)) %>%
  select(c(-id.x, -id.y)) %>%
  select(c(-Winner_player_matched, -Loser_player_matched))

saveDatasets(all_atp_matches, dir_result_datasets, "all_atp_matches", lvl = "lvl3")
saveDatasets(all_Sackmann_matches, dir_result_datasets, "all_Sackmann_matches", lvl = "lvl3")
saveDatasets(ALL_MATCHES, dir_result_datasets, "all_matches", lvl = "lvl1")
#the player file can be aggregated based on this function
saveDatasets(player_matches, dir_result_datasets, "player_matches", lvl = "lvl1") 

