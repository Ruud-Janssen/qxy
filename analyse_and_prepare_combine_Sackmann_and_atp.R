library(dplyr)


rm(list = ls())

source("formulas.R")
source("constants.R")
source("enhance_games_data_formulas.R")

# import aggregated source files
all_atp_matches <- getDatasets(dir_result_datasets, "all_atp_matches", lvl = "lvl1", change_datatype = FALSE)
all_Sackmann_matches <- getDatasets(dir_result_datasets, "all_Sackmann_matches", lvl = "lvl1", change_datatype = FALSE)

# colnames(all_atp_matches)

# clean up unused columns
drop_cols <- c(
  "CBW",
  "CBL",
  "GBW",
  "GBL",
  "IWW",
  "IWL",
  "SBW",
  "SBL",
  "B365W",
  "B365L",
  "B.WW",
  "B.WL",
  "EXW",
  "EXL",
  "PSW",
  "PSL",
  "UBW",
  "UBL",
  "LBW",
  "LBL",
  "SJW",
  "SJL",
  "MaxL",
  "MaxW",
  "AvgW",
  "AvgL"
)
all_atp_matches <-
  all_atp_matches %>% select(-one_of(drop_cols)) 

drop_cols <- c(
  "WRank",
  "LRank",
  "WPts",
  "LPts",
  "AvgW",
  "AvgL"
)

all_atp_matches <-
  all_atp_matches %>% select(-one_of(drop_cols)) 




# colnames(all_Sackmann_matches)

# clean up unused columns
drop_cols <- c(
  "w_ace",
  "w_df",
  "w_svpt",
  "w_1stIn",
  "w_1stWon",
  "w_2ndWon",
  "w_SvGms",
  "w_bpSaved",
  "w_bpFaced",
  "l_ace",
  "l_df",
  "l_svpt",
  "l_1stIn",
  "l_1stWon",
  "l_2ndWon",
  "l_SvGms",
  "l_bpSaved",
  "l_bpFaced",
  "match_num"
)
  
all_Sackmann_matches <-
  all_Sackmann_matches %>% select(-one_of(drop_cols)) 

# clean up unused columns, player info
drop_cols <- c(
  "winner_seed",
  "winner_entry",
  "winner_rank",
  "winner_rank_points",
  
  "loser_seed",
  "loser_entry",
  "loser_rank",
  "loser_rank_points"
)

all_Sackmann_matches <-
  all_Sackmann_matches %>% select(-one_of(drop_cols)) 

#summary(as.factor(all_Sackmann_matches$match_num))




# match columns, data types, and information
all_Sackmann_matches <- rename(all_Sackmann_matches, 
                                      idWinner = winner_id, 
                                      idLoser = loser_id, 
                                      Surface = surface, 
                                      Best.of = best_of, 
                                      Date = tourney_date, 
                                      Winner = winner_name,
                                      Loser = loser_name,
                                      Round = round
                                      
)

#distinct(select(all_Sackmann_matches, c("tourney_level", "atp_match")))

#distinct(select(all_atp_matches, c("ï..ATP", "ATP")))

#distinct(select(all_atp_matches, c("Round")))
#summary(as.factor(all_atp_matches$Round))
#distinct(select(all_Sackmann_matches, c("Round")))
#br <- filter(all_Sackmann_matches, Round == "BR") # Olympics
#olympics <- filter(all_Sackmann_matches, tourney_name == "Olympics") # Olympics

#distinct(select(all_Sackmann_matches, c("tourney_name", "Round")))
#distinct(select(all_atp_matches, c("Tournament", "Round")))

#Sackman -> Round == RR -> davis cup
#Sackman -> Round == BR -> Olympics 3/4 place match

#date_sackmann <- distinct(select(all_Sackmann_matches, c("Date")))
#date_atp <- distinct(select(all_atp_matches, c("Date")))
# date is everywhere available

all_atp_matches <- all_atp_matches %>%
  mutate(
    Location = replace(Location, Location == "s-Hertogenbosch", "'s-Hertogenbosch"),
    Date = as.Date(Date, format="%m/%d/%Y")
  )
                
all_Sackmann_matches <- all_Sackmann_matches %>%
  mutate(
    Date = as.Date(Date, format="%Y-%m-%d")
  )

#align sets and games
all_atp_matches <- all_atp_matches %>% 
  mutate(
    W1 = ifelse(W1 == " ", "", W1),
    L1 = ifelse(L1 == " ", "", L1),
    W2 = ifelse(W2 == " ", "", W2),
    L2 = ifelse(L2 == " ", "", L2),
    W3 = ifelse(W3 == " ", "", W3),
    L3 = ifelse(L3 == " ", "", L3),
    W4 = ifelse(W4 == " ", "", W4),
    L4 = ifelse(L4 == " ", "", L4),
    W5 = ifelse(W5 == " ", "", W5),
    L5 = ifelse(L5 == " ", "", L5)
  )

# parse score column Sackmann "6-4 6-7(15) 5-4 RET into W1, L1, ... and WSets, LSets...
all_Sackmann_matches <- parse_score_column_Sackmann(all_Sackmann_matches)
print("WARNINGS OCCUR HERE AS WE TRY TO PARSE ALSO NON-NUMERIC VALUES, (THIS IS AS EXPECTED, AS THIS WAS NOT CLEANED UP WITH grep)")

# date <- distinct(select(all_atp_matches, c("Date")))
# 
# check for 2 games on 1 day 
# about 5000 times players play twice on one day 
# NOTE, TOURNAMENT DATE IN 2000, 2001, 2002 THEN EXACT DATE FOR INDIVIDUAL ROUND, including the two tournaments crossing the year 2002 to 2003
# 
# all_atp_matches2003 <- filter(all_atp_matches, as.Date(Date, format="%m/%d/%Y") < "2003-01-01")
# tournament_Date <- distinct(select(all_atp_matches2003, c("Location", "Tournament", "Date")))
# 
# 
# date_winner <- distinct(select(all_atp_matches2003, c("Date", "Winner")))
# date_loser <- distinct(select(all_atp_matches2003, c("Date", "Loser")))
# date_loser <- rename(date_loser, Winner = Loser)
# date_player <- rbind(date_winner, date_loser)
# date_playerD <- distinct(select(date_player, c("Date", "Winner")))
# 
# tournament_date <- distinct(select(all_atp_matches, c("Location", "Tournament", "Date")))
# tournament_date_Sackmann <- distinct(select(all_Sackmann_matches, c("tourney_name", "Date")))
# 
# 
# date_winner_loser <- distinct(select(all_atp_matches, c("Date", "Winner", "Loser"))) #1 game twice mentioned
# all_atp_matches %>%
#   group_by(Date, Winner, Loser) %>%
#   summarise(n = n()) %>%
#   filter(n > 1)
# 
# all_atp_matches %>%
#   filter(Date == "11/12/2001" & Winner == "Hewitt L." & Loser == "Grosjean S.")
# 
# all_atp_matches %>%
#   filter(Tournament == "Masters Cup" & Location == "Sydney")


# tournament name difficult to match





# Export aggregated source files
saveDatasets(all_atp_matches, dir_result_datasets, "all_atp_matches", lvl = "lvl2")
saveDatasets(all_Sackmann_matches, dir_result_datasets, "all_Sackmann_matches", lvl = "lvl2")