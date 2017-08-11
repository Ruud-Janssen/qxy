create_matching_player_names <- function(allGames) {
  allGames <- allGames %>% 
    mutate(
      Winner2 = str_replace_all(trimws(tolower(str_replace_all(Winner, "[^[:alnum:]]", " "))), "  ", " "),
      Loser2 = str_replace_all(trimws(tolower(str_replace_all(Loser, "[^[:alnum:]]", " "))), "  ", " ")
    ) %>% 
    filter(Winner2 != "" | Loser2 != "")
  return (allGames)
}

update_misspelled_player_names_atp <- function(allGames) {
  allGames[allGames$Winner2 == "chela j i", "Winner2"] <- "chela j"
  allGames[allGames$Loser2 == "chela j i", "Loser2"] <- "chela j"
  
  allGames[allGames$Winner2 == "dell acqua m", "Winner2"] <- "dellacqua m"
  allGames[allGames$Loser2 == "dell acqua m", "Loser2"] <- "dellacqua m"
  
  allGames[allGames$Winner2 == "del potro j m", "Winner2"] <- "del potro j"
  allGames[allGames$Loser2 == "del potro j m", "Loser2"] <- "del potro j"
  
  allGames[allGames$Winner2 == "del bonis f", "Winner2"] <- "delbonis f"
  allGames[allGames$Loser2 == "del bonis f", "Loser2"] <- "delbonis f"
  
  allGames[allGames$Winner2 == "de heart r", "Winner2"] <- "deheart r"
  allGames[allGames$Loser2 == "de heart r", "Loser2"] <- "deheart r"
  
  allGames[allGames$Winner2 == "dev varman s", "Winner2"] <- "devvarman s"
  allGames[allGames$Loser2 == "dev varman s", "Loser2"] <- "devvarman s"
  
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
  
  allGames[allGames$Winner2 == "wang y t", "Winner2"] <- "wang j"
  allGames[allGames$Loser2 == "wang y t", "Loser2"]  <- "wang j"
  
  allGames[allGames$Winner2 == "sanchez de luna j a", "Winner2"] <- "sanchez de luna j"
  allGames[allGames$Loser2 == "sanchez de luna j a", "Loser2"] <- "sanchez de luna j"
  
  allGames[allGames$Winner2 == "al ghareeb m", "Winner2"] <- "ghareeb m"
  allGames[allGames$Loser2 == "al ghareeb m", "Loser2"] <- "ghareeb m"
  
  allGames[allGames$Winner2 == "querry s", "Winner2"] <- "querrey s"
  allGames[allGames$Loser2 == "querry s", "Loser2"] <- "querrey s"
  
  allGames[allGames$Winner2 == "bahrouzyan o", "Winner2"] <- "awadhy o"
  allGames[allGames$Loser2 == "bahrouzyan o", "Loser2"] <- "awadhy o"
  
  allGames[allGames$Winner2 == "schuettler p", "Winner2"] <- "schuettler r"
  allGames[allGames$Loser2 == "schuettler p", "Loser2"] <- "schuettler r" 
  
  #madrid??!!??!!
  allGames[allGames$Winner2 == "riba madrid p", "Winner2"] <- "riba p"
  allGames[allGames$Loser2 == "riba madrid p", "Loser2"] <- "riba p"
  
  #initials based split, there is granollers,m and granollers pujol,g nothing else
  allGames[allGames$Winner2 == "granollers g", "Winner2"] <- "granollers pujol g"
  allGames[allGames$Loser2 == "granollers g", "Loser2"] <- "granollers pujol g"
  allGames[allGames$Winner2 == "granollers pujol m", "Winner2"] <- "granollers m"
  allGames[allGames$Loser2 == "granollers pujol m", "Loser2"] <- "granollers m"
  
  return (allGames)
}

update_misspelled_player_names_sackmann <- function(allGames) {
  allGames[allGames$Winner2 == "albert ramos", "Winner2"] <- "albert ramos vinolas"
  allGames[allGames$Loser2 == "albert ramos", "Loser2"]  <- "albert ramos vinolas"
  
  allGames[allGames$Winner2 == "ivan navarro", "Winner2"] <- "ivan navarro pastor"
  allGames[allGames$Loser2 == "ivan navarro", "Loser2"]  <- "ivan navarro pastor"
  
  
  

  return (allGames)
}

get_manual_match_between_atp_sackmann <- function() {
  manual_match <- data.frame(playeratp = "al alawi s k", playerSackmann = "sultan khalfan", stringsAsFactors = FALSE)
  manual_match <- manual_match %>% 
    bind_rows(c(playeratp = "ali mutawa j m", playerSackmann = "jabor al mutawa")) %>%
    bind_rows(c(playeratp = "dolgopolov o", playerSackmann = "alexandr dolgopolov")) %>%
    bind_rows(c(playeratp = "dutra da silva r", playerSackmann = "daniel dutra da silva")) %>%
    bind_rows(c(playeratp = "granollers pujol g", playerSackmann = "gerard granollers")) %>%
    bind_rows(c(playeratp = "jones g d", playerSackmann = "gd jones")) %>%
    bind_rows(c(playeratp = "al ghareeb m", playerSackmann = "ali ghareeb")) %>% #or mohammad ghareeb
    bind_rows(c(playeratp = "o brien a", playerSackmann = "alex obrien")) %>%
    bind_rows(c(playeratp = "o neal j", playerSackmann = "justin oneal")) %>%
    bind_rows(c(playeratp = "sultan khalfan a", playerSackmann = "sultan khalfan")) %>%
    bind_rows(c(playeratp = "van der dium a", playerSackmann = "antal van der duim"))
  
  return (manual_match)  
}

get_manual_non_match_between_atp_sackmann <- function() {
  manual_non_match <- data.frame(playeratp = "ancic i", playerManual = "Ivica Ancic", stringsAsFactors = FALSE)
  manual_non_match <- manual_non_match %>% 
    bind_rows(c(playeratp = "artunedo martinavarro a", playerManual = "Artunedo Martinavarro")) %>%
    bind_rows(c(playeratp = "fnisk", playerManual = "Fnisk")) %>%
    bind_rows(c(playeratp = "haas p", playerManual = "Philippe Haas")) %>%
    bind_rows(c(playeratp = "bahrouzyan o", playerManual = "O Bahrouzyan")) %>%
    bind_rows(c(playeratp = "chekov p", playerManual = "P Chekov")) %>%
    bind_rows(c(playeratp = "guccione a", playerManual = "A Guccione")) %>%
    bind_rows(c(playeratp = "kutac r", playerManual = "R Kutac")) %>%
    bind_rows(c(playeratp = "luczak a", playerManual = "A Luczak")) %>%
    bind_rows(c(playeratp = "march o", playerManual = "O March"))  %>%   
    bind_rows(c(playeratp = "marin l", playerManual = "Lucas Marin"))  %>% 
    bind_rows(c(playeratp = "nadal pareira r", playerManual = "R Nadal Pareira"))  %>% # or Rafael Nadal
    bind_rows(c(playeratp = "navarro pastor i", playerManual = "I Navarro Pastor"))  %>% # or Pablo Navarro-Pastor
    bind_rows(c(playeratp = "nedovyesov o", playerManual = "O Nedovyesov"))  %>% # or Aleksandr Nedovyesov
    bind_rows(c(playeratp = "prpic a", playerManual = "Adrian Prpic"))  %>%
    bind_rows(c(playeratp = "querry s", playerManual = "sam Querry"))  %>%  
    bind_rows(c(playeratp = "ramos vinolas a", playerManual = "Albert Ramos Vinolas"))  %>%
    bind_rows(c(playeratp = "ruevski p", playerManual = "P Ruevski"))  %>%    
    bind_rows(c(playeratp = "schuettler p", playerManual = "Patrick Schuttler"))  %>%  
    bind_rows(c(playeratp = "schuttler p", playerManual = "Patrick Schuttler"))  %>%    
    bind_rows(c(playeratp = "verdasco m", playerManual = "Marco Verdasco Crespo")) %>%
    bind_rows(c(playeratp = "vicente m", playerManual = "M Vicente")) %>%  
    bind_rows(c(playeratp = "wang y t", playerManual = "Y T Wang")) %>%  
    bind_rows(c(playeratp = "zayid m s", playerManual = "Mubarak Shannan Zayid"))

  return (manual_non_match)  
}

get_unique_playerlist_from_matches <- function(allGames) {
  winner <- allGames %>% distinct(Winner2)
  names(winner) <- c("playername")
  
  loser <- allGames %>% distinct(Loser2)
  names(loser) <- c("playername")
  
  player <- distinct(rbind(winner, loser))
  
  player <- arrange(player, playername)
  player$id <- seq.int(nrow(player))
  return (player)
}

match_player <- function (p, p2 = unique_player_list_from_Sackmann_matches) {
  #print(p)
  psplit <- unlist(strsplit(p, "[ ]"))
  #now for each list object in playersplit, find it in the same playername2
  #if all list objects are find in player2 return these players
  matched_players <- c()
  n <- nrow(p2)
  for(i in 1 : n) {
    if(all(unlist(lapply(psplit, function (text) { length(grep(paste("^",text,"+", sep = ""), unlist(p2[i, "playernameSplit"]))) > 0 }))) == TRUE) {
      matched_players <- append(matched_players, p2[i, "playername"])
    }
  }
  return (matched_players)
}
# test enkel record
# match_player("dosedel s", unique_player_list_from_Sackmann_matches)