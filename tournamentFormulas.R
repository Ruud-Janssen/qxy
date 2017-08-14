library(dplyr)

adapt_start_date_tournaments <- function(all_Sackmann_matches) {
  if (is.character(all_Sackmann_matches$Date)) { 
    print("Date column is character convert it first to date") 
  } 
  else {
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-11-14" & all_Sackmann_matches$tourney_name == "London", "Date"] <- "2016-11-13"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-10-10" & all_Sackmann_matches$tourney_name == "Shanghai Masters", "Date"] <- "2016-10-09"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-08-22" & all_Sackmann_matches$tourney_name == "Winston-Salem", "Date"] <- "2016-08-21"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-08-08" & all_Sackmann_matches$tourney_name == "Los Cabos", "Date"] <- "2016-08-10"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-07-18" & all_Sackmann_matches$tourney_name == "Washington", "Date"] <- "2016-07-19"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-06-20" & all_Sackmann_matches$tourney_name == "Nottingham", "Date"] <- "2016-06-19"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-05-23" & all_Sackmann_matches$tourney_name == "Roland Garros", "Date"] <- "2016-05-22"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-05-16" & all_Sackmann_matches$tourney_name == "Nice", "Date"] <- "2016-05-15"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-05-16" & all_Sackmann_matches$tourney_name == "Geneva", "Date"] <- "2016-05-15"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-05-09" & all_Sackmann_matches$tourney_name == "Rome Masters", "Date"] <- "2016-05-08"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-05-02" & all_Sackmann_matches$tourney_name == "Madrid Masters", "Date"] <- "2016-05-01"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-04-11" & all_Sackmann_matches$tourney_name == "Monte Carlo Masters", "Date"] <- "2016-04-10"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-03-21" & all_Sackmann_matches$tourney_name == "Miami Masters", "Date"] <- "2016-03-23"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-03-07" & all_Sackmann_matches$tourney_name == "Indian Wells Masters", "Date"] <- "2016-03-10"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2016-01-11" & all_Sackmann_matches$tourney_name == "Auckland", "Date"] <- "2016-01-10"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2014-05-26" & all_Sackmann_matches$tourney_name == "Roland Garros", "Date"] <- "2014-05-25"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2013-10-07" & all_Sackmann_matches$tourney_name == "Shanghai Masters", "Date"] <- "2013-10-06"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2013-05-27" & all_Sackmann_matches$tourney_name == "Roland Garros", "Date"] <- "2013-05-26"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2013-02-11" & all_Sackmann_matches$tourney_name == "San Jose", "Date"] <- "2013-02-12"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2013-02-04" & all_Sackmann_matches$tourney_name == "Montpellier", "Date"] <- "2013-02-05"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2013-02-04" & all_Sackmann_matches$tourney_name == "Zagreb", "Date"] <- "2013-02-05"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2012-06-11" & all_Sackmann_matches$tourney_name == "Queen's Club", "Date"] <- "2012-06-12"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2012-05-20" & all_Sackmann_matches$tourney_name == "Nice", "Date"] <- "2012-05-22"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2011-10-31" & all_Sackmann_matches$tourney_name == "Valencia", "Date"] <- "2011-10-30"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2011-06-12" & all_Sackmann_matches$tourney_name == "Eastbourne", "Date"] <- "2011-06-13"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2010-08-22" & all_Sackmann_matches$tourney_name == "New Haven", "Date"] <- "2010-08-23"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2010-05-24" & all_Sackmann_matches$tourney_name == "Roland Garros", "Date"] <- "2010-05-23"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2010-01-11" & all_Sackmann_matches$tourney_name == "Auckland", "Date"] <- "2010-01-10"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2009-08-16" & all_Sackmann_matches$tourney_name == "Cincinnati Masters", "Date"] <- "2009-08-17"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2009-07-27" & all_Sackmann_matches$tourney_name == "Umag", "Date"] <- "2009-07-28"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2009-07-19" & all_Sackmann_matches$tourney_name == "Hamburg", "Date"] <- "2009-07-20"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2009-05-25" & all_Sackmann_matches$tourney_name == "Roland Garros", "Date"] <- "2009-05-24"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2009-05-04" & all_Sackmann_matches$tourney_name == "Belgrade", "Date"] <- "2009-05-05"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2008-09-22" & all_Sackmann_matches$tourney_name == "Bangkok", "Date"] <- "2008-09-23"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2008-07-14" & all_Sackmann_matches$tourney_name == "Kitzbuhel", "Date"] <- "2008-07-15"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2008-05-18" & all_Sackmann_matches$tourney_name == "Poertschach", "Date"] <- "2008-05-19"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2008-03-27" & all_Sackmann_matches$tourney_name == "Miami Masters", "Date"] <- "2008-03-26"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2007-11-12" & all_Sackmann_matches$tourney_name == "Masters Cup", "Date"] <- "2007-11-11"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2007-10-22" & all_Sackmann_matches$tourney_name == "Basel", "Date"] <- "2007-10-23"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2007-09-24" & all_Sackmann_matches$tourney_name == "Bangkok", "Date"] <- "2007-09-25"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2007-05-28" & all_Sackmann_matches$tourney_name == "Roland Garros", "Date"] <- "2007-05-27"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2007-03-19" & all_Sackmann_matches$tourney_name == "Miami Masters", "Date"] <- "2007-03-21"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2007-03-05" & all_Sackmann_matches$tourney_name == "Indian Wells Masters", "Date"] <- "2007-03-09"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2007-02-26" & all_Sackmann_matches$tourney_name == "Acapulco", "Date"] <- "2007-02-27"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2007-01-08" & all_Sackmann_matches$tourney_name == "Auckland", "Date"] <- "2007-01-07"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-11-13" & all_Sackmann_matches$tourney_name == "Masters Cup", "Date"] <- "2006-11-12"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-10-23" & all_Sackmann_matches$tourney_name == "Basel", "Date"] <- "2006-10-24"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-09-25" & all_Sackmann_matches$tourney_name == "Bangkok", "Date"] <- "2006-09-26"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-08-21" & all_Sackmann_matches$tourney_name == "New Haven", "Date"] <- "2006-08-20"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-06-26" & all_Sackmann_matches$tourney_name == "Wimbledon", "Date"] <- "2006-06-27"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-06-19" & all_Sackmann_matches$tourney_name == "'s-Hertogenbosch", "Date"] <- "2006-06-18"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-05-29" & all_Sackmann_matches$tourney_name == "Roland Garros", "Date"] <- "2006-05-28"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-03-20" & all_Sackmann_matches$tourney_name == "Miami Masters", "Date"] <- "2006-03-22"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-03-06" & all_Sackmann_matches$tourney_name == "Indian Wells Masters", "Date"] <- "2006-03-10"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2006-01-09" & all_Sackmann_matches$tourney_name == "Auckland", "Date"] <- "2006-01-08"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2005-11-14" & all_Sackmann_matches$tourney_name == "Masters Cup", "Date"] <- "2005-11-13"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2005-10-24" & all_Sackmann_matches$tourney_name == "Basel", "Date"] <- "2005-10-25"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2005-05-16" & all_Sackmann_matches$tourney_name == "St. Poelten", "Date"] <- "2005-05-15"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2005-04-25" & all_Sackmann_matches$tourney_name == "Munich", "Date"] <- "2005-04-26"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2005-03-21" & all_Sackmann_matches$tourney_name == "Miami Masters", "Date"] <- "2005-03-23"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2005-03-07" & all_Sackmann_matches$tourney_name == "Indian Wells Masters", "Date"] <- "2005-03-11"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2005-01-10" & all_Sackmann_matches$tourney_name == "Auckland", "Date"] <- "2005-01-09"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2004-10-25" & all_Sackmann_matches$tourney_name == "Basel", "Date"] <- "2004-10-26"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2004-07-05" & all_Sackmann_matches$tourney_name == "Newport", "Date"] <- "2004-07-06"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2004-05-17" & all_Sackmann_matches$tourney_name == "Casablanca", "Date"] <- "2004-05-16"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2004-03-22" & all_Sackmann_matches$tourney_name == "Miami Masters", "Date"] <- "2004-03-24"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2004-03-08" & all_Sackmann_matches$tourney_name == "Indian Wells Masters", "Date"] <- "2004-03-12"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2004-02-09" & all_Sackmann_matches$tourney_name == "Milan", "Date"] <- "2004-02-10"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2004-01-12" & all_Sackmann_matches$tourney_name == "Auckland", "Date"] <- "2004-01-11"
     #all_Sackmann_matches[all_Sackmann_matches$Date == "2003-11-10" & all_Sackmann_matches$tourney_name == "Masters Cup", "Date"] <- "2003-11-12" VIENNA ??????
     all_Sackmann_matches[all_Sackmann_matches$Date == "2003-10-20" & all_Sackmann_matches$tourney_name == "Basel", "Date"] <- "2003-10-21"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2003-07-28" & all_Sackmann_matches$tourney_name == "Sopot", "Date"] <- "2003-07-29"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2003-05-19" & all_Sackmann_matches$tourney_name == "St. Poelten", "Date"] <- "2003-05-18"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2003-03-17" & all_Sackmann_matches$tourney_name == "Miami Masters", "Date"] <- "2003-03-19"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2003-02-10" & all_Sackmann_matches$tourney_name == "San Jose", "Date"] <- "2003-02-11"
     all_Sackmann_matches[all_Sackmann_matches$Date == "2000-03-20" & all_Sackmann_matches$tourney_name == "Miami Masters", "Date"] <- "2000-03-23"
  }  
  return (all_Sackmann_matches)  
}




add_start_date_of_tournament <- function(tournament_date, threshhold_in_days_same_tournament = 30) {
  tournament_date <- tournament_date %>%
    mutate(
      StartDate = Date
    )

  l_prev <- ""
  t_prev <- ""
  d_prev <- tournament_date$Date[1]
  sd_prev <- tournament_date$Date[1]
  
  for (i in 1 : nrow(tournament_date))
  {
    l <- tournament_date$Location[i]
    t <- tournament_date$Tournament[i]
    d <- tournament_date$Date[i]
    
    
    if (l == l_prev & t == t_prev) {
      #each year a tournament is played, check the diff in days
      if (d - d_prev > threshhold_in_days_same_tournament) {
        #new tournament
        sd_prev <- d        
      }
      else {
        tournament_date$StartDate[i] <- sd_prev
      }
    } else {
      #new tournament does not need to do anything
      sd_prev <- d
    }
    
    l_prev <- l
    t_prev <- t
    d_prev <- d
  }
  
  return (tournament_date)
}

match_tournament_games_atp_sackmann2 <- function (atp_matches, Sackmann_matches, start, end) {
  theDate <- start
  
  #initialize result dataframe
  matched_results <- data.frame(id_atp = integer(0), id_Sackmann = integer(0), idWinner = integer(0), idLoser = integer(0))
  
  # small_all_atp_matches2 <- match_tournament_games_atp_sackmann2 (
  #   all_atp_matches[all_atp_matches$StartDate == theDate-1, ], 
  #   all_Sackmann_matches[all_Sackmann_matches$Date == theDate-1, c("id","Winner2","Loser2","Date")]
  # )
  
  while (theDate <= end)
  {
    print(theDate)
    atp_matches_day <- atp_matches[atp_matches$StartDate == theDate, ]
    
    if(nrow(atp_matches_day) > 0) {
      one_day_atp_matches <- match_tournament_games_atp_sackmann_one_day2 (
        atp_matches_day, 
        Sackmann_matches[Sackmann_matches$Date >= theDate - 3 & Sackmann_matches$Date <= theDate + 3, c("id","Winner2","Loser2","Date", "idWinner", "idLoser")]
      )
      
      matched_results <- bind_rows(matched_results, one_day_atp_matches)
    }
    
    theDate <- theDate + 1
  }
  return (matched_results)
}



match_tournament_games_atp_sackmann_one_day2 <- function (atp_matches, Sackmann_matches) {
  #initialize result dataframe
  matched_results <- data.frame(id_atp = integer(0), id_Sackmann = integer(0), idWinner = integer(0), idLoser = integer(0))

  for (i in 1 : nrow(atp_matches)) {
    #now look into matches to find the matching winner and loser
    for(x in 1 : length(unlist(atp_matches$Loser_player_matched[i]))) {
      #print (unlist(atp_matches$Loser_player_matched[i]))
      if (is.null(unlist(atp_matches$Loser_player_matched[i])[1])) { next }
      ss <- Sackmann_matches %>%
        filter(Loser2 == unlist(atp_matches$Loser_player_matched[i])[x])
      if (nrow(ss) > 0) {
        for (y in 1 : length(unlist(atp_matches$Winner_player_matched[i]))) {
          if (is.null(unlist(atp_matches$Winner_player_matched[i])[1])) { next }
          sss <- ss %>%
            filter(Winner2 == unlist(atp_matches$Winner_player_matched[i])[y])
          if (nrow(sss) > 0) {
            for (z in 1 : nrow(sss)) {
              #print (sss)
              matched_results <- bind_rows(matched_results, c(id_atp = atp_matches$id_atp[i], id_Sackmann = sss$id[z], idWinner = sss$idWinner[z], idLoser = sss$idLoser[z]))
            }
          }
        }
      }
    }
  }
  return (matched_results)
}








#OLD CODE
# match_tournament_games_atp_sackmann <- function (atp_matches, Sackmann_matches, start, end) {
#   theDate <- start
#   
#   # small_all_atp_matches2 <- match_tournament_games_atp_sackmann2 (
#   #   all_atp_matches[all_atp_matches$StartDate == theDate-1, ], 
#   #   all_Sackmann_matches[all_Sackmann_matches$Date == theDate-1, c("id","Winner2","Loser2","Date")]
#   # )
#   
#   while (theDate <= end)
#   {
#     print(theDate)
#     atp_matches_day <- atp_matches[atp_matches$StartDate == theDate, ]
#     
#     if(nrow(atp_matches_day) > 0) {
#       one_day_atp_matches <- match_tournament_games_atp_sackmann_one_day (
#         atp_matches_day, 
#         Sackmann_matches[Sackmann_matches$Date >= theDate - 3 & Sackmann_matches$Date <= theDate + 3, c("id","Winner2","Loser2","Date")]
#       )
#       if (exists("subtotal_atp_matches")) {
#         subtotal_atp_matches <- bind_rows(subtotal_atp_matches, one_day_atp_matches)
#       }
#       else {
#         subtotal_atp_matches <- one_day_atp_matches
#       }
#     }
#     theDate <- theDate + 1
#   }
#   if (!exists("subtotal_atp_matches")) { subtotal_atp_matches <- NULL }
#   
#   return (subtotal_atp_matches)
# }
#   
# 
# 
# match_tournament_games_atp_sackmann_one_day <- function (atp_matches, Sackmann_matches) {
#   #initialize new return column
#   atp_matches$Sackman_matched <- NA
# 
#   for (i in 1 : nrow(atp_matches)) {
#     l <- list()
#     #now look into matches to find the matching winner and loser
#     for(x in 1 : length(unlist(atp_matches$Loser_player_matched[i]))) {
#       #print (unlist(atp_matches$Loser_player_matched[i]))
#       if (is.null(unlist(atp_matches$Loser_player_matched[i])[1])) { next }
#       ss <- Sackmann_matches %>%
#         dplyr::filter(Loser2 == unlist(atp_matches$Loser_player_matched[i])[x])
#       if (nrow(ss) > 0) {
#         for (y in 1 : length(unlist(atp_matches$Winner_player_matched[i]))) {
#           if (is.null(unlist(atp_matches$Winner_player_matched[i])[1])) { next }
#           sss <- ss %>%
#             dplyr::filter(Winner2 == unlist(atp_matches$Winner_player_matched[i])[y])
#           if (nrow(sss) > 0) {
#             for (z in 1 : nrow(sss)) {
#               #print (sss)
#               l <- append(l, sss$id[z])
#             }
#           }
#         }
#       }
#     }
#     if (length(l) > 0) { atp_matches$Sackman_matched[i] <- l }
#   }
#   return (atp_matches)
# }

