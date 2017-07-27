rm(list = ls())
source("formulas.r")

allGames           <- getAllGamesWithRating()
player             <- getPlayers()
player$Games       <- 0
player$Retirements <- 0

allGames$Winner_retirementFrequency <- NA
allGames$Loser_retirementFrequency  <- NA

Nall = nrow(allGames)

for (i in 1: Nall) {
  
  # get matching winner and loserplayer in rating and save the rownr
  row_nr_winner <- which(player$id == allGames$idWinner[i])
  row_nr_loser  <- which(player$id == allGames$idLoser[i])
  
  if (row_nr_winner > 0 & row_nr_loser > 0) {
    winner_retirement_frequency <- player$Retirements[row_nr_winner] / player$Games[row_nr_winner]
    loser_retirement_frequency  <- player$Retirements[row_nr_loser] / player$Games[row_nr_loser]
    
    if(!is.nan(winner_retirement_frequency)) {
      allGames$Winner_retirementFrequency[i] = winner_retirement_frequency
    }
    
    if(!is.nan(loser_retirement_frequency)) {
      allGames$Loser_retirementFrequency[i] = loser_retirement_frequency
    }
    
    player$Games[row_nr_winner] <- player$Games[row_nr_winner] + 1
    player$Games[row_nr_loser]  <- player$Games[row_nr_loser] + 1
    
    if(allGames$Comment[i] == 'Retired' || allGames$Comment[i] == 'retired' || allGames$Comment[i] == 'Retied') {
      player$Retirements[row_nr_loser]  <- player$Retirements[row_nr_loser] + 1
    }
  }
  else {
    print("ERROR: Player cannot be matched with Rating")
  }
}
    
saveDatasetsWithRating(allGames)
