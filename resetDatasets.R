rm(list = ls())

source("import_data_atp_and_challengers.R")

source("analyse_and_prepare_combine_Sackmann_and_atp.R")

source("import_cities_to_countries.R")

source("add_id_for_player_and_matches.R")

source("enhance_games_data.R")
source("hyperparameters/approximateScoresMissingGames.R")

# Not finished and needs to be based on previous changes
# source("player_Sackmann_atp_connection.R")


source("addHeadtoHeadAndPrevMatches.R")
source("addRetiredandFatigue.R")
source("setPercentSetsGamesPointsWon.R")

source("CommonOpponents/addCommonOpponents.R")
source("addratings.R")
source("addratingsservereturn.R")

#source("addGlicko.R")
#source("addGlicko2.R")

source("split_train_CV_test.R")

