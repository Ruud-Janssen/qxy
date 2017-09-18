#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)

rm(list = ls())
source("formulas.r")
source("constants.r")


allGames <- getDatasets(dir_result_datasets, "all_atp_matches", lvl = "lvl3")