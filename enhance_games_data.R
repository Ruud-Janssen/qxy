# Enhance data of the Sackmann dataset
# - add country

rm(list = ls())

source("formulas.R")
source("constants.R")

all_Sackmann_matches <- getDatasets(dir_result_datasets, "all_Sackmann_matches", lvl = "lvl3")

citycountry = read.table("Data/datasets/citycountry.csv",  header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

all_Sackmann_matches$Country = citycountry$country[match(all_Sackmann_matches$Location, citycountry$city)]

saveDatasets(all_Sackmann_matches, dir_result_datasets, "all_Sackmann_matches", lvl = "lvl4")