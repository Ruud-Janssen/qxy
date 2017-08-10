# Enhance data all ATP Games
# - add country

# Enhance data all Challenger Games (convert to format of ATP Games)
# - convert sets field to set standings
# - rename fields

rm(list = ls())

source("formulas.R")
source("constants.R")

all_matches <- getDatasets(dir_result_datasets, "all_matches", lvl = "lvl1")

citycountry = read.table("Data/datasets/citycountry.csv",  header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

all_matches$Country = citycountry$country[match(all_matches$Location, citycountry$city)]






saveDatasets(all_matches, dir_result_datasets, "all_matches", lvl = "lvl2")