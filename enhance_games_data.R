# Enhance data allGames
# - add country

allGames = getAllGamesWithoutRating()

citycountry = read.table("Data/datasets/citycountry.csv",  header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

allGames$Country = citycountry$country[match(allGames$Location, citycountry$city)]




saveDatasetsWithoutRating(allGames)