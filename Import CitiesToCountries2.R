###################################################################
# Import External Data:
# - CountryCities
# - Country CountryCodes
# 
# And convert it to data.frame (done for CountryCities)
###################################################################





###################################################################
# Import data
###################################################################

#install.packages("rjson")
#library("rjson")
#original json_file <- "https://github.com/David-Haim/CountriesToCitiesJSON/blob/master/countriesToCities.json"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))

#or


#install.packages("jsonlite")
library(jsonlite)

#original json_file <- "https://github.com/David-Haim/CountriesToCitiesJSON/blob/master/countriesToCities.json"
json_file <- "Data\\datasets\\countriesToCities.json"
countriesToCities <- fromJSON(json_file, flatten=TRUE)


#http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
json_file <- "https://comtrade.un.org/data/cache/reporterAreas.json"
json_file2 <- "https://comtrade.un.org/data/cache/partnerAreas.json"

countriesReporterAreas <- fromJSON(json_file, flatten=TRUE)
countriesPartnerAreas <- fromJSON(json_file2, flatten=TRUE)
###################################################################





###################################################################
# Conversion
# New approach to convert city country into a data.frame
# (still I don't like it, but the problem is that the simple 'lapply' loses the name of the list which contains the country name)
#
# Runtime:
# 1 sec
###################################################################
convertCityCountry = function(cityCountry, countryNames)
{
  r <- cbind(as.character(unlist(cityCountry)), countryNames)
  r <- as.data.frame(r, stringsAsFactors = FALSE)
  names(r) <- c("country", "city")
  return (r)
}
dfCountryCities <- mapply(convertCityCountry, names(countriesToCities), countriesToCities, SIMPLIFY = FALSE, USE.NAMES = TRUE)
dfCountryCities <- rbind.fill(dfCountryCities)




###################################################################
# Old approach to convert city country
#
# Runtime:
# 1 min
###################################################################
# j=0
# CityCountry <- data.frame(c("city"), c("country"))
# colnames(CityCountry) <- c("city", "country")
# 
# for (i in countriesToCities)
# {
#   j=j+1
#   CC <- cbind(i, names(countriesToCities[j]))
#   colnames(CC) <- c("city", "country")
#   CityCountry <- rbind(CityCountry, CC)
# }
###################################################################





###################################################################
# Test data for binding (Bart)
###################################################################
train_rating = read.table("Data/datasets/train_rating.csv", header = T, sep = ",", quote = "\"",
                          colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
train_model = read.table("Data/datasets/train_model.csv", header = T, sep = ",", quote = "\"",
                         colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
cv = read.table("Data/datasets/cv.csv", header = T, sep = ",", quote = "\"", 
                colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
test = read.table("Data/datasets/test.csv", header = T, sep = ",", quote = "\"",
                  colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

allGames = dplyr::bind_rows(train_rating, train_model, cv, test)

cities = unique(allGames$Location)





