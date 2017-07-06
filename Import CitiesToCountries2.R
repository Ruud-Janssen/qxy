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


dfCountriesReporterAreas <- as.data.frame(countriesReporterAreas, stringsAsFactors = FALSE)


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

dfCountryCities <- unique(dfCountryCities)


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










city = trimws(unique(allGames$Location))
city <- as.data.frame(city, stringsAsFactors = FALSE)
city <- filter(city, city$city != "")

uniqueCities <- as.integer(count(distinct(city, city)))



library(dplyr)
city2 <- left_join (city, dfCountryCities, by = c('city' = 'city'))
#hmmm some city names occur in multiple countries...

###########################################
# ADD Reference city - country list for cities in different countries
###########################################
CityCountryReference <- data.frame("Melbourne","Australia", stringsAsFactors = FALSE)
names(CityCountryReference) <- c("city","country")

CityCountryReference[nrow(CityCountryReference) + 1,] = c("San Jose","United States")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("London","United Kingdom")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Copenhagen","Denmark")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Santiago","Chile")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Scottsdale","United States")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Miami","United States")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Barcelona","Spain")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Rome","Italy")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Hamburg","Germany")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Paris","France")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Halle","Germany")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Nottingham","United Kingdom")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Newport","United States")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Amsterdam","Netherlands")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Stuttgart","Germany")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Los Angeles","United States")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("San Marino","San Marino")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Toronto","Canada")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Washington","United States")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Palermo","Italy")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Brighton","United Kingdom")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Stockholm","Sweden")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Lisbon","Portugal")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Houston","United States")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Sopot","Poland")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Montreal","Canada")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Madrid","Spain")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Valencia","Spain")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Metz","France")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Las Vegas","United States")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Warsaw","Poland")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Brisbane","Australia")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Belgrade","Serbia")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Eastbourne","United Kingdom")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Moscow","Russia")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Lyon","France")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Basel","Switzerland")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Oeiras","Portugal")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Geneva","Switzerland")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Antwerp","Belgium")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Munich","Germany")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Vienna","Austria")
CityCountryReference[nrow(CityCountryReference) + 1,] = c("Sydney","Australia")

###########################################
# Missing Reference city - country list for cities in different countries
###########################################
MissingCityCountryReference <- data.frame("Mallorca","Spain", stringsAsFactors = FALSE)
names(MissingCityCountryReference) <- c("city","country")

MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("St. Polten","Austria")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Queens Club","United Kingdom")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Kitzbuhel","Austria")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("St. Petersburg","Russia")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Vina del Mar","Chile")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("'s-Hertogenbosch","Netherlands")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("s-Hertogenbosch","Netherlands")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Dubai","United Arab Emirates")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Estoril","Portugal")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Costa Do Sauipe","Brasil")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Ho Chi Min City","China")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Portschach","Austria")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Johannesburg","South Africa")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Shenzhen","China")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Marrakech","Morocco")
MissingCityCountryReference[nrow(MissingCityCountryReference) + 1,] = c("Los Cabos","Mexico")
###########################################


#remove reference rows
city3 <- filter(city2, !city %in% CityCountryReference[[1]])
city4 <- rbind(city3, CityCountryReference) 

#arrange(city4, city)

#remove known missing rows
city5 <- filter(city4, !city %in% MissingCityCountryReference[[1]])
city6 <- rbind(city5, MissingCityCountryReference) 

#check if there are still missing rows
allMatched <- as.integer(sum(is.na(city6$country)))

checkCityCountStartAndEnd <- ifelse (allMatched > 0,
                                "Not all countries are found for all cities, NOT GOOD",
                                ifelse (as.integer(count(city6)) == uniqueCities, 
                                        "All cities are matched with countries, GOOD", 
                                        "NOT All cities are matched with countries, NOT GOOD"
                                )
                            )

checkCityCountStartAndEnd
