library(jsonlite)
library(dplyr)

rm(list = ls())
source("formulas.r")

# Import data -----------------------------------------------------------------
# original json file "https://github.com/David-Haim/CountriesToCitiesJSON/blob/master/countriesToCities.json"
json_countries_to_cities <- "Data\\datasets\\countriesToCities.json"
countries_to_cities <- fromJSON(json_countries_to_cities, flatten=TRUE)



# Conversion of countries_to_cities into a data.frame -------------------------
# The simple 'lapply' loses the name of the list which contains the country name
convert_countries_to_cities = function(list_countries_to_cities, 
                                       country_names) {
  r <- cbind(as.character(unlist(list_countries_to_cities)), country_names)
  r <- as.data.frame(r, stringsAsFactors = FALSE)
  names(r) <- c("country", "city")
  return (r)
}
df_countries_to_cities <- mapply(convert_countries_to_cities, names(countries_to_cities), countries_to_cities, SIMPLIFY = FALSE, USE.NAMES = TRUE)
df_countries_to_cities <- bind_rows(df_countries_to_cities, NULL)
df_countries_to_cities <- unique(df_countries_to_cities)



# get data frame of cities from tennis data -----------------------------------
allGamesWithoutRating <- getAllGamesWithoutRating()

city <- trimws(unique(allGamesWithoutRating$Location))
city <- as.data.frame(city, stringsAsFactors = FALSE)
city <- filter(city, city$city != "")
unique_cities <- count(distinct(city, city))



# join data city with countries_to_cities -------------------------------------
city <- left_join (city, df_countries_to_cities, by = c('city' = 'city'))
# Some city names occur in multiple countries

# Add Reference city - country list for cities in different countries
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
  
  
  # Missing Reference city - country list for cities in different countries
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



# first remove reference rows with multiple countries
city <- filter(city, !city %in% CityCountryReference[[1]])

# add correct reference rows for cities with multiple countries
city <- rbind(city, CityCountryReference) 

# remove known cities/places with missing countries (country = NA)
city <- filter(city, !city %in% MissingCityCountryReference[[1]])

# add missing countries for city/places
city <- rbind(city, MissingCityCountryReference) 



# Validation check if there are still missing rows ---------------------------- 
allMatched <- as.integer(sum(is.na(city$country)))

if (allMatched > 0) {
  "Not all countries are found for all cities, NOT GOOD"
  } else if (nrow(city) == unique_cities) { 
    "All cities are matched with countries, GOOD"
  } else {
    "NOT All cities are matched with countries, NOT GOOD"
  }


# Export result city country --------------------------------------------------
write.csv(file = "Data/datasets/citycountry.csv", city, row.names=FALSE)



# clean up variables ----------------------------------------------------------
rm(json_countries_to_cities, CityCountryReference, df_countries_to_cities, allMatched, countries_to_cities, MissingCityCountryReference, convert_countries_to_cities, unique_cities)