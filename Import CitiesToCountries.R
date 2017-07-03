#install.packages("rjson")
#library("rjson")

#original json_file <- "https://github.com/David-Haim/CountriesToCitiesJSON/blob/master/countriesToCities.json"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))


#or


install.packages("jsonlite")
library(jsonlite)

#original json_file <- "https://github.com/David-Haim/CountriesToCitiesJSON/blob/master/countriesToCities.json"
json_file <- "Data\\datasets\\countriesToCities.json"
countriesToCities <- fromJSON(json_file, flatten=TRUE)


#http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
json_file <- "https://comtrade.un.org/data/cache/reporterAreas.json"
json_file2 <- "https://comtrade.un.org/data/cache/partnerAreas.json"

countriesReporterAreas <- fromJSON(json_file, flatten=TRUE)
countriesPartnerAreas <- fromJSON(json_file2, flatten=TRUE)
