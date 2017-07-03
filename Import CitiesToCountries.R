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
