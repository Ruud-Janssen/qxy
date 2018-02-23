library(tidyverse)

rm(list = ls())

source("formulas.R")
source("constants.R")

#standard ATP

standard_ATP_files <- Sys.glob(paste(dir_source_ATP, "/*", sep = ""))

all_atp_matches <- bind_rows(lapply(standard_ATP_files, function(x) read.table(x, header = T, sep = ",", quote = "", colClasses = "character", 
                                                                                        stringsAsFactors = FALSE, fill = TRUE)))

saveDatasets(all_atp_matches, dir_result_datasets, "all_atp_matches", "lvl1")


#Sackmann

# Sackmann ATP
Sackmann_ATP_files <- Sys.glob(paste(dir_source_Sackmann_ATP, "/*", sep = ""))

all_Sackmann_ATP_matches <- bind_rows(lapply(Sackmann_ATP_files, function(x) read.table(x, header = T, sep = ",", quote = "", colClasses = "character", 
                                                                            stringsAsFactors = FALSE, fill = TRUE)))
all_Sackmann_ATP_matches <- all_Sackmann_ATP_matches %>% mutate(atp_match = 1) 

# Sackmann Challengers
Sackmann_challengers_files       <- Sys.glob(paste(dir_source_Sackmann_Challengers, "/*", sep = ""))

all_Sackmann_Challengers_matches <- bind_rows(lapply(Sackmann_challengers_files, function(x) read.table(x, header = T, sep = ",", quote = "", colClasses = "character", 
                                                                                 stringsAsFactors = FALSE, fill = TRUE)))
all_Sackmann_Challengers_matches <- all_Sackmann_Challengers_matches %>% mutate(atp_match = 0) 

# Combine Sackmann data
all_Sackmann_matches <- bind_rows(all_Sackmann_ATP_matches, all_Sackmann_Challengers_matches)

# remove matches data before 2000
date_format <- "%Y%m%d"
all_Sackmann_matches <- all_Sackmann_matches %>% mutate(tourney_date = as.Date(as.character(tourney_date), 
                                                                               format = date_format))
all_Sackmann_matches <- all_Sackmann_matches[with(all_Sackmann_matches, order(tourney_date, atp_match)), ]
all_Sackmann_matches <- all_Sackmann_matches %>% filter(tourney_date >= as.Date("20000101", format = date_format))

saveDatasets(all_Sackmann_matches, dir_result_datasets, "all_Sackmann_matches", lvl = "lvl1")

