library(tidyverse)

rm(list = ls())

source("formulas.R")
source("constants.R")

# ATP
data2000 <- read.table(paste(dir_source_ATP, "2000.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2001 <- read.table(paste(dir_source_ATP, "2001.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2002 <- read.table(paste(dir_source_ATP, "2002.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2003 <- read.table(paste(dir_source_ATP, "2003.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2004 <- read.table(paste(dir_source_ATP, "2004.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2005 <- read.table(paste(dir_source_ATP, "2005.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2006 <- read.table(paste(dir_source_ATP, "2006.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2007 <- read.table(paste(dir_source_ATP, "2007.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2008 <- read.table(paste(dir_source_ATP, "2008.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2009 <- read.table(paste(dir_source_ATP, "2009.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2010 <- read.table(paste(dir_source_ATP, "2010.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2011 <- read.table(paste(dir_source_ATP, "2011.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2012 <- read.table(paste(dir_source_ATP, "2012.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2013 <- read.table(paste(dir_source_ATP, "2013.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2014 <- read.table(paste(dir_source_ATP, "2014.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2015 <- read.table(paste(dir_source_ATP, "2015.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2016 <- read.table(paste(dir_source_ATP, "2016.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

all_atp_matches <- dplyr::bind_rows(data2000, data2001, data2002, data2003, data2004, data2005, 
                                    data2006, data2007, data2008, data2009, data2010, data2011, 
                                    data2012, data2013, data2014, data2015, data2016)

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