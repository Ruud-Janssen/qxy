rm(list = ls())
library(dplyr)
library(plyr)

#Data 2000-2004
wdtrain_rating = "D:/Betting/Tennis/Data/train_rating"
setwd(wdtrain_rating)

data2000<-read.table("2000.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2001<-read.table("2001.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2002<-read.table("2002.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2003<-read.table("2003.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2004<-read.table("2004.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

train_rating = dplyr::bind_rows(data2000, data2001, data2002, data2003, data2004)
train_rating = train_rating[train_rating$Surface!= "Carpet", ]

write.csv(file = "D:/Betting/Tennis/Data/train_rating.csv", train_rating)

#Data from 2005 up to 2012
wdtrain_model = "D:/Betting/Tennis/Data/train_model"
setwd(wdtrain_model)
data2005<-read.table("2005.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2006<-read.table("2006.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2007<-read.table("2007.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2008<-read.table("2008.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2009<-read.table("2009.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2010<-read.table("2010.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2011<-read.table("2011.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2012<-read.table("2012.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

train_model = dplyr::bind_rows(data2005, data2006, data2007, data2008, data2009, 
                                data2010, data2011, data2012)
train_model = train_model[train_model$Surface!= "Carpet", ]

write.csv(file = "D:/Betting/Tennis/Data/train_model.csv", train_model)

#Data from 2013 up to 2014
wdcv = "D:/Betting/Tennis/Data/cv"
setwd(wdcv)
data2013<-read.table("2013.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2014<-read.table("2014.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

cv = dplyr::bind_rows(data2013, data2014)

write.csv(file = "D:/Betting/Tennis/Data/cv.csv", cv)

#Data from 2015 up to 2016
wdtest = "D:/Betting/Tennis/Data/test"
setwd(wdtest)
data2015<-read.table("2015.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2016<-read.table("2016.csv", header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

test = dplyr::bind_rows(data2015, data2016)

write.csv(file = "D:/Betting/Tennis/Data/test.csv", test)

#allData
allData = dplyr::bind_rows(train_rating, train_model, cv, test)
write.csv(file = "D:/Betting/Tennis/Data/all_unaltered.csv", allData)