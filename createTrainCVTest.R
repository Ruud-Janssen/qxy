rm(list = ls())
library(dplyr)

saveDatasetsDir = "Data/datasets/"
#Data 2000-2004
dataTrainRatingDir = "Data/train_rating/"

data2000<-read.table(paste(dataTrainRatingDir, "2000.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2001<-read.table(paste(dataTrainRatingDir, "2001.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2002<-read.table(paste(dataTrainRatingDir, "2002.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2003<-read.table(paste(dataTrainRatingDir, "2003.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2004<-read.table(paste(dataTrainRatingDir, "2004.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

train_rating = dplyr::bind_rows(data2000, data2001, data2002, data2003, data2004)

write.csv(file = paste(saveDatasetsDir, "train_rating.csv", sep = ""), train_rating, row.names=FALSE)

#Data from 2005 up to 2012
dataTrainModelDir = "Data/train_model/"

data2005<-read.table(paste(dataTrainModelDir, "2005.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2006<-read.table(paste(dataTrainModelDir, "2006.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2007<-read.table(paste(dataTrainModelDir, "2007.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2008<-read.table(paste(dataTrainModelDir, "2008.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2009<-read.table(paste(dataTrainModelDir, "2009.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2010<-read.table(paste(dataTrainModelDir, "2010.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2011<-read.table(paste(dataTrainModelDir, "2011.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2012<-read.table(paste(dataTrainModelDir, "2012.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

train_model = dplyr::bind_rows(data2005, data2006, data2007, data2008, data2009, 
                                data2010, data2011, data2012)

write.csv(file = paste(saveDatasetsDir, "train_model.csv", sep = ""), train_model, row.names=FALSE)

#Data from 2013 up to 2014
dataCvDir = "Data/cv/"

data2013<-read.table(paste(dataCvDir, "2013.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2014<-read.table(paste(dataCvDir, "2014.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

cv = dplyr::bind_rows(data2013, data2014)

write.csv(file = paste(saveDatasetsDir, "cv.csv", sep = ""), cv, row.names=FALSE)

#Data from 2015 up to 2016
dataTestDir = "Data/test/"

data2015<-read.table(paste(dataTestDir, "2015.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)
data2016<-read.table(paste(dataTestDir, "2016.csv", sep = ""), header = T, sep = ",", quote = "",colClasses = "character", stringsAsFactors = FALSE, fill = TRUE)

test = dplyr::bind_rows(data2015, data2016)

write.csv(file = paste(saveDatasetsDir, "test.csv", sep = ""), test, row.names=FALSE)

#allData
allData = dplyr::bind_rows(train_rating, train_model, cv, test)
write.csv(file = paste(saveDatasetsDir, "all_unaltered.csv", sep = ""), allData, row.names=FALSE)