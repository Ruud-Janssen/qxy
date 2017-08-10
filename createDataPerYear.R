rm(list = ls())
library(dplyr)
#install.packages("tidyr")
library(tidyr)
library(stringr)

source("formulas.r")






data2000 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2000, ]
data2001 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2001, ]
data2002 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2002, ]
data2003 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2003, ]
data2004 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2004, ]
data2005 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2005, ]
data2006 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2006, ]
data2007 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2007, ]
data2008 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2008, ]
data2009 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2009, ]
data2010 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2010, ]
data2011 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2011, ]
data2012 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2012, ]
data2013 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2013, ]
data2014 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2014, ]
data2015 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2015, ]
data2016 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2016, ]
data2017 <- allMatches[as.numeric(format(allMatches$Date, '%Y')) == 2017, ]


