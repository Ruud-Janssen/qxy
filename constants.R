# constants
##Paths
dir_result_datasets <- "Data/datasets/"

dir_source_ATP <- "Data/source_data_ATP/"

dir_source_Sackmann             <- "Data/source_data_Sackmann/"
dir_source_Sackmann_ATP         <- paste(dir_source_Sackmann, "atp/", sep="")
dir_source_Sackmann_Challengers <- paste(dir_source_Sackmann, "challengers/", sep="")

# dir_source_Sackmann_Futures <- paste(dir_source_Sackmann + "futures/")

##Dates
DF <- "%Y-%m-%d"

fdTrain_Rating          <- as.Date("2004-12-31", DF)
fdTrain_Model_Hyper_Val <- as.Date("2011-12-31", DF)
fdTrain_Model           <- as.Date("2012-12-31", DF)
fdVal                   <- as.Date("2014-12-31", DF)
fdTest                  <- as.Date("2016-12-31", DF)

##Seed
seed <- 42
