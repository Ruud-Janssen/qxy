library(dplyr)

source("formulas.R")
source("constants.R")

# first level
all_atp_matches <- getDatasets(dir_result_datasets, "all_atp_matches", lvl = "lvl1", change_datatype = FALSE)

##Number
message(paste("In the atp dataset there are", count(all_atp_matches), "matches \n"))

# after matching
all_Sackmann_matches <- getDatasets(dir_result_datasets, "all_Sackmann_matches", lvl = "lvl3", change_datatype = TRUE)
message(paste("In the Sackmann dataset there are", count(all_Sackmann_matches), "matches"))
count_surface <- all_Sackmann_matches %>% count(Surface)
apply(count_surface, 1, function(x) {message(paste("with", x[2], "matches for surface", x[1]))})
message(paste("a total of", sum(all_Sackmann_matches$matched == "1", na.rm = T), "are matched with the atp dataset \n", 
              "a number of", sum(all_Sackmann_matches$w_svpt != ""), "have point data and the other", sum(all_Sackmann_matches$w_svpt == ""),
              "matches miss this data"))


