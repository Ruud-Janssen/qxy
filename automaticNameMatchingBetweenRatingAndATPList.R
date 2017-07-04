#Names Matching automatical approach
#This was not particularly successful, but still it can be used
#assumption is that the data for rating and atp players are loaded
#all new names get the 2 behind it to not mess it up


#First change names in rating set
convertNames = function(originalName){
  name <- unlist(strsplit(originalName, split = " (?=[^ ]+$)", perl=TRUE))
  # lastName = fullname[1]
  # firstName = fullname[2]
  
  fullName <- paste(name[2], name[1], sep = " ")
  
  return (fullName)
}

rating$fullName2 <- unlist(lapply(as.character(rating$Players), convertNames))


#second change names in atp_players set

#already done:
#atp_players = read.table("Data/datasets/atp_players.csv", header = T, sep = ",", 
#                           quote = "\"", fill = TRUE)

  atp_players$firstName2 <- as.character(atp_players$firstName)
  atp_players$lastName2 <- as.character(atp_players$lastName)
  
  atp_players$initial2 <- substring(atp_players$firstName2, 1, 1) 
  
  atp_players$fullName2 <- paste(atp_players$initial2, atp_players$lastName2, sep = ". ")

    
#Combine names of tennis players from 2 datasets
rating$result2 <- data.frame(rating$fullName2, atp_players$fullName2 [max.col(-adist(rating$fullName2,atp_players$fullName2))])
  
#Show differences
ratingdiff <- subset(rating, toupper(as.character(rating$result2$rating.fullName2)) != toupper(as.character(rating$result2$atp_players.fullName2.max.col..adist.rating.fullName2..atp_players.fullName2...)))


