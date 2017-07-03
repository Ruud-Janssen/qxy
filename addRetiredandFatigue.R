rm(list = ls())
source("D:/Betting/Tennis/formulas.r")
source("D:/Betting/Tennis/retiredformulas.r")

train_model = read.table("D:/Betting/Tennis/Data/train_model.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)
train_model = CreateRetiredWalkoverAndFatigue(train_model)
write.csv(file = "D:/Betting/Tennis/Data/train_model.csv", 
          train_model, row.names=FALSE)

cv = read.table("D:/Betting/Tennis/Data/cv.csv"
                , header = T, sep = ",", quote = "\"", fill = TRUE)
cv = CreateRetiredWalkoverAndFatigue(cv)
write.csv(file = "D:/Betting/Tennis/Data/cv.csv", 
          cv, row.names=FALSE)

Test = read.table("D:/Betting/Tennis/Data/test.csv"
                  , header = T, sep = ",", quote = "\"", fill = TRUE)
Test = CreateRetiredWalkoverAndFatigue(Test)
write.csv(file = "D:/Betting/Tennis/Data/test.csv", 
          Test, row.names=FALSE)
  