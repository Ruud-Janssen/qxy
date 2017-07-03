rm(list = ls())
source("D:/Betting/Tennis/formulas.r")
source("D:/Betting/Tennis/retiredformulas.r")

train_model = read.table("Data/datasets/train_model.csv"
                                    , header = T, sep = ",", quote = "\"", fill = TRUE)
train_model = CreateRetiredWalkoverAndFatigue(train_model)
write.csv(file = "Data/datasets/train_model.csv", 
          train_model, row.names=FALSE)

cv = read.table("Data/datasets/cv.csv"
                , header = T, sep = ",", quote = "\"", fill = TRUE)
cv = CreateRetiredWalkoverAndFatigue(cv)
write.csv(file = "Data/datasets/cv.csv", 
          cv, row.names=FALSE)

Test = read.table("Data/datasets/test.csv"
                  , header = T, sep = ",", quote = "\"", fill = TRUE)
Test = CreateRetiredWalkoverAndFatigue(Test)
write.csv(file = "Data/datasets/test.csv", 
          Test, row.names=FALSE)
  