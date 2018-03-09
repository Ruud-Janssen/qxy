rm(list = ls())
source("hyperparametersfunctions.r")
source("HyperParameters/elo/hyperratingfunctions2.r")
library(tictoc)
library(ggplot2)
library(foreach)

Nt <- nrow(read.table("Data/datasets/train_modelWithRatings.csv"
                      , header = T, sep = ",", quote = "\"", fill = TRUE))
set.seed(see)
yt_m <- as.numeric(runif(Nt, 0, 1) > 0.5)

tic()

K <- function(numberOfGames) {
  250 / (numberOfGames + 5) ^ 0.4
}

FiveThreeEightRating <- GetRatings(K)
rangeQ               <- seq(10, 50, 0.25)
totalSteps           <- length(rangeQ)

total <- foreach (q = rangeQ, .combine = rbind, .packages = c("leaps","bestglm", "plyr")) %do% {
  
  message(paste("STEP", match(q, rangeQ),
                "\n of", totalSteps, 
                "\n at", Sys.time(), "\n\n"))
  
  xTrain      <- getratingregressorvariables(yt_m, FiveThreeEightRating)
  xTrain$Date <- ymd(xTrain$Date)
  
  lastGame2011 <- fdTrain_Model_Hyper_Val
  Train        <- xTrain %>% filter(Date <= lastGame2011)
  Validation   <- xTrain %>% filter(Date > lastGame2011)
  
  results <- data.frame(LogLossOutOfSample = rep(0, 20))
  
  
  quantile         <- quantile(Train$Uncertainty, q / 100)
  TrainCurrent     <- removeUncertainMatches(Train, quantile, "")
  TrainCurrentHard <- getXThisSurface(TrainCurrent, "Hard")
  
  ValidationCurrent     <- removeUncertainMatches(Validation, quantile, "")
  ValidationCurrentHard <- getXThisSurface(ValidationCurrent, "Hard")
  
  RegHard = glm(y ~ 0 + ratingHarddiff + ratingdiff, data = TrainCurrentHard, family = binomial)
  
  predictions <- predict(RegHard, ValidationCurrentHard, type = "response")
  

  return(c(q, LogLoss(actual = ValidationCurrentHard$y, predicted = predictions)))
}

toc()

results        <- data.frame(q = total[, 1], LogLoss = total[, 2])
min            <- which.min(results$LogLoss)
bottom2Percent <- which(results$LogLoss < quantile(results$LogLoss, 0.05))

ggplot(results, aes(x = q, y = LogLoss)) + geom_line() +
  geom_point(data = results[bottom2Percent, ], aes(x = q, y = LogLoss), col = "green") + 
  geom_point(aes(x = results[min, 1], y = results[min, 2]), col = "red") + 
  xlab("q") + ylab("Logistic Loss")

ggsave("HyperParameters/uncertainty/uncertainty.png")
write_csv(results, "HyperParameters/uncertainty/uncertainty.csv", col_names = T)