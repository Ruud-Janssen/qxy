rm(list = ls())
source("hyperparametersfunctions.r")
source("HyperParameters/barto/servereturnbartofunctions.r")
library(leaps)
library(bestglm)
library(tictoc)
library(ggplot2)

#Parallel
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 4)
registerDoParallel(cl, cores = (detectCores() - 4))

Nt <- nrow(read.table("Data/datasets/train_modelWithRatings.csv"
                     , header = T, sep = ",", quote = "\"", fill = TRUE))
set.seed(42)
yt_m <- as.numeric(runif(Nt, 0, 1) > 0.5)

tic()
gamesWithApproximateScores <- getTrainDataSetsWithRating()
variablesStepA             <- seq(20, 40, 2)
totalSteps                 <- length(variablesStepA)
total <- foreach (Bf = variablesStepA, .combine = rbind, .packages = c("leaps","bestglm", "plyr")) %do% {
  Bp     <- 1 / 400
  Bf     <- Bf
  message(paste("STEP", match(Bf,variablesStepA),
                "\n of", totalSteps, 
                "\n at", Sys.time(), "\n\n"))
  
  return(foreach(s = seq(16.5, 18.3, 0.2), .packages = c("leaps","bestglm", "lubridate"), .combine = rbind) %dopar% {
    source("hyperparametersfunctions.r")
    source("HyperParameters/barto/servereturnbartofunctions.r")
    s    <- s
    ratingGainForWin <- 0
  
    gamesWithApproximateScores$Date <- ymd(gamesWithApproximateScores$Date) 
    
    train_model <- GetServeReturnBartoTrain_model(gamesWithApproximateScores, Bp, Bf, s, ratingGainForWin)
    xTrain      <- regressorvariables(yt_m, train_model)
    xTrain$Date <- ymd(xTrain$Date)

    lastGame2011 <- fdTrain_Model_Hyper_Val
    Train        <- xTrain %>% filter(Date <= lastGame2011)
    Validation   <- xTrain %>% filter(Date > lastGame2011)

    results <- data.frame(LogLossOutOfSample = rep(0, 20))
    
    for(q in 1 : 20) {
      
      quantile         <- quantile(Train$Uncertainty, (q + 19) / 100)
      TrainCurrent     <- removeUncertainMatches(Train, quantile, "")
      TrainCurrentHard <- getXThisSurface(TrainCurrent, "Hard")
      
      ValidationCurrent     <- removeUncertainMatches(Validation, quantile, "")
      ValidationCurrentHard <- getXThisSurface(ValidationCurrent, "Hard")
      
      regServeReturnRating <- glm(y ~ 0 + bartoservereturndiff + bartoservereturnHarddiff 
                                  , data = TrainCurrentHard, family = binomial)
      
      predictions <- predict(regServeReturnRating, ValidationCurrentHard, type = "response")
      
      results$LogLossOutOfSample[q] <- LogLoss(actual = ValidationCurrentHard$y, predicted = predictions)   
    }
    return(c(Bf, s, mean(results$LogLossOutOfSample)))
  })
}
stopCluster(cl)
toc()

results        <- data.frame(Bf = total[, 1], s = total[, 2], LogLoss = total[, 3])
min            <- which.min(results$LogLoss)
bottom2Percent <- which(results$LogLoss < quantile(results$LogLoss, 0.05))

ggplot(results, aes(x = Bf, y = s, z = LogLoss)) + geom_raster(aes(fill = LogLoss)) +
  geom_contour(colour = "white", bins = 5)  + 
  geom_point(data = results[bottom2Percent, ], aes(x = Bf, y = s), col = "green") + 
  geom_point(aes(x = results[min, 1], y = results[min, 2]), col = "red")