rm(list = ls())
source("hyperparametersfunctions.r")
library(leaps)
library(bestglm)
library(tictoc)
library(ggplot2)

#Parallel
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 3)
registerDoParallel(cl, cores = (detectCores() - 3))

Nt <- nrow(read.table("Data/datasets/train_modelWithRatings.csv"
                     , header = T, sep = ",", quote = "\"", fill = TRUE))
set.seed(42)
yt_m <- as.numeric(runif(Nt, 0, 1) > 0.5)

tic()
variablesStepA = seq(14, 50, 3)
totalSteps = length(variablesStepA)
total <- foreach (Bf = variablesStepA, .combine = rbind, .packages = c("leaps","bestglm", "plyr")) %do% {
  Bp     <- 1 / 400
  Bf     <- Bf
  message("step")
  message(match(Bf,variablesStepA))
  message("of")
  message(totalSteps)
  message("at")
  message(Sys.time())
  
  return(foreach(s = seq(14.5, 20.2, 0.3), .packages = c("leaps","bestglm", "lubridate"), .combine = rbind) %dopar% {
    source("hyperparametersfunctions.r")
    source("HyperParameters/servereturnbartofunctions.r")
    s    <- s
    ratingGainForWin <- 0
  
    train_model <- GetServeReturnBarto(Bp, Bf, s, ratingGainForWin)
    xTrain      <- regressorvariables(yt_m, train_model)
    xTrain$Date <- ymd(xTrain$Date)

    lastGame2011 <- ymd("2011-12-31")
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