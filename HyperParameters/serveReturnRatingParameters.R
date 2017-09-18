rm(list = ls())
source("hyperparametersfunctions.r")
source("HyperParameters/hyperratingfunctions2.r")
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
total <- foreach (k = 55 : 65, .combine = rbind, .packages = c("leaps","bestglm", "plyr")) %do% {
  constant <- k
  offset   <- 0.1
  power    <- 0.1
  
  return(foreach(wb = 6 : 10, .packages = c("leaps","bestglm", "lubridate"), .combine = rbind) %dopar% {
    source("hyperparametersfunctions.r")
    source("HyperParameters/hyperservereturnratingfunctionsSquared.r")
    winbonus = wb / 100
  
    train_model <- GetServeReturnRatings(offset, power, constant, winbonus)
    xTrain      <- regressorvariables(yt_m, train_model)
    xTrain$Date <- ymd(xTrain$Date)

    lastGame2011 <- ymd("2011-12-31")
    Train        <- xTrain %>% filter(Date <= lastGame2011)
    Validation   <- xTrain %>% filter(Date > lastGame2011)

    results                    <- as.data.frame(matrix(0, 20))
    results$LogLossOutOfSample <- 0
    
    for(q in 1 : 20) {
      
      quantile         <- quantile(Train$Uncertainty, (q + 19) / 100)
      TrainCurrent     <- removeUncertainMatches(Train, quantile, "")
      TrainCurrentHard <- getXThisSurface(TrainCurrent, "Hard")
      
      ValidationCurrent     <- removeUncertainMatches(Validation, quantile, "")
      ValidationCurrentHard <- getXThisSurface(ValidationCurrent, "Hard")
      
      regServeReturnRating <- glm(y ~ 0 + ratingservereturndiff + ratingservereturnHarddiff 
                                  , data = TrainCurrentHard, family = binomial)
      
      
      predictions <- predict(regServeReturnRating, ValidationCurrentHard, type = "response")
      
      results$LogLossOutOfSample[q] <- LogLoss(actual = ValidationCurrentHard$y, predicted = predictions)   
    }
    return(c(constant, winbonus, mean(results$LogLossOutOfSample)))
  })
}
stopCluster(cl)
toc()



results <- data.frame(K = total[, 1], winBonus = total[, 2], LogLoss = total[, 3])
ggplot(results, aes(x = K, y = winBonus, z = LogLoss)) + geom_raster(aes(fill = LogLoss)) +
  geom_contour(colour = "white", bins = 30)