rm(list = ls())
source("formulas.R")
source("hyperparametersfunctions.R")
library(PlayerRatings)

allGames <- getAllGamesWithoutRating()
allGames <- getXThisSurface(allGames, "Hard")
allGames <- allGames[!is.na(allGames$Date), ]
allGames$Date <- as.Date(allGames$Date, format = "%m/%d/%Y")
allGames <- arrange(allGames, Date)
allGames <- RemoveWalkOvers(allGames)
allGames$Date <- format(as.Date(allGames$Date, format = "%m/%d/%Y"), "%Y/%m")
allGames$Date <- group_indices_(allGames, .dots = c("Date"))

splitDate <- 20887
allGamesTrain <- allGames[1 : 20887,  ]
allGamesValidation <- allGames[(20887 + 1): nrow(allGames), ]

xTotal <- data.frame(cbind(allGames$Date), allGames$idWinner, allGames$idLoser, rep(1, nrow(allGames)))
xTrain <- data.frame(cbind(allGamesTrain$Date), allGamesTrain$idWinner, allGamesTrain$idLoser, 
                     rep(1, nrow(allGamesTrain)))
xVal <- data.frame(cbind(allGamesValidation$Date), allGamesValidation$idWinner, allGamesValidation$idLoser, 
                   rep(1, nrow(allGamesValidation)))

xTotal <- data.frame(apply(xTotal, 2, as.numeric))
xTrain <- data.frame(apply(xTrain, 2, as.numeric))
xVal <- data.frame(apply(xVal, 2, as.numeric))

K <- 16.8
c <- 2.5
h <- 2.5
l <- 0

elo <- elo(xTrain, init = 1500, kfac = K, history = TRUE, gamma = 0)
glicko <- glicko(xTrain, init = c(1500,300), cval = c, history = TRUE, gamma = 0)
steph <- steph(xTrain, init = c(1500,300), cval = c, hval = h, lambda = l, history = TRUE, gamma = 0)

minGames <- 150

uniqueDates <- unique(xVal$cbind.allGamesValidation.Date.)
pvals_elo <- rep(0, 0)
pvals_glicko <- rep(0,0)
pvals_steph <-  rep(0, 0)

for (i in uniqueDates) {
  batch <- xVal[xVal$cbind.allGamesValidation.Date. == i, ] 
  pvals_elo <- c(pvals_elo, predict(elo, batch, tng = minGames, gamma = 0))
  pvals_glicko <- c(pvals_glicko, predict(glicko, batch, tng = minGames, gamma = 0))
  pvals_steph <- c(pvals_steph, predict(steph, batch, tng = minGames, gamma = 0))
  
  print(table(table(c(batch$allGamesValidation.idWinner, batch$allGamesValidation.idLoser))))

  
  elo <- elo(batch, init = 1500, kfac = K, history = TRUE, gamma = 0, status = elo$ratings)
  glicko <- glicko(batch, init = c(1500,300), cval = c, history = TRUE, gamma = 0, status = glicko$ratings)
  steph <- steph(batch, init = c(1500,300), cval = c, hval = h, lambda = l, 
                 history = TRUE, gamma = 0, status = steph$ratings)
}

#pvals_elo <- predict(elo, xVal, tng = minGames, gamma = 0)
#pvals_steph <- predict(steph, xVal, tng = minGames, gamma = 0)

i <- !is.na(pvals_elo)

impOdds <- as.numeric(allGamesValidation$PSL)/(as.numeric(allGamesValidation$PSL) + as.numeric(allGamesValidation$PSW))
impOdds <- impOdds[i]
pvals_elo <- pvals_elo[i]
pvals_glicko <- pvals_glicko[i]
pvals_steph <- pvals_steph[i]

j = !is.na(impOdds)

impOdds <- impOdds[j]
pvals_elo <- pvals_elo[j]
pvals_glicko <- pvals_glicko[j]
pvals_steph <- pvals_steph[j]

LogLoss(rep(1, length(pvals_elo)), pvals_elo)
LogLoss(rep(1, length(pvals_elo)), pvals_glicko)
LogLoss(rep(1, length(pvals_elo)), pvals_steph)
LogLoss(rep(1, length(pvals_elo)), impOdds)
