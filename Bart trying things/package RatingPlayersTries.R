rm(list = ls())
source("formulas.R")
source("hyperparametersfunctions.R")
library(PlayerRatings)

allGames <- getAllGamesWithoutRating()
allGames <- getXThisSurface(allGames, "Hard")

split <- 21000
allGamesTrain <- allGames[1 : split,  ]
allGamesValidation <- allGames[(split + 1) : nrow(allGames), ]

xTrain <- data.frame(cbind(as.numeric(as.Date(as.character(allGamesTrain$Date), format = "%m/%d/%Y")), 
                          allGamesTrain$idWinner, allGamesTrain$idLoser, rep(1, nrow(allGamesTrain))))
xVal <- data.frame(cbind(as.numeric(as.Date(as.character(allGamesValidation$Date), format = "%m/%d/%Y")), 
                        allGamesValidation$idWinner, allGamesValidation$idLoser, rep(1, nrow(allGamesValidation))))

xTrain <- data.frame(apply(xTrain, 2, as.numeric))
xVal <- data.frame(apply(xVal, 2, as.numeric))


elo <- elo(xTrain, init = 1500, kfac = 25, history = TRUE)
steph <- steph(xTrain, init = c(1500,300), cval = 1, hval = 1, lambda = 0)

mingames = 50

pvals_elo <- predict(elo, xVal, tng = mingames)
pvals_steph <- predict(steph, xVal, tng = mingames)


i = !is.na(pvals_elo)

impOdds = as.numeric(allGamesValidation$PSL)/(as.numeric(allGamesValidation$PSL) + as.numeric(allGamesValidation$PSW))
impOdds = impOdds[i]
pvals_elo = pvals_elo[i]
pvals_steph = pvals_steph[i]

j = !is.na(impOdds)

impOdds = impOdds[j]
pvals_elo = pvals_elo[j]
pvals_steph = pvals_steph[j]

LogLoss(pvals_elo, rep(1, length(pvals_elo)))
LogLoss(pvals_steph, rep(1, length(pvals_elo)))
LogLoss(impOdds, rep(1, length(pvals_elo)))
