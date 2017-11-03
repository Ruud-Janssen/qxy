library(tidyverse)

removeUncertainMatches <- function(x, edge, uncertaintyParameter){
  if(uncertaintyParameter == "COSurface") {
    index_keep <- (x$UncertaintyCOSurface < edge)
  } else if(uncertaintyParameter == "Surface"){
    index_keep <- (x$UncertaintySurface < edge)
  } else {
    index_keep <- (x$Uncertainty < edge)
  }
  x <- x[index_keep, ]
  return(x)
}

getXThisSurface <- function(x, surface){
  x <- x %>% filter(Surface == surface)
}

regressorvariables <- function(y, data) {
  
  data <- data %>% mutate(mp = ifelse(y == 1, 1, -1),
                          y = y, 
                          Bo5 = as.numeric(Best.of == 5), 
                          ratingdiff = mp * (Winner_rating - Loser_rating),
                          ratingClaydiff = mp * (Winner_ratingClay - Loser_ratingClay),
                          ratingHarddiff = mp * (Winner_ratingHard - Loser_ratingHard),
                          ratingGrassdiff = mp * (Winner_ratingGrass - Loser_ratingGrass),
                          ratingNotHarddiff = mp * (Winner_ratingNotHard - Loser_ratingNotHard),
                          
                          ratingservereturndiff = mp * (Winner_serverating + Winner_returnrating -
                                                          Loser_serverating - Loser_returnrating) / 2, 
                          ratingservereturnHarddiff = mp * (Winner_serveratingHard + Winner_returnratingHard -
                                                              Loser_serveratingHard - Loser_returnratingHard) / 2,
                          
                          bartoservereturndiff = mp * (Winner_servebarto + Winner_returnbarto - 
                                                         Loser_servebarto - Loser_returnbarto) / 2,
                          bartoservereturnHarddiff = mp * (Winner_servebartoHard + Winner_returnbartoHard - 
                                                         Loser_servebartoHard - Loser_returnbartoHard) / 2,
                          
                          RetiredDiff            = mp * (Winner_retired_last_game - Loser_retired_last_game),
                          WalkoverDiff           = mp * (Winner_walkover_last_game - Loser_walkover_last_game),
                          FatigueDiff            = mp * (Winner_fatigue - Loser_fatigue),
                          HeadtoHeaddiff         = mp * (HeadtoHead),
                          
                          COPercentMatchesWonDiff   = mp * (Winner_COPercentMatchesWon -  Loser_COPercentMatchesWon ),
                          COPercentSetsDiff         = mp * (Winner_COPercentSetsWon - Loser_COPercentSetsWon),
                          COPercentGamesDiff        = mp * (Winner_COPercentGamesWon - Loser_COPercentGamesWon), 
                          COPercentPointsDiff       = mp * (Winner_COPercentPointsWon - Loser_COPercentPointsWon),
                          COPercentCompletenessDiff =
                            mp * (Winner_COPercentPointsWon * Winner_COPercentGamesWon - 
                                    Loser_COPercentPointsWon * Loser_COPercentGamesWon),
                          
                          COPercentMatchesWonThisSurfaceDiff = 
                            mp * (Winner_COPercentMatchesThisSurfaceWon -  Loser_COPercentMatchesThisSurfaceWon ),
                          COPercentSetsThisSurfaceDiff       = 
                            mp * (Winner_COPercentSetsThisSurfaceWon - Loser_COPercentSetsThisSurfaceWon),
                          COPercentGamesThisSurfaceDiff      = 
                            mp * (Winner_COPercentGamesThisSurfaceWon - Loser_COPercentGamesThisSurfaceWon), 
                          COPercentPointsThisSurfaceDiff     = 
                            mp * (Winner_COPercentPointsThisSurfaceWon - Loser_COPercentPointsThisSurfaceWon),
                          COPercentCompletenessThisSurfaceDiff = 
                            mp * (Winner_COPercentPointsThisSurfaceWon * Winner_COPercentGamesThisSurfaceWon - 
                                    Loser_COPercentPointsThisSurfaceWon * Loser_COPercentGamesThisSurfaceWon)
                          )
  
  data %>% select(y, Surface, Bo5, Date, ratingdiff, ratingClaydiff, ratingHarddiff, ratingGrassdiff, ratingNotHarddiff, 
                  ratingservereturndiff, ratingservereturnHarddiff, bartoservereturndiff, bartoservereturnHarddiff, RetiredDiff, 
                  WalkoverDiff, FatigueDiff, HeadtoHeaddiff, COPercentMatchesWonDiff, COPercentSetsDiff, COPercentGamesDiff, 
                  COPercentPointsDiff, COPercentCompletenessDiff, COPercentMatchesWonThisSurfaceDiff, COPercentSetsThisSurfaceDiff, 
                  COPercentGamesThisSurfaceDiff, COPercentPointsThisSurfaceDiff, COPercentCompletenessThisSurfaceDiff,
                  Uncertainty, Uncertainty2, UncertaintySurface)
}

cvpredictions = function(results, Reg, xcv, ycv, q) {

  cvpred <- predict(Reg, xcv, type = "response")
  Npred  <- length(cvpred)
  
  bets        <- as.data.frame(matrix(nrow = Npred, ncol = 0))
  bets$result <- rep(0, Npred)
  bets$bet    <- rep(0, Npred)
  bets$br     <- rep(1, Npred)
  #Unfortunately, there appears to be a big upset in the beginning of the rowset, causing bad ROI results
  for(i in 1 : Npred) {
    winexpectation  <- cvpred[i]
    lossexpectation <- 1 - cvpred[i]
    if(!is.na(xcv$PSL[i]) & !is.na(xcv$PSW[i])){# & results$Br[q] > 0) {
      if(winexpectation * xcv$PSW[i] - 1 > 0.05) {
        results$Nrbets[q] <- results$Nrbets[q] + 1
        bets$bet[i]       <- (winexpectation * xcv$PSW[i] - 1) / (xcv$PSW[i] - 1)
        #WHY DOES THIS NOT WORK 
        #bets$bet[i] = 1/4 * results$Br[q] * bets$bet[i]
        #bet won
        if(ycv[i] == 1 ) {
          bets$result[i] <- bets$bet[i]*(xcv$PSW[i] - 1) 
          #bet loss
        } else {
          bets$result[i] <- -bets$bet[i]
        }
      } else if(lossexpectation * xcv$PSL[i] - 1 > 0.05) {
        results$Nrbets[q] <- results$Nrbets[q] + 1
        bets$bet[i]       <- (lossexpectation * xcv$PSL[i] - 1) / (xcv$PSL[i] - 1)
        #WHY DOES THIS NOT WORK? 
        #bets$bet[i] =  1/4 * results$Br[q] * bets$bet[i]
        #bet won
        if(ycv[i] == 0){
          bets$result[i] <- bets$bet[i] * (xcv$PSL[i] - 1)
        } else {
          bets$result[i] <- -bets$bet[i]
        } 
      }
    }
    if( i > 1) {
      bets$br[i] <- bets$br[i - 1] + bets$result[i]
    } else {
      bets$br[i] <- 1 + bets$result[i]
    }
  }
  results$LogLossOutOfSample[q] <- LogLoss(cvpred, ycv)
  
  results$Br[q]  <- bets$br[Npred]
  results$ROI[q] <- sum(bets$result) / sum(bets$bet)
  
  results$
    ple[q] <- LogLoss(cvpred, ycv)
  
  clay <- (xcv$Surface == "Clay")
  if(sum(clay) > 0) {
    results$BrClay[q]                 <- bets$br[Npred]
    results$LogLossOutOfSampleClay[q] <- LogLoss(cvpred[clay], ycv[clay])
  }
  
  grass <- (xcv$Surface == "Grass")
  if(sum(grass) > 0) {
    results$BrGrass[q]                 <- bets$br[Npred]
    results$LogLossOutOfSampleGrass[q] <- LogLoss(cvpred[grass], ycv[grass])
  }
  
  hard <- (xcv$Surface == "Hard")
  if(sum(hard) > 0) {
    results$BrHard[q]                 <- bets$br[Npred]
    results$LogLossOutOfSampleHard[q] <- LogLoss(cvpred[hard], ycv[hard])
  }
  
  return(results)
}