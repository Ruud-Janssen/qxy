library(plyr)

removeUncertainMatches = function(x, edge, uncertaintyParameter){
  if(uncertaintyParameter == "COSurface") {
    index_keep = (x$UncertaintyCOSurface < edge)
  } else if(uncertaintyParameter == "Surface"){
    index_keep = (x$UncertaintySurface < edge)
  } else {
    index_keep = (x$Uncertainty < edge)
  }
  x = x[index_keep, ]
  return(x)
}

getXThisSurface = function(x, surface){
  indexSurface = (x$Surface == surface)
  x = x[indexSurface, ]
  return(x)
}

regressorvariables = function(y, data) {
  
  data$y = y
  returnData = adply(data, 1, addRegressorVariableRow)
  
  returnData$Uncertainty = data$Uncertainty
  returnData$Uncertainty2 = data$Uncertainty2
  returnData$Surface = data$Surface
  returnData$Court = data$Court
  
  returnData$y = y
  
  return(returnData)
}

addRegressorVariableRow = function(row){
  x = data.frame(matrix(nrow = 1))
  x = setPointOfViewVariables(x, row)
  #x = setOtherVariables(x, row)
}

setPointOfViewVariables = function(x, row) {
  if(row$y == 1){
    mp = 1
  } else {
    mp = -1
  }
  
  #x$PSW   = row$y * row$PSW + (1 - row$y) * row$PSL
  #x$PSL   = row$y * row$PSL + (1 - row$y) * row$PSW
  #x$B365W = row$y * row$B365W + (1 - row$y) * row$B365L
  #x$B365L = row$y * row$B365L + (1 - row$y) * row$B365W
  
  x$ratingdiff        = mp * (row$Winner_rating - row$Loser_rating)
  x$ratingClaydiff    = mp * (row$Winner_ratingClay - row$Loser_ratingClay)
  x$ratingHarddiff    = mp * (row$Winner_ratingHard - row$Loser_ratingHard)
  x$ratingGrassdiff   = mp * (row$Winner_ratingGrass - row$Loser_ratingGrass)
  #x$ratingNotHarddiff = mp * (row$Winner_ratingNotHard - row$Loser_ratingNotHard)
  #x$ratingBo3diff     = mp * (row$Winner_ratingBo3 - row$Loser_ratingBo3)
  #x$ratingBo5diff     = mp * (row$Winner_ratingBo5 - row$Loser_ratingBo5)
  
  #x$RetiredDiff           = mp * (row$Winner_retired_last_game - row$Loser_retired_last_game)
  #x$WalkoverDiff          = mp * (row$Winner_walkover_last_game - row$Loser_walkover_last_game)
  #x$RetiredOrWalkoverDiff = x$RetiredDiff + x$WalkoverDiff
  #x$FatigueDiff           = mp * (row$Winner_fatigue - row$Loser_fatigue)
  #x$HeadtoHead            = mp * (row$HeadtoHead)
  #x$LastHeadtoHead        = mp * row$LastHeadtoHead
  #x$HomeDiff              = mp * (row$WinnerisHome - row$LoserisHome)
  
  #if(row$HeadtoHeadMatches != 0) {
  #  x$HeadtoHeadPercentageWeightedsqN =  mp * (((row$HeadtoHead + 0.5 * row$HeadtoHeadMatches) / 
  #                                                (row$HeadtoHeadMatches) - 0.5) * row$HeadtoHeadMatches ^ 0.5) 
  #} else {
  #  x$HeadtoHeadPercentageWeightedsqN = 0  
  #}
  
  #if(row$Best.of == 3){
  #  x$ThisBoxSkillDiff           = mp * (row$Winner_skillBo3 - row$Loser_skillBo3)
  #  x$ThisBoxSkillDiffPlusScores = mp * (row$Winner_skillBo3PlusScores - row$Loser_skillBo3PlusScores)
  #  x$ThisBoxSkillRatingMethod   = mp * ((row$Winner_ratingBo5 - row$Winner_ratingBo3) - 
  #                                         (row$Loser_ratingBo5 - row$Loser_ratingBo3))
  #} else {
  #  x$ThisBoxSkillDiff           = mp * (row$Winner_skillBo5 - row$Loser_skillBo5)
  #  x$ThisBoxSkillDiffPlusScores = mp * (row$Winner_skillBo5PlusScores - row$Loser_skillBo5PlusScores)
  #  x$ThisBoxSkillRatingMethod   = mp * ((row$Winner_ratingBo3 - row$Winner_ratingBo5) - 
  #                                         (row$Loser_ratingBo3 - row$Loser_ratingBo5))
  #}
  #x$recentGamesDiff = mp * (row$Winner_recentGames - row$Loser_recentGames)
  
  #x$COPercentMatchesWonDiff   = mp * (row$Winner_COPercentMatchesWon -  row$Loser_COPercentMatchesWon )
  #x$COPercentSetsDiff         = mp * (row$Winner_COPercentSetsWon - row$Loser_COPercentSetsWon)
  #x$COPercentGamesDiff        = mp * (row$Winner_COPercentGamesWon - row$Loser_COPercentGamesWon) 
  #x$COPercentPointsDiff       = mp * (row$Winner_COPercentPointsWon - row$Loser_COPercentPointsWon)
  #x$COPercentCompletenessDiff =
  #  mp * (row$Winner_COPercentPointsWon * row$Winner_COPercentGamesWon - 
  #          row$Loser_COPercentPointsWon * row$Loser_COPercentGamesWon)
  
  #x$COPercentMatchesWonThisSurfaceDiff = 
  #  mp * (row$Winner_COPercentMatchesThisSurfaceWon -  row$Loser_COPercentMatchesThisSurfaceWon )
  #x$COPercentSetsThisSurfaceDiff       = 
  #  mp * (row$Winner_COPercentSetsThisSurfaceWon - row$Loser_COPercentSetsThisSurfaceWon)
  #x$COPercentGamesThisSurfaceDiff      = 
  #  mp * (row$Winner_COPercentGamesThisSurfaceWon - row$Loser_COPercentGamesThisSurfaceWon) 
  #x$COPercentPointsThisSurfaceDiff     = 
  #  mp * (row$Winner_COPercentPointsThisSurfaceWon - row$Loser_COPercentPointsThisSurfaceWon)
  #x$COPercentCompletenessThisSurfaceDiff = 
  #  mp * (row$Winner_COPercentPointsThisSurfaceWon * row$Winner_COPercentGamesThisSurfaceWon - 
  #          row$Loser_COPercentPointsThisSurfaceWon * row$Loser_COPercentGamesThisSurfaceWon)
  
  return(x)
}

setOtherVariables = function(x, row){
  x$DummyClay                = as.numeric(row$Surface == "Clay")
  x$DummyGrass               = as.numeric(row$Surface == "Grass")
  x$DummyHard                = as.numeric(row$Surface == "Hard")
  x$ratingdiffCurrentSurface = x$DummyClay * x$ratingClaydiff + x$DummyGrass * x$ratingGrassdiff + 
    x$DummyHard * x$ratingHarddiff
  
  if(row$Best.of == 5){
    x$DummyBo5                   = 1
    x$DummyBo5TimesRatingdiff    = x$DummyBo5 * x$ratingdiff
    x$DummyBo5TimesAvgRatingdiff = 0.5 * (x$ratingdiff + x$ratingdiffCurrentSurface)
  } else {
    x$DummyBo5                   = 0
    x$DummyBo5TimesRatingdiff    = 0
    x$DummyBo5TimesAvgRatingdiff = 0
  }
  
  if(x$DummyBo5TimesRatingdiff > 0) {
    x$DummyBo5TimesRatingdiff_5SameSign      = x$DummyBo5TimesRatingdiff ^ 0.5
    x$DummyBo5TimesRatingdiffSquaredSameSign = x$DummyBo5TimesRatingdiff ^ 2
    x$DummyBo5TimesRatingdiff1_5SameSign     = x$DummyBo5TimesRatingdiff ^ 1.5
    x$DummyBo5TimesRatingdiffthirdSameSign   = x$DummyBo5TimesRatingdiff ^ 3
  } else {
    x$DummyBo5TimesRatingdiffSquaredSameSign = - (x$DummyBo5TimesRatingdiff) ^ 2
    x$DummyBo5TimesRatingdiff_5SameSign      = -(abs(x$DummyBo5TimesRatingdiff)) ^ 0.5
    x$DummyBo5TimesRatingdiff1_5SameSign     = -(abs(x$DummyBo5TimesRatingdiff)) ^ 1.5
    x$DummyBo5TimesRatingdiffthirdSameSign   = -(abs(x$DummyBo5TimesRatingdiff)) ^ 3
  }
  
  x$FatigueDiffTimesBo5 = x$DummyBo5 * x$FatigueDiff
  
  if(row$Winner_COGames > 0 & row$Loser_COGames > 0) {
    x$UncertaintyCO = 1 / (row$Winner_COGames * row$Loser_COGames) 
  } else {
  x$UncertaintyCO = 2
  }
  
  if(row$Winner_COThisSurfaceGames > 0 & row$Loser_COThisSurfaceGames > 0) {
    x$UncertaintyCOSurface = 1 / (row$Winner_COThisSurfaceGames * row$Loser_COThisSurfaceGames) 
  } else {
    x$UncertaintyCOSurface = 2
  }
  
  return(x)
}

cvpredictions = function(results, Reg, xcv, ycv, q) {

  cvpred = predict(Reg, xcv, type = "response")
  Npred = length(cvpred)
  
  bets = as.data.frame(matrix(nrow = Npred, ncol = 0))
  bets$result = rep(0, Npred)
  bets$bet = rep(0, Npred)
  bets$br = rep(1, Npred)
  #Unfortunately, there appears to be a big upset in the beginning of the rowset, causing bad ROI results
  for(i in 1 : Npred) {
    winexpectation = cvpred[i]
    lossexpectation = 1 - cvpred[i]
    if(!is.na(xcv$PSL[i]) & !is.na(xcv$PSW[i])){# & results$Br[q] > 0) {
      if(winexpectation * xcv$PSW[i] - 1 > 0.05) {
        results$Nrbets[q] = results$Nrbets[q] + 1
        bets$bet[i] = (winexpectation * xcv$PSW[i] - 1) / (xcv$PSW[i] - 1)
        #WHY DOES THIS NOT WORK 
        #bets$bet[i] = 1/4 * results$Br[q] * bets$bet[i]
        #bet won
        if(ycv[i] == 1 ) {
          bets$result[i] = bets$bet[i]*(xcv$PSW[i] - 1) 
          #bet loss
        } else {
          bets$result[i] = -bets$bet[i]
        }
      } else if(lossexpectation * xcv$PSL[i] - 1 > 0.05) {
        results$Nrbets[q] = results$Nrbets[q] + 1
        bets$bet[i] = (lossexpectation * xcv$PSL[i] - 1) / (xcv$PSL[i] - 1)
        #WHY DOES THIS NOT WORK? 
        #bets$bet[i] =  1/4 * results$Br[q] * bets$bet[i]
        #bet won
        if(ycv[i] == 0){
          bets$result[i] = bets$bet[i] * (xcv$PSL[i] - 1)
        } else {
          bets$result[i] = -bets$bet[i]
        } 
      }
    }
    if( i > 1) {
      bets$br[i] = bets$br[i - 1] + bets$result[i]
    } else {
      bets$br[i] = 1 + bets$result[i]
    }
  }
  results$LogLossOutOfSample[q] = LogLoss(cvpred, ycv)
  
  results$Br[q] = bets$br[Npred]
  results$ROI[q] = sum(bets$result) / sum(bets$bet)
  
  results$
    ple[q] = LogLoss(cvpred, ycv)
  
  clay = (xcv$Surface == "Clay")
  if(sum(clay) > 0) {
    results$BrClay[q] = bets$br[Npred]
    results$LogLossOutOfSampleClay[q] = LogLoss(cvpred[clay], ycv[clay])
  }
  
  grass = (xcv$Surface == "Grass")
  if(sum(grass) > 0) {
    results$BrGrass[q] = bets$br[Npred]
    results$LogLossOutOfSampleGrass[q] = LogLoss(cvpred[grass], ycv[grass])
  }
  
  hard = (xcv$Surface == "Hard")
  if(sum(hard) > 0) {
    results$BrHard[q] = bets$br[Npred]
    results$LogLossOutOfSampleHard[q] = LogLoss(cvpred[hard], ycv[hard])
  }
  
  return(results)
}


