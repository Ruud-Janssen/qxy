library(plyr)

removeUncertainMatches = function(x, edge){
  index_keep = (x$Uncertainty < edge)
  x = x[index_keep, ]
  return(x)
}

getXThisSurface = function(x, surface){
  indexSurface = (x$Surface == surface)
  x = x[indexSurface, ]
  return(x)
}


addRegressorVariableRow = function(row){
  x = data.frame(matrix(nrow = 1))
  
  #winner's viewpoint
  if(row$y == 1){
    x$PSWthisplayer = row$PSW
    x$PSLthisplayer = row$PSL
    
    x$ratingdiff = row$Winner_rating - row$Loser_rating
    x$ratingClaydiff = row$Winner_ratingClay - row$Loser_ratingClay
    x$ratingHarddiff = row$Winner_ratingHard - row$Loser_ratingHard
    x$ratingGrassdiff = row$Winner_ratingGrass - row$Loser_ratingGrass
    x$ratingNotHarddiff = row$Winner_ratingNotHard - row$Loser_ratingNotHard
    x$ratingBo3diff = row$Winner_ratingBo3 - row$Loser_ratingBo3
    x$ratingBo5diff = row$Winner_ratingBo5 - row$Loser_ratingBo5
  
    
    x$RetiredDiff = row$Winner_retired_last_game - row$Loser_retired_last_game
    x$WalkoverDiff = row$Winner_walkover_last_game - row$Loser_walkover_last_game
    x$RetiredOrWalkoverDiff = x$RetiredDiff + x$WalkoverDiff
    x$FatigueDiff = row$Winner_fatigue - row$Loser_fatigue
    x$HeadtoHead = row$HeadtoHead
    if(row$HeadtoHeadMatches != 0) {
      x$HeadtoHeadPercentageWeightedsqN = ((row$HeadtoHead + 0.5 * row$HeadtoHeadMatches) / (row$HeadtoHeadMatches) - 0.5) * row$HeadtoHeadMatches ^ 0.5   
    } else {
      x$HeadtoHeadPercentageWeightedsqN = 0  
    }
    x$LastHeadtoHead = row$LastHeadtoHead
    x$HomeDiff = row$WinnerisHome - row$LoserisHome
    
    if(row$Best.of == 3){
      x$ThisBoxSkillDiff = row$Winner_skillBo3 - row$Loser_skillBo3
      x$ThisBoxSkillDiffPlusScores = row$Winner_skillBo3PlusScores - row$Loser_skillBo3PlusScores
      
      x$ThisBoxSkillRatingMethod = (row$Winner_ratingBo5 - row$Winner_ratingBo3) - 
        (row$Loser_ratingBo5 - row$Loser_ratingBo3)
          } else {
      x$ThisBoxSkillDiff = row$Winner_skillBo5 - row$Loser_skillBo5
      x$ThisBoxSkillDiffPlusScores = row$Winner_skillBo5PlusScores - row$Loser_skillBo5PlusScores
      
      x$ThisBoxSkillRatingMethod = (row$Winner_ratingBo3 - row$Winner_ratingBo5) - 
        (row$Loser_ratingBo3 - row$Loser_ratingBo5)
    }
    
    x$recentGamesDiff = row$Winner_recentGames - row$Loser_recentGames
    
    #loser's viewpoint
  } else{
    x$PSWthisplayer = row$PSL
    x$PSLthisplayer = row$PSW
    x$ratingdiff = -(row$Winner_rating - row$Loser_rating)
    x$ratingClaydiff = -(row$Winner_ratingClay - row$Loser_ratingClay)
    x$ratingHarddiff = -(row$Winner_ratingHard - row$Loser_ratingHard)
    x$ratingGrassdiff = -(row$Winner_ratingGrass - row$Loser_ratingGrass)
    x$ratingNotHarddiff = -(row$Winner_ratingNotHard - row$Loser_ratingNotHard)
    x$ratingBo3diff = -(row$Winner_ratingBo3 - row$Loser_ratingBo3)
    x$ratingBo5diff = -(row$Winner_ratingBo5 - row$Loser_ratingBo5)
    
    x$RetiredDiff = row$Loser_retired_last_game - row$Winner_retired_last_game
    x$WalkoverDiff = row$Loser_walkover_last_game - row$Winner_walkover_last_game 
    x$RetiredOrWalkoverDiff = x$RetiredDiff + x$WalkoverDiff
    x$FatigueDiff = row$Loser_fatigue - row$Winner_fatigue
    x$HeadtoHead = -row$HeadtoHead
    if(row$HeadtoHeadMatches != 0) {
      x$HeadtoHeadPercentageWeightedsqN = -(((row$HeadtoHead + 0.5 * row$HeadtoHeadMatches) / (row$HeadtoHeadMatches) - 0.5) * row$HeadtoHeadMatches ^ 0.5)   
    } else {
      x$HeadtoHeadPercentageWeightedsqN = 0  
    }
    x$LastHeadtoHead = -row$LastHeadtoHead
    x$HomeDiff = row$LoserisHome -  row$WinnerisHome 
    
    if(row$Best.of == 3){
      x$ThisBoxSkillDiff =  row$Loser_skillBo3 - row$Winner_skillBo3
      x$ThisBoxSkillDiffPlusScores = row$Loser_skillBo3PlusScores - row$Winner_skillBo3PlusScores
      
      x$ThisBoxSkillRatingMethod = -((row$Winner_ratingBo5 - row$Winner_ratingBo3) - 
        (row$Loser_ratingBo5 - row$Loser_ratingBo3))
    } else {
      x$ThisBoxSkillDiff =  row$Loser_skillBo5 - row$Winner_skillBo5 
      x$ThisBoxSkillDiffPlusScores = row$Loser_skillBo5PlusScores - row$Winner_skillBo5PlusScores 
      
      x$ThisBoxSkillRatingMethod = -((row$Winner_ratingBo3 - row$Winner_ratingBo5) - 
        (row$Loser_ratingBo3 - row$Loser_ratingBo5))
    }
    
    -(x$recentGamesDiff = row$Winner_recentGames - row$Loser_recentGames)
  }
  
  
  surface = row$Surface
  
  if(surface == "Clay") {
    x$ratingdiffCurrentSurface = x$ratingClaydiff
  } else if(surface == "Hard") {
    x$ratingdiffCurrentSurface = x$ratingHarddiff
  } else if(surface == "Grass") {
    x$ratingdiffCurrentSurface = x$ratingGrassdiff
  } 
  
  if(row$Best.of == 5){
    x$DummyBo5TimesRatingdiff = x$ratingdiff
    
    x$DummyBo5TimesAvgRatingdiff = 0.5 * (x$ratingdiff + x$ratingdiffCurrentSurface)
    
    #if(x$ratingdiff > 0) {
      x$DummyBo5 = 1
    #} else if(x$ratingdiff < 0) {
    #  x$DummyBo5 = -1
    #} else {
    #  x$DummyBo5 = 0
    #}
    
    
  } else {
    x$DummyBo5TimesRatingdiff = 0
    x$DummyBo5TimesAvgRatingdiff = 0
    x$DummyBo5 = 0
  }
  
  if(x$DummyBo5TimesRatingdiff > 0) {
    x$DummyBo5TimesRatingdiff_5SameSign = x$DummyBo5TimesRatingdiff ^ 0.5
    x$DummyBo5TimesRatingdiffSquaredSameSign = (x$DummyBo5TimesRatingdiff) ^ 2
    x$DummyBo5TimesRatingdiff1_5SameSign = x$DummyBo5TimesRatingdiff ^ 1.5
    x$DummyBo5TimesRatingdiffthirdSameSign = x$DummyBo5TimesRatingdiff ^ 3
  } else {
    x$DummyBo5TimesRatingdiffSquaredSameSign = - (x$DummyBo5TimesRatingdiff) ^ 2
    x$DummyBo5TimesRatingdiff_5SameSign = -(abs(x$DummyBo5TimesRatingdiff)) ^ 0.5
    x$DummyBo5TimesRatingdiff1_5SameSign = -(abs(x$DummyBo5TimesRatingdiff)) ^ 1.5
    x$DummyBo5TimesRatingdiffthirdSameSign = -(abs(x$DummyBo5TimesRatingdiff)) ^ 3
  }
  
  x$FatigueDiffTimesBo5 = x$DummyBo5 * x$FatigueDiff
  
  if(surface == "Clay") {
    x$DummyClay = 1
    x$DummyGrass = 0
    x$DummyHard = 0
  } else if(surface == "Grass") {
    x$DummyClay = 0
    x$DummyGrass = 1
    x$DummyHard = 0
  } else if(surface == "Hard") {
    x$DummyClay = 0
    x$DummyGrass = 0
    x$DummyHard = 1
  }
  return(x)
}

regressorvariables = function(y, Data) {

  Data$y = y
  x = adply(Data, 1, addRegressorVariableRow)
  
  x$Uncertainty = Data$Uncertainty
  x$Uncertainty2 = Data$Uncertainty2
  x$Surface = Data$Surface
  x$Court = Data$Court
  
  x$y = y
  
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
    if(!is.na(xcv$PSLthisplayer[i]) & !is.na(xcv$PSWthisplayer[i])){# & results$Br[q] > 0) {
      if(winexpectation * xcv$PSWthisplayer[i] - 1 > 0.05) {
        results$Nrbets[q] = results$Nrbets[q] + 1
        bets$bet[i] = (winexpectation * xcv$PSWthisplayer[i] - 1) / (xcv$PSWthisplayer[i] - 1)
        #WHY DOES THIS NOT WORK 
        #bets$bet[i] = 1/4 * results$Br[q] * bets$bet[i]
        #bet won
        if(ycv[i] == 1 ) {
          bets$result[i] = bets$bet[i]*(xcv$PSWthisplayer[i] - 1) 
          #bet loss
        } else {
          bets$result[i] = -bets$bet[i]
        }
      } else if(lossexpectation * xcv$PSLthisplayer[i] - 1 > 0.05) {
        results$Nrbets[q] = results$Nrbets[q] + 1
        bets$bet[i] = (lossexpectation * xcv$PSLthisplayer[i] - 1) / (xcv$PSLthisplayer[i] - 1)
        #WHY DOES THIS NOT WORK? 
        #bets$bet[i] =  1/4 * results$Br[q] * bets$bet[i]
        #bet won
        if(ycv[i] == 0){
          bets$result[i] = bets$bet[i] * (xcv$PSLthisplayer[i] - 1)
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


